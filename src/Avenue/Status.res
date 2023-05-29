open Common

type log_entry =
  | Action(string)
  | Event(string)

type event =
  | GameStarted
  | RoundStarted(string)
  | TurnSkipped
  | RoundIsOver(string)
  | ScoredZero(string)
  | ScoredNotEnough(int, string, int)
  | GameIsOver(int, int)

let add_action = (action, log) => list{(action, list{}), ...log}

let add_event = (event: event, x) =>
  switch x {
  | list{(last_action, events), ...previous_actions} => list{
      (last_action, list{event, ...events}),
      ...previous_actions,
    }
  | list{} => raise(Impossible("an event must only occur as a consequence of another action"))
  }

let add_suggestion = (entry, guide) => list{entry, ...guide}

let add_round_start_event = farm => add_event(RoundStarted(farm->Farm.to_string))

let add_round_over_event = farm => add_event(RoundIsOver(farm->Farm.to_string))

let suggest_play = x =>
  switch x {
  | FlipFarm => "click the left deck to begin the next round"
  | FlipRoad => "click the right deck to flip a road card"
  | PeekFarm => "or click the left deck to peek at the upcoming farm"
  | DrawRoad(_, _) => "click an empty cell to draw the face-up road"
  }

let describe_play = x =>
  switch x {
  | FlipFarm => "you flipped a farm card to begin the next round"
  | FlipRoad => "you flipped a road card"
  | PeekFarm => "you peeked at the upcoming farm"
  | DrawRoad(row, col) =>
    let r = row + 1
    let c = col + 1
    j`you drew a road in cell ($r, $c)`
  }

let suggest_control = x =>
  switch x {
  | Create => "(click here to create a new game)"
  | Restart => "(click here to restart the game)"
  | Undo => "(click here to undo the last action)"
  }

let suggest_action = x =>
  switch x {
  | Play(action) => action->suggest_play
  | Control(action) => action->suggest_control
  }

let describe_event = x =>
  switch x {
  | GameStarted => list{
      "Welcome to Avenue!",
      "The goal of the game is to draw roads connecting farms to grapes.",
      "Here is a rules summary to help you understand the game:",
      `• A game is played for 5 rounds, each scoring a single farm.`,
      `• A round begins when you flip a farm card, and it's over when 4 yellow`,
      `road cards are played (no matter how many grey cards were also played).`,
      `• In a round, you flip a road card, draw it in a (not drawn yet) square in`,
      `your map; flip another card, draw it, and so on, until the round is over.`,
      `• When the round is over you'll score for each grape in the sites where a`,
      `road going through it can reach the round's farm (also going through it).`,
      `• But pay attention, each round must score more than the previous round`,
      `AND more than zero, otherwise your round score will be a -5 point penalty.`,
      `• After playing 5 farms, you'll also score for the grapes connected to`,
      `the castles, but each castle only scores for grapes of its own color.`,
      `Learn more details by playing a couple of games (it's pretty quick!),`,
      `make sure to pay attention to the green messages, which tell you possible`,
      `actions (some can be activated by clicking the message itself), and to the`,
      `orange messages, they give more details about the last action taken and`,
      `its consequences.`,
      `So now, reveal a farm card to start the first round, and have fun!`,
    }
  | RoundStarted(farm) => list{
      j`round $farm started`,
      j`draw roads to connect grapes to farm $farm`,
    }
  | TurnSkipped => list{
      "you skip drawing a road this turn",
      "because you chose to peek at the next farm",
    }
  | RoundIsOver(farm) => list{
      j`round $farm is over`,
      j`the round ends after 4 yellow road cards are played in a round`,
    }
  | ScoredZero(farm) => list{
      "you take a -5 points penalty this round",
      j`because you don't have any grapes connected to farm $farm`,
    }
  | ScoredNotEnough(points, farm, previous) =>
    let s = points == 1 ? "" : "s"
    list{
      "you take a -5 points penalty this round",
      j`because you connected $points grape$s to farm $farm`,
      j`but last round you connected more grapes ($previous)`,
    }
  | GameIsOver(points, seed) => list{
      "the game is over!",
      "after five rounds are played, the game comes to an end",
      j`you scored $points points` ++ (points > 0 ? j`, congratulations!` : ` 🤷🏻‍♂️`),
      j`you can copy the game url (with the game identifier $seed in the end)`,
      "and send it to friends to challenge them and see who scores better!",
    }
  }

let list_of_log_entry = ((action, events)) => {
  let event_entries =
    events
    |> List.rev_map(describe_event)
    |> List.concat
    |> List.map(description => Event(description))
  switch action {
  | Some(action) => list{Action(action->describe_play), ...event_entries}
  | None => event_entries
  }
}

let short_list_of_log_entry = x =>
  switch x {
  | (Some(play), _) => list{play->describe_play}
  | (None, _) => list{}
  }

let guide_flip_farm = (avenue, guide) =>
  Avenue.Rules.can_flip_farm(avenue) ? guide |> add_suggestion(Play(FlipFarm)) : guide

let guide_peek_farm = (player, avenue, guide) =>
  Avenue.Rules.can_peek_farm(player, avenue) ? guide |> add_suggestion(Play(PeekFarm)) : guide

let guide_flip_road = (player, avenue, guide) =>
  Avenue.Rules.can_flip_road(player, avenue) ? guide |> add_suggestion(Play(FlipRoad)) : guide

let guide_draw_road = (player, avenue, guide) =>
  Avenue.Rules.can_draw_road_somewhere(player, avenue)
    ? guide |> add_suggestion(Play(DrawRoad(0, 0)))
    : guide

let guide = (player, avenue) =>
  list{}
  |> guide_flip_farm(avenue)
  |> guide_flip_road(player, avenue)
  |> guide_draw_road(player, avenue)
  |> guide_peek_farm(player, avenue)

@react.component
let make = (~guide, ~log, ~dispatch) => {
  let last_log_entry = switch log {
  | list{} => list{}
  | list{last_log, ..._} => last_log |> list_of_log_entry
  }
  let guide_entries =
    guide |> List.rev_map(guide_entry => (guide_entry, suggest_action(guide_entry)))
  let previous_log_entries = switch log {
  | list{} => list{}
  | list{_, ...previous_log} =>
    previous_log
    |> List.filter(x =>
      switch x {
      | (Some(_), _) => true
      | (None, _) => false
      }
    )
    |> List.map(short_list_of_log_entry)
    |> List.concat
  }
  let entry_text = (key, style, translateY, fill, fillOpacity, text) =>
    <text
      key={
        Js.log2("text", key)
        key
      }
      style
      x="0"
      y="5"
      transform={"translate(0 " ++ ((translateY |> Js.Float.toString) ++ ")")}
      fill
      fillOpacity={Js.Float.toString(fillOpacity)}>
      {text->str}
    </text>

  <g>
    <g transform="translate(88 25)" fillOpacity="1">
      {guide_entries
      |> List.mapi((i, (suggested_action, guide_entry)) =>
        switch suggested_action {
        | Play(DrawRoad(_, _) | PeekFarm) =>
          entry_text(
            "guide_" ++ ((log |> List.length |> string_of_int) ++ ("_" ++ i->string_of_int)),
            Theme.guide_text,
            Theme.line_height *. float_of_int(i),
            "white",
            0.5,
            guide_entry,
          )
        | Control(Create | Restart | Undo)
        | Play(FlipRoad | FlipFarm) =>
          <g
            key={
              let key =
                "guide_button_" ++
                ((log |> List.length |> string_of_int) ++
                ("_" ++ i->string_of_int))
              Js.log2("button", key)
              key
            }
            onClick={_evt => dispatch(suggested_action)}
            style=Theme.link
            transform={"translate(0 " ++
            ((float_of_int(i) *. Theme.line_height |> Js.Float.toString) ++
            ")")}>
            {entry_text(
              "guide_text_" ++ ((log |> List.length |> string_of_int) ++ ("_" ++ i->string_of_int)),
              Theme.guide_text,
              0.,
              "white",
              1.,
              guide_entry,
            )}
          </g>
        }
      )
      |> arr}
      {last_log_entry
      |> List.mapi((i, x) =>
        switch x {
        | Action(detail_line) =>
          entry_text(
            log->List.length |> string_of_int,
            Theme.log_text,
            Theme.line_height *. float_of_int(guide_entries->List.length),
            "blue",
            1.,
            (log->List.length - 1 |> string_of_int) ++ (". " ++ detail_line),
          )
        | Event(detail_line) =>
          entry_text(
            "detail_" ++
            ((log->List.length |> string_of_int) ++
            ("_" ++ (last_log_entry->List.length - i |> string_of_int))),
            Theme.log_text,
            Theme.line_height *. float_of_int(i + guide_entries->List.length),
            "orange",
            1.,
            detail_line,
          )
        }
      )
      |> arr}
      {previous_log_entries
      |> List.mapi((i, entry_line) =>
        entry_text(
          i |> string_of_int,
          Theme.log_text,
          Theme.line_height *.
          float_of_int(i + last_log_entry->List.length + guide_entries->List.length),
          "blue",
          max(0., 1. /. (i + 2 |> float_of_int)),
          (previous_log_entries->List.length - i |> string_of_int) ++ ". " ++ entry_line,
        )
      )
      |> arr}
    </g>
  </g>
}
