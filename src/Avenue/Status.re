open Common;

type details =
  | Undoable(string)
  | Revealing(string)
  | Event(string);

type event =
  | GameStarted
  | RoundStarted(string)
  | TurnSkipped
  | RoundIsOver(string)
  | ScoredZero(string)
  | ScoredNotEnough(int, string, int)
  | GameIsOver;

let add_action = (action, log) => [(action, []), ...log];

let add_event = (event: event) =>
  fun
  | [(last_action, events), ...previous_actions] => [
      (last_action, [event, ...events]),
      ...previous_actions,
    ]
  | [] =>
    raise(
      Impossible(
        "an event must only occur as a consequence of another action",
      ),
    );

let add_suggestion = (entry, guide) => [entry, ...guide];

let add_round_start_event = farm =>
  add_event(RoundStarted(farm->Farm.to_string));

let add_round_over_event = farm =>
  add_event(RoundIsOver(farm->Farm.to_string));

let suggest_play =
  fun
  | FlipFarm => "click the left deck to begin the next round"
  | FlipRoad => "click the right deck to flip a road card"
  | PeekFarm => "or click the left deck to peek at the upcoming farm"
  | DrawRoad(_, _) => "click an empty cell to draw the face-up road";

let describe_play =
  fun
  | FlipFarm => "you flipped a farm card to begin the next round"
  | FlipRoad => "you flipped a road card"
  | PeekFarm => "you peeked at the upcoming farm"
  | DrawRoad(row, col) => {
      let r = row + 1;
      let c = col + 1;
      {j|you drew a road in cell ($r, $c)|j};
    };

let suggest_control =
  fun
  | Create => "click here to create a new game"
  | Start => "click here to begin the game"
  | Restart => "click here to restart this game"
  | Undo => "click here to revert the last action";

let suggest_action =
  fun
  | Play(action) => action->suggest_play
  | Control(action) => action->suggest_control;

let describe_event =
  fun
  | GameStarted => [
      "the goal of the game is to draw roads connecting",
      "farms to grapes, which will score you points.",
      {js|these tips in green ⤵ should help you through your first game|js},
    ]
  | RoundStarted(farm) => [
      {j|round $farm started|j},
      {j|draw roads to connect grapes to farm $farm|j},
    ]
  | TurnSkipped => [
      "you skip drawing a road this turn",
      "because you chose to peek at the next farm",
    ]
  | RoundIsOver(farm) => [
      {j|round $farm is over|j},
      {j|the round ends after 4 yellow road cards are played in a round|j},
    ]
  | ScoredZero(farm) => [
      "you take a -5 points penalty this round",
      {j|because you don't have any grapes connected to farm $farm|j},
    ]
  | ScoredNotEnough(previous, farm, points) => {
      let s = points == 1 ? "" : "s";
      [
        "you take a -5 points penalty this round",
        {j|because you connected $points grape$s to farm $farm|j},
        {j|but last round you connected more grapes ($previous)|j},
      ];
    }
  | GameIsOver => [
      "the game is over!",
      "after five rounds are played, the game comes to an end",
    ];

let list_of_log_entry =
  fun
  | (action, events) => [
      switch (action) {
      | DrawRoad(_, _) => Undoable(action->describe_play)
      | PeekFarm
      | FlipFarm
      | FlipRoad => Revealing(action->describe_play)
      },
      ...events
         |> List.rev_map(describe_event)
         |> List.concat
         |> List.map(description => Event(description)),
    ];

let short_list_of_log_entry =
  fun
  | (play, _) => [play->describe_play];

let can_start =
  fun
  | {Avenue.stage: Flow(Created)} => true
  | {stage: Flow(Begin | RoundEnd | End)}
  | {stage: Round(_, _)} => false;

let guide_start = (game, guide) =>
  can_start(game) ? guide |> add_suggestion(Control(Start)) : guide;

let guide_flip_farm = (avenue, guide) =>
  Avenue.Rules.can_flip_farm(avenue)
    ? guide |> add_suggestion(Play(FlipFarm)) : guide;

let guide_peek_farm = (player, avenue, guide) =>
  Avenue.Rules.can_peek_farm(player, avenue)
    ? guide |> add_suggestion(Play(PeekFarm)) : guide;

let guide_flip_road = (player, avenue, guide) =>
  Avenue.Rules.can_flip_road(player, avenue)
    ? guide |> add_suggestion(Play(FlipRoad)) : guide;

let guide_draw_road = (player, avenue, guide) =>
  Avenue.Rules.can_draw_road_somewhere(player, avenue)
    ? guide |> add_suggestion(Play(DrawRoad(0, 0))) : guide;

let guide = (player, avenue) =>
  []
  |> guide_start(avenue)
  |> guide_peek_farm(player, avenue)
  |> guide_flip_farm(avenue)
  |> guide_flip_road(player, avenue)
  |> guide_draw_road(player, avenue);

[@react.component]
let make = (~guide, ~log, ~dispatch) => {
  let last_log_entry =
    switch (log) {
    | [] => []
    | [last_log, ..._] => last_log |> list_of_log_entry
    };
  let guide_entries = guide |> List.map(suggest_action);
  let previous_log_entries =
    switch (log) {
    | [] => []
    | [_, ...previous_log] =>
      previous_log |> List.map(short_list_of_log_entry) |> List.concat
    };
  let entry_text = (key, style, translateY, fill, fillOpacity, text) =>
    <text
      key
      style
      x="0"
      y="5"
      transform={"translate(0 " ++ (translateY |> string_of_int) ++ ")"}
      fill
      fillOpacity={Js.Float.toString(fillOpacity)}>
      text->str
    </text>;

  <g>
    <g transform="translate(88 25)" fillOpacity="1">
      {guide_entries
       |> List.mapi((i, guide_entry) =>
            entry_text(
              "d" ++ (guide_entries->List.length - i |> string_of_int),
              Theme.guide_text,
              i * 3,
              "white",
              1.,
              guide_entry,
            )
          )
       |> arr}
      {last_log_entry
       |> List.mapi(i =>
            fun
            | Undoable(detail_line) =>
              <g
                key={log->List.length |> string_of_int}
                onClick={_evt => dispatch(Control(Undo))}
                style=Theme.link>
                {entry_text(
                   log->List.length |> string_of_int,
                   Theme.log_text,
                   3 * guide_entries->List.length,
                   "blue",
                   1.,
                   (log->List.length |> string_of_int)
                   ++ ". "
                   ++ detail_line
                   ++ {js| [⎌ click here to undo]|js},
                 )}
              </g>
            | Revealing(detail_line) =>
              entry_text(
                log->List.length |> string_of_int,
                Theme.log_text,
                3 * guide_entries->List.length,
                "blue",
                1.,
                (log->List.length |> string_of_int) ++ ". " ++ detail_line,
              )
            | Event(detail_line) =>
              entry_text(
                "hl" ++ (last_log_entry->List.length - i |> string_of_int),
                Theme.log_text,
                3 * (i + guide_entries->List.length),
                "orange",
                1.,
                detail_line,
              )
          )
       |> arr}
      {previous_log_entries
       |> List.mapi((i, entry_line) =>
            entry_text(
              previous_log_entries->List.length - i |> string_of_int,
              Theme.log_text,
              3
              * (i + last_log_entry->List.length + guide_entries->List.length),
              "blue",
              max(0., 1. /. (i + 2 |> float_of_int)),
              (
                (previous_log_entries->List.length - i |> string_of_int) ++ ". "
              )
              ++ entry_line,
            )
          )
       |> arr}
    </g>
  </g>;
};