open Common;

type details =
  | Undoable(string)
  | Revealing(string)
  | Event(string);

let add_action = (action, log) => [(action, []), ...log];

let add_event = (event: Stage.event) =>
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
  add_event(Stage.RoundStarted(farm->Farm.to_string));

let add_round_over_event = farm =>
  add_event(Stage.RoundIsOver(farm->Farm.to_string));

let suggest_play =
  fun
  | FlipFarm => "click the bottom deck to begin the next round"
  | FlipRoad => "click the top deck to flip a road card"
  | PeekFarm => "or click the bottom deck to peek at the upcoming farm"
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
         |> List.rev
         |> List.map(Stage.describe_event)
         |> List.concat
         |> List.map(description => Event(description)),
    ];

let short_list_of_log_entry =
  fun
  | (play, _) => [play->describe_play];

let guide_flip_farm = (avenue, guide) =>
  Avenue.Rules.can_flip_farm(avenue)
    ? guide |> add_suggestion(FlipFarm) : guide;

let guide_peek_farm = (player, avenue, guide) =>
  Avenue.Rules.can_peek_farm(player, avenue)
    ? guide |> add_suggestion(PeekFarm) : guide;

let guide_flip_road = (player, avenue, guide) =>
  Avenue.Rules.can_flip_road(player, avenue)
    ? guide |> add_suggestion(FlipRoad) : guide;

let guide_draw_road = (player, avenue, guide) =>
  Avenue.Rules.can_draw_road_somewhere(player, avenue)
    ? guide |> add_suggestion(DrawRoad(0, 0)) : guide;

let guide = (player, avenue) =>
  []
  |> guide_peek_farm(player, avenue)
  |> guide_flip_farm(avenue)
  |> guide_flip_road(player, avenue)
  |> guide_draw_road(player, avenue);

[@react.component]
let make = (~guide, ~log, ~dispatch_undo) => {
  let last_log_entry =
    switch (log) {
    | [] => []
    | [last_log, ..._] => last_log |> list_of_log_entry
    };
  let guide_entries = guide |> List.map(suggest_play);
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
                onClick={_evt => dispatch_undo()}
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
                   ++ {j| [⎌ click here to undo]|j},
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