open Common;

type t = (Avenue.action, list(Stage.event));

let add_action = (action: Avenue.action, log: list(t)) => [
  (action, []),
  ...log,
];

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

let list_of_log_entry =
  fun
  | (action, events) => [
      action->Avenue.Rules.describe_play,
      ...events |> List.rev |> List.map(Stage.describe_event) |> List.concat,
    ];

let short_list_of_log_entry =
  fun
  | (play, _) => [play->Avenue.Rules.describe_play];

let guide_flip_farm = (avenue, guide) =>
  Avenue.Rules.can_flip_farm(avenue)
    ? guide |> add_suggestion(Avenue.FlipFarm) : guide;

let guide_peek_farm = (player, avenue, guide) =>
  Avenue.Rules.can_peek_farm(player, avenue)
    ? guide |> add_suggestion(Avenue.PeekFarm) : guide;

let guide_flip_road = (player, avenue, guide) =>
  Avenue.Rules.can_flip_road(player, avenue)
    ? guide |> add_suggestion(Avenue.FlipRoad) : guide;

let guide_draw_road = (player, avenue, guide) =>
  Avenue.Rules.can_draw_road_somewhere(player, avenue)
    ? guide |> add_suggestion(Avenue.DrawRoad(0, 0)) : guide;

let guide = (player, avenue) =>
  []
  |> guide_peek_farm(player, avenue)
  |> guide_flip_farm(avenue)
  |> guide_flip_road(player, avenue)
  |> guide_draw_road(player, avenue);

[@react.component]
let make = (~guide, ~log) => {
  let last_log_entry =
    switch (log) {
    | [] => []
    | [last_log, ..._] => last_log |> list_of_log_entry
    };
  let guide_entries = guide |> List.map(Avenue.Rules.suggest_play);
  let previous_log_entries =
    switch (log) {
    | [] => []
    | [_, ...previous_log] =>
      previous_log |> List.map(short_list_of_log_entry) |> List.concat
    };

  <g>
    <clipPath id="status-panel-clip">
      <rect width="80" height="30" />
    </clipPath>
    <g
      transform="translate(0 70)"
      fillOpacity="1"
      clipPath="url(#status-panel-clip)">
      {guide_entries
       |> List.mapi((i, guide_entry) =>
            <text
              key={"d" ++ (guide_entries->List.length - i |> string_of_int)}
              style=Theme.guide_text
              x="0"
              y="5"
              transform={"translate(0 " ++ (i * 3 |> string_of_int) ++ ")"}
              fill="white"
              fillOpacity="1">
              guide_entry->str
            </text>
          )
       |> arr}
      {last_log_entry
       |> List.mapi((i, log_line) =>
            <text
              key={
                i == 0
                  ? log->List.length |> string_of_int
                  : "hl" ++ (last_log_entry->List.length - i |> string_of_int)
              }
              style=Theme.log_text
              x="0"
              y="5"
              transform={
                "translate(0 "
                ++ (3 * (i + guide_entries->List.length) |> string_of_int)
                ++ ")"
              }
              fill={i == 0 ? "blue" : "orange"}
              fillOpacity="1">
              {i == 0 ? (log->List.length |> string_of_int) ++ ". " : ""}->str
              log_line->str
            </text>
          )
       |> arr}
      {previous_log_entries
       |> List.mapi((i, entry_line) =>
            <text
              key={previous_log_entries->List.length - i |> string_of_int}
              style=Theme.log_text
              x="0"
              y="5"
              transform={
                "translate(0 "
                ++ (
                  3
                  * (
                    i
                    + last_log_entry->List.length
                    + guide_entries->List.length
                  )
                  |> string_of_int
                )
                ++ ")"
              }
              fill="blue"
              fillOpacity={
                max(0., 1. /. (i + 2 |> float_of_int)) |> Js.Float.toString
              }>
              {(
                 (previous_log_entries->List.length - i |> string_of_int)
                 ++ ". "
               )
               ->str}
              entry_line->str
            </text>
          )
       |> arr}
    </g>
  </g>;
};