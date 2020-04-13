open Types;

open Converters;

[@react.component]
let make = (~game as {guide, log}) => {
  let (guide_entries, log_entries) = (
    guide
    |> List.fold_left(
         (acc, guide_entry) =>
           List.concat([acc, guide_entry->string_of_guide]),
         [],
       ),
    log
    |> List.map(log_entry =>
         (log_entry->color_of_log, log_entry->string_of_log |> List.hd)
       ),
  );

  <g>
    <clipPath id="status-panel-clip">
      <rect width="80" height="30" />
    </clipPath>
    <g
      transform="translate(0 70)"
      fillOpacity="1"
      clipPath="url(#status-panel-clip)">
      <g style=Theme.guide_text>
        {guide_entries
         |> List.mapi((i, guide_entry) =>
              <text
                key={
                  (guide_entries->List.length + i |> string_of_int)
                  ++ (
                    i == 0
                      ? ""
                      : "d"
                        ++ (guide_entries->List.length - i |> string_of_int)
                  )
                }
                style={Theme.quick_transition("transform")}
                x="0"
                y="5"
                transform={"translate(0 " ++ (3 * i |> string_of_int) ++ ")"}
                fill="white"
                fillOpacity="1">
                guide_entry->str
              </text>
            )
         |> arr}
      </g>
      <g style=Theme.log_text>
        {log_entries
         |> List.mapi((i, (entry_color, entry_line)) =>
              <text
                key={log_entries->List.length - i |> string_of_int}
                style={Theme.quick_transition("transform")}
                x="0"
                y="5"
                transform={
                  "translate(0 "
                  ++ (
                    3 * ((guide_entries |> List.length) + i) |> string_of_int
                  )
                  ++ ")"
                }
                fill=entry_color
                fillOpacity={
                  max(0., 1. /. (i + 1 |> float_of_int)) |> Js.Float.toString
                }>
                entry_line->str
              </text>
            )
         |> arr}
      </g>
    </g>
  </g>;
};