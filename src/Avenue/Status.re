open Types;

open Converters;

[@react.component]
let make = (~game as {guide, log}) => {
  let last_log_entry = log |> List.hd;
  let last_log_entry_lines = last_log_entry->string_of_log;
  let guide_entries =
    guide
    |> List.fold_left(
         (acc, guide_entry) =>
           List.concat([acc, guide_entry->string_of_suggestion]),
         [],
       );
  let previous_log_entries =
    log
    |> List.tl
    |> List.map(log_entry =>
         (log_entry->color_of_log, log_entry->string_of_log |> List.hd)
       );

  <g>
    <clipPath id="status-panel-clip">
      <rect width="80" height="30" />
    </clipPath>
    <g
      transform="translate(0 70)"
      fillOpacity="1"
      clipPath="url(#status-panel-clip)">
      {last_log_entry_lines
       |> List.mapi((i, log_line) =>
            <text
              key={
                i == 0
                  ? log->List.length |> string_of_int
                  : "hl"
                    ++ (last_log_entry_lines->List.length - i |> string_of_int)
              }
              style=Theme.log_text
              x="0"
              y="5"
              transform={"translate(0 " ++ (i * 3 |> string_of_int) ++ ")"}
              fill={last_log_entry->color_of_log}
              fillOpacity="1">
              log_line->str
            </text>
          )
       |> arr}
      {guide_entries
       |> List.mapi((i, guide_entry) =>
            <text
              key={"d" ++ (guide_entries->List.length - i |> string_of_int)}
              style=Theme.guide_text
              x="0"
              y="5"
              transform={
                "translate(0 "
                ++ (
                  3 * (i + last_log_entry_lines->List.length) |> string_of_int
                )
                ++ ")"
              }
              fill="white"
              fillOpacity="1">
              guide_entry->str
            </text>
          )
       |> arr}
      {previous_log_entries
       |> List.mapi((i, (entry_color, entry_line)) =>
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
                    + last_log_entry_lines->List.length
                    + guide_entries->List.length
                  )
                  |> string_of_int
                )
                ++ ")"
              }
              fill=entry_color
              fillOpacity={
                max(0., 1. /. (i + 2 |> float_of_int)) |> Js.Float.toString
              }>
              entry_line->str
            </text>
          )
       |> arr}
    </g>
  </g>;
};