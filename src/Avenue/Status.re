open Types;
open Converters;

[@react.component]
  <g transform="translate(0 75)" fillOpacity="1" style=Theme.log_text>
     |> List.mapi((i, entry) =>
          <text
            key={(history |> List.length) - i - 1 |> string_of_int}
            style={Theme.quick_transition("transform")}
            x="0"
            y="0"
            transform={"translate(0 " ++ (3 * i |> string_of_int) ++ ")"}
            fill={entry->history_to_color}
            fillOpacity={
              max(0., 1. /. (i + 1 |> float_of_int)) |> Js.Float.toString
            }>
            {entry->history_to_string->str}
          </text>
        )
     |> arr}
  </g>;