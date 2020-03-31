open Types;
open Converters;

[@react.component]
let make = (~messages) =>
  <g transform="translate(0 75)" fillOpacity="1" style=Theme.log_text>
    {messages
     |> List.mapi((i, entry) =>
          switch (entry) {
          | Message(_, description) =>
            <text
              key={(messages |> List.length) - i - 1 |> string_of_int}
              style={Theme.quick_transition("transform")}
              x="0"
              y="0"
              transform={"translate(0 " ++ (3 * i |> string_of_int) ++ ")"}
              fill={entry->history_to_color}
              fillOpacity={
                max(0., 1. /. (i + 1 |> float_of_int)) |> Js.Float.toString
              }>
              description->str
            </text>
          | Action(_) => React.null
          }
        )
     |> arr}
  </g>;