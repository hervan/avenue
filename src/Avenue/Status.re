open Converters;

[@react.component]
let make = (~messages) =>
  <>
    <clipPath id="status">
      <rect x="0" y="0" width="80" height="30" />
    </clipPath>
    <g
      transform="translate(0 70)"
      fillOpacity="1"
      style=Theme.log_text
      clipPath="url(#status)">
      {messages
       |> List.mapi((i, entry) =>
            <text
              key={(messages |> List.length) - i - 1 |> string_of_int}
              style={Theme.quick_transition("transform")}
              x="0"
              y="5"
              transform={"translate(0 " ++ (3 * i |> string_of_int) ++ ")"}
              fill={entry->history_to_color}
              fillOpacity={
                max(0., 1. /. (i + 1 |> float_of_int)) |> Js.Float.toString
              }>
              {entry->history_to_friendly_string->str}
            </text>
          )
       |> arr}
    </g>
  </>;