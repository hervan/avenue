open Converters;

[@react.component]
let make = (~history) => {
  let messages =
    history
    |> List.fold_left(
         (messages, entry) =>
           List.concat([
             entry->string_of_history
             |> List.map(line => (line, entry->history_to_color)),
             messages,
           ]),
         [],
       )
    |> List.rev;
  <g>
    <clipPath id="status"> <rect width="80" height="30" /> </clipPath>
    <g
      transform="translate(0 70)"
      fillOpacity="1"
      style=Theme.log_text
      clipPath="url(#status)">
      {messages
       |> List.mapi((i, (message, color)) =>
            <text
              key={(messages |> List.length) - i - 1 |> string_of_int}
              style={Theme.quick_transition("transform")}
              x="0"
              y="5"
              transform={"translate(0 " ++ (3 * i |> string_of_int) ++ ")"}
              fill=color
              fillOpacity={
                max(0., 1. /. (i + 1 |> float_of_int)) |> Js.Float.toString
              }>
              message->str
            </text>
          )
       |> arr}
    </g>
  </g>;
};