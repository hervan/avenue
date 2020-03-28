open Types;
open Converters;

[@react.component]
let make = (~game as {history}) =>
  switch (history) {
  | [] => React.null
  | [last_entry, ..._] =>
    <g
      transform="translate(0 75)"
      strokeWidth="0.1"
      stroke="black"
      fillOpacity="1"
      fill={last_entry->history_to_color}
      style={Theme.text("2.4px")}>
      <text>
        {last_entry->history_to_string->str}
        <animate
          attributeName="x"
          values="100;-120"
          dur="8s"
          repeatCount="indefinite"
        />
      </text>
    </g>
  };