open Types;
open Converters;

[@react.component]
let make = (~game as {history}) =>
  switch (history) {
  | [] => React.null
  | [last_entry, ..._] =>
    <g
      transform="translate(0 90)"
      strokeWidth="0.5"
      stroke="black"
      fillOpacity="1"
      fill="yellow"
      style={ReactDOMRe.Style.make(
        ~fontSize="3.6",
        ~fontFamily="Verdana",
        (),
      )}>
      <text>
        {last_entry->history_to_string->str}
        <animate
          // attributeType="XML"
          attributeName="x"
          values="0;50;0;-50;0"
          dur="5s"
          repeatCount="indefinite"
        />
      </text>
    </g>
  };