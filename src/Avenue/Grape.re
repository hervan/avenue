open Converters;

[@react.component]
let make = (~color, ~i) =>
  <circle
    cx={((i + 1) * 2)->string_of_int}
    cy={((i + 1) * 2)->string_of_int}
    r="1"
    fill={color->string_of_grape_color}
    stroke={color->string_of_grape_color}
    strokeWidth="0.25"
    fillOpacity="0.5"
  />;