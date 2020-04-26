type t =
  | Purple
  | Green;

let color_of_grape =
  fun
  | Purple => "purple"
  | Green => "green";

[@react.component]
let make = (~grape, ~i) =>
  <circle
    cx={((i + 1) * 2)->string_of_int}
    cy={((i + 1) * 2)->string_of_int}
    r="1"
    fill={grape->color_of_grape}
    stroke={grape->color_of_grape}
    strokeWidth="0.25"
    fillOpacity="0.5"
  />;