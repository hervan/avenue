open Converters;

[@react.component]
let make = (~stretch as (entry, exit)) =>
  <path
    stroke="lightslategrey"
    fill="transparent"
    d={
      "M "
      ++ string_position_of_side(entry)
      ++ " Q "
      ++ string_control_point_of_side(entry)
      ++ " 5 5 T "
      ++ string_position_of_side(exit)
    }
  />;