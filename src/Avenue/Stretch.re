open Converters;

[@react.component]
let make = (~stretch as (entry, exit), ~pos) => {
  let initial_stretch =
    "M "
    ++ (point_of_side(entry) |> string_of_point)
    ++ " C "
    ++ (point_of_side(entry) |> string_of_point)
    ++ " "
    ++ (point_of_side(entry) |> string_of_point)
    ++ " "
    ++ (point_of_side(entry) |> string_of_point);
  let (point, setPoint) = React.useState(() => initial_stretch);

  <path
    stroke="lightslategrey"
    fill="transparent"
    d=point
    style={
      | None => ReactDOMRe.Style.make()
    }
  />;