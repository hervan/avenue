open Converters;

[@react.component]
let make = (~road as (entry, exit), ~pos) => {
  let initial_road =
    "M "
    ++ (point_of_side(entry) |> string_of_point)
    ++ " C "
    ++ (point_of_side(entry) |> string_of_point)
    ++ " "
    ++ (point_of_side(entry) |> string_of_point)
    ++ " "
    ++ (point_of_side(entry) |> string_of_point);

  let final_road =
    "M "
    ++ (point_of_side(entry) |> string_of_point)
    ++ " C "
    ++ (control_point_of_pos_side(pos, entry) |> string_of_point)
    ++ " "
    ++ (control_point_of_pos_side(pos, exit) |> string_of_point)
    ++ " "
    ++ (point_of_side(exit) |> string_of_point);

  let (point, setPoint) = React.useState(() => initial_road);

  React.useEffect(() => {
    setPoint(_ => final_road);
    None;
  });

  <path
    stroke="lightslategrey"
    fill="transparent"
    d=point
    style={
      switch (pos) {
      | Some(_) => Theme.quick_transition("d")
      | None => Theme.no_transition
      }
    }
  />;
};