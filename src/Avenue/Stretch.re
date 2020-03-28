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

  let final_stretch =
    "M "
    ++ (point_of_side(entry) |> string_of_point)
    ++ " C "
    ++ (control_point_of_pos_side(pos, entry) |> string_of_point)
    ++ " "
    ++ (control_point_of_pos_side(pos, exit) |> string_of_point)
    ++ " "
    ++ (point_of_side(exit) |> string_of_point);

  let (point, setPoint) = React.useState(() => initial_stretch);

  React.useEffect(() => {
    setPoint(_ => final_stretch);
    None;
  });

  <path
    stroke="lightslategrey"
    fill="transparent"
    d=point
    style={
      switch (pos) {
      | Some(_) => Theme.quick_transition
      | None => Theme.no_transition
      }
    }
  />;
};