open Common;

type side =
  | Top
  | Right
  | Bottom
  | Left;

type t = (side, side);

type orientation =
  | Forward
  | Backward;

let road_of_int =
  fun
  | 0 => (Top, Left)
  | 1 => (Top, Right)
  | 2 => (Right, Bottom)
  | 3 => (Left, Bottom)
  | 4 => (Left, Right)
  | 5 => (Top, Bottom)
  | _ => raise(Impossible("there are only six possible roads"));

let point_of_side =
  fun
  | Top => (5., 0.)
  | Right => (10., 5.)
  | Bottom => (5., 10.)
  | Left => (0., 5.);

let control_point_of_pos_side = (pos, side) =>
  (
    switch (pos) {
    | None => (0., 0.)
    | Some((row, col)) => (
        5. *. ((col |> float_of_int) /. 5. -. 0.5),
        5. *. ((row |> float_of_int) /. 6. -. 0.5),
      )
    }
  )
  |> (
    ((x, y)) =>
      switch (side) {
      | Top => (x +. 5., y +. 5.)
      | Right => (y +. 5., -. x +. 5.)
      | Bottom => (-. x +. 5., -. y +. 5.)
      | Left => (-. y +. 5., x +. 5.)
      }
  );

let string_of_point = ((x, y)) => {j|$x $y|j};

// don't know how refer to a parent module type, came up with this
type road_t = t;

module Card = {
  type color =
    | Grey
    | Yellow;

  type t = (road_t, color);

  let color_of_int =
    fun
    | 0 => Yellow
    | 1 => Grey
    | _ => raise(Impossible("there are only two colors of road cards"));

  let card_of_ints = (road, color) => (
    road_of_int(road),
    color_of_int(color),
  );

  let string_of_color =
    fun
    | Grey => "lightgrey"
    | Yellow => "yellow";
};

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