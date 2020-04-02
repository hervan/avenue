open Types;

let str = React.string;

let arr = list => list |> Array.of_list |> ReasonReact.array;

let string_of_farm =
  fun
  | A => "A"
  | B => "B"
  | C => "C"
  | D => "D"
  | E => "E"
  | F => "F";

let farm_of_int =
  fun
  | 0 => A
  | 1 => B
  | 2 => C
  | 3 => D
  | 4 => E
  | _ => F;

let road_of_int =
  fun
  | 0 => (Top, Left)
  | 1 => (Top, Right)
  | 2 => (Right, Bottom)
  | 3 => (Left, Bottom)
  | 4 => (Left, Right)
  | _ => (Top, Bottom);

let card_color_of_int =
  fun
  | 0 => Yellow
  | _ => Grey;

let road_card_of_ints = (road, color) => (
  road_of_int(road),
  card_color_of_int(color),
);

let string_of_grape_color =
  fun
  | Purple => "purple"
  | Green => "green";

let string_of_card_color =
  fun
  | Grey => "lightgrey"
  | Yellow => "yellow";

let string_of_point = ((x, y)) => {j|$x $y|j};

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

let to_pos = cell => (cell.row, cell.col);

let action_to_string =
  fun
  | PeekFarm => "peek farm"
  | FlipFarm => "flip farm"
  | FlipRoad => "flip road"
  | DrawRoad(row, col) => {j|draw $row, $col|j};

let describe_action =
  fun
  | PeekFarm => "click the bottom deck to see the upcoming farm"
  | FlipFarm => "click the bottom deck to begin next phase"
  | FlipRoad => "click the top deck to flip a road card"
  | DrawRoad(_, _) => "click an empty cell to draw the road card";

let message_to_string =
  fun
  | Impossible => "error"
  | Mistake => "attention"
  | Info => "info"
  | Guide => "next:";

let history_to_friendly_string =
  fun
  | Action(action) => "you chose to " ++ action->describe_action
  | Message(Guide, description) => "you can now " ++ description
  | Message(_, description) => description;

let history_to_string =
  fun
  | Action(action) =>
    action->action_to_string ++ ": " ++ action->describe_action
  | Message(message, description) =>
    message->message_to_string ++ ": " ++ description;

let history_to_color =
  fun
  | Action(_) => "blue"
  | Message(Impossible, _) => "white"
  | Message(Mistake, _) => "red"
  | Message(Info, _) => "orange"
  | Message(Guide, _) => "green";

let int_of_yc =
  fun
  | Zero => 0
  | One => 1
  | Two => 2
  | Three => 3
  | Four => 4;