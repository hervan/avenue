open Types;

let str = React.string;

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

let stretch_of_int =
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

let stretch_card_of_ints = (stretch, color) => (
  stretch_of_int(stretch),
  card_color_of_int(color),
);

let string_of_grape_color =
  fun
  | Purple => "purple"
  | Green => "green";

let string_of_card_color =
  fun
  | Grey => "lightgrey"
  | Yellow => "lightyellow";

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
  | RevealPhase => "reveal phase"
  | RevealStretchCard => "reveal stretch"
  | DrawStretch(row, col) => {j|draw $row, $col|j}
  | PeekPhase => "peek phase";

let describe_action =
  fun
  | RevealPhase => "flip farm card for the next phase, which begins a new phase"
  | RevealStretchCard => "flip new stretch card, which players must draw in a cell in their board"
  | DrawStretch(row, col) => {j|player draws current stretch card without rotation at row $row, column $col|j}
  | PeekPhase => "player skips draw to peek at next farm card";

let message_to_string =
  fun
  | Mistake => "attention"
  | Info => "info"
  | Tip => "tip";

let history_to_string =
  fun
  | Action(action) =>
    action->action_to_string ++ ": " ++ action->describe_action
  | Message(message, description) =>
    message->message_to_string ++ ": " ++ description;

let history_to_color =
  fun
  | Action(_) => "blue"
  | Message(Mistake, _) => "red"
  | Message(Info, _) => "yellow"
  | Message(Tip, _) => "green";