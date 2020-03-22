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

let string_position_of_side =
  fun
  | Top => "5 0"
  | Left => "0 5"
  | Bottom => "5 10"
  | Right => "10 5";

let string_control_point_of_side =
  fun
  | Top => "6 4"
  | Left => "4 4"
  | Bottom => "4 6"
  | Right => "6 6";

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