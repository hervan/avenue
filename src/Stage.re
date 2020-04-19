type yellow_cards =
  | Zero
  | One
  | Two
  | Three
  | Four;

let int_of_yc =
  fun
  | Zero => 0
  | One => 1
  | Two => 2
  | Three => 3
  | Four => 4;

let add_yc =
  fun
  | Zero => One
  | One => Two
  | Two => Three
  | Three => Four
  | Four => Four;

type flow =
  | Created
  | Begin
  | RoundEnd
  | End;

type t =
  | Round(Farm.t, yellow_cards)
  | Flow(flow);