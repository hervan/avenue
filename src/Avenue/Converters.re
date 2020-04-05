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

let suggest_action =
  fun
  | PeekFarm => ["or click the bottom deck to peek at the upcoming farm"]
  | FlipFarm => ["you must click the bottom deck to begin the next round"]
  | FlipRoad => ["you must click the top deck to flip a road card"]
  | DrawRoad(_, _) => ["you can click an empty cell to draw the road card"];

let describe_action =
  fun
  | PeekFarm => ["you clicked the bottom deck to peek at the upcoming farm"]
  | FlipFarm => ["you clicked the bottom deck to begin the next round"]
  | FlipRoad => ["you clicked the top deck to flip a road card"]
  | DrawRoad(_, _) => ["you clicked an empty cell to draw the road card"];

let describe_event =
  fun
  | GameStarted => [
      "welcome to avenue!",
      "try to draw paths connecting grapes to farms",
    ]
  | RoundStarted(farm) => [
      {j|round $farm started!|j},
      {j|you must draw roads to connect grapes to farm $farm|j},
    ]
  | SkippedTurn => [
      "you used your turn to peek at the next farm,",
      "so you can't draw a road this turn",
    ]
  | RoundIsOver(farm) => ["round " ++ farm->string_of_farm ++ " is over!"]
  | ScoredZero => [
      "you don't have any grape connected to farm $farm,",
      "so this round you take a -5 points penalty",
    ]
  | ScoredNotEnough(previous, points) => [
      {j|you connected $points grapes this round,|j},
      {j|but last round you connected more grapes ($previous),|j},
      "so this round you take a -5 points penalty",
    ]
  | ScoredEnough(points) => [
      {j|you scored $points connected grapes this round!|j},
    ]
  | GameIsOver => ["five rounds played, the game is over!"]
  | ScoredCastle(color, points) => [
      {j|you scored $points $color grapes connected to the $color castle!|j},
    ]
  | ScoredTotal(points) => [
      {j|your final score is $points points! congratulations!|j},
    ];

let string_of_history =
  fun
  | Action(action) => action->describe_action
  | Suggestion(action) => action->suggest_action
  | Event(event) => event->describe_event;

let history_to_color =
  fun
  | Action(_) => "blue"
  | Suggestion(_) => "green"
  | Event(_) => "orange";

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