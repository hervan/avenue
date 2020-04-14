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
  | 5 => F
  | _ => raise(Impossible("farms only exist from A to F"));

let road_of_int =
  fun
  | 0 => (Top, Left)
  | 1 => (Top, Right)
  | 2 => (Right, Bottom)
  | 3 => (Left, Bottom)
  | 4 => (Left, Right)
  | 5 => (Top, Bottom)
  | _ => raise(Impossible("there are only six possible roads"));

let card_color_of_int =
  fun
  | 0 => Yellow
  | 1 => Grey
  | _ => raise(Impossible("there are only two colors of road cards"));

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

let suggest_play =
  fun
  | PeekFarm => "or click the bottom deck to peek at the upcoming farm"
  | FlipFarm => "click the bottom deck to begin the next round"
  | FlipRoad => "click the top deck to flip a road card"
  | DrawRoad(_, _) => "click an empty cell to draw the current road";

let describe_play =
  fun
  | PeekFarm => "you peeked at the upcoming farm"
  | FlipFarm => "you flipped a farm card to begin the next round"
  | FlipRoad => "you flipped a road card"
  | DrawRoad(row, col) => {j|you drew a road in cell ($row, $col)|j};

let suggest_control =
  fun
  | Start => "click start to begin the game"
  | Restart => "click restart to start a new game"
  | Undo => "click undo to revert the last action";

let describe_control =
  fun
  | Start => "the game has started"
  | Restart => "the game was restarted"
  | Undo => "the last action was undone";

let describe_event =
  fun
  | GameStarted => [
      "welcome to avenue!",
      "the goal of the game is to draw roads connecting",
      "a farm to grapes, which will score you points",
    ]
  | RoundStarted(farm) => [
      {j|round $farm started|j},
      {j|draw roads to connect grapes to farm $farm|j},
    ]
  | TurnSkipped => [
      "you skip drawing a road this turn",
      "because you chose to peek at the next farm",
    ]
  | RoundIsOver(farm) => [
      {j|round $farm is over|j},
      {j|this was triggered because 4 yellow|j},
      {j|road cards were played this round|j},
    ]
  | ScoredZero(farm) => [
      "you take a -5 points penalty this round",
      {j|because you don't have any grapes connected to farm $farm|j},
    ]
  | ScoredNotEnough(previous, points) => [
      "you take a -5 points penalty this round",
      {j|because you connected $points grapes this round|j},
      {j|but last round you connected more grapes ($previous)|j},
    ]
  | GameIsOver => [
      "the game is over!",
      "after five rounds played the game comes to an end",
    ];

let describe_action =
  fun
  | Play(action) => action->describe_play
  | Control(action) => action->describe_control;

let suggest_action =
  fun
  | Play(action) => action->suggest_play
  | Control(action) => action->suggest_control;

let list_of_log_entry =
  fun
  | (action, events) => [
      action->describe_action,
      ...events |> List.rev |> List.map(describe_event) |> List.concat,
    ];

let color_of_action =
  fun
  | Play(_) => "blue"
  | Control(_) => "red";

let short_list_of_log_entry =
  fun
  | (action, events) =>
    [
      (action->color_of_action, action->describe_action),
      ...events
         |> List.rev
         |> List.map(describe_event)
         |> List.map(event => ("orange", event |> List.hd)),
    ]
    |> List.rev;

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