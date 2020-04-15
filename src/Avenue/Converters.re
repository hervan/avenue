open Types;

let suggest_play =
  fun
  | PeekFarm => "or click the bottom deck to peek at the upcoming farm"
  | FlipFarm => "click the bottom deck to begin the next round"
  | FlipRoad => "click the top deck to flip a road card"
  | DrawRoad(_, _) => "click an empty cell to draw the face-up road";

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
  | Start => "welcome to avenue!"
  | Restart => "the game was restarted"
  | Undo => "the last action was undone";

let describe_event =
  fun
  | GameStarted => [
      "the goal of the game is to draw roads connecting",
      "farms to grapes, which will score you points.",
      {js|these tips in green ⤵ should help you through your first game|js},
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
  | ScoredNotEnough(previous, points) => {
      let s = points == 1 ? "" : "s";
      [
        "you take a -5 points penalty this round",
        {j|because you connected $points grape$s this round|j},
        {j|but last round you connected more grapes ($previous)|j},
      ];
    }
  | GameIsOver => [
      "the game is over!",
      "after five rounds are played, the game comes to an end",
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
  | (action, _) => [(action->color_of_action, action->describe_action)];

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