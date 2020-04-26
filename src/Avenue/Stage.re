type flow =
  | Begin
  | RoundEnd
  | End;

type event =
  | GameStarted
  | RoundStarted(string)
  | TurnSkipped
  | RoundIsOver(string)
  | ScoredZero(string)
  | ScoredNotEnough(int, string, int)
  | GameIsOver;

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
      {j|the round ends after 4 yellow road cards are played in a round|j},
    ]
  | ScoredZero(farm) => [
      "you take a -5 points penalty this round",
      {j|because you don't have any grapes connected to farm $farm|j},
    ]
  | ScoredNotEnough(previous, farm, points) => {
      let s = points == 1 ? "" : "s";
      [
        "you take a -5 points penalty this round",
        {j|because you connected $points grape$s to farm $farm|j},
        {j|but last round you connected more grapes ($previous)|j},
      ];
    }
  | GameIsOver => [
      "the game is over!",
      "after five rounds are played, the game comes to an end",
    ];

module YellowCards = {
  type t =
    | Zero
    | One
    | Two
    | Three
    | Four;

  let to_int =
    fun
    | Zero => 0
    | One => 1
    | Two => 2
    | Three => 3
    | Four => 4;

  let add =
    fun
    | Zero => One
    | One => Two
    | Two => Three
    | Three => Four
    | Four => Four;
};

type t =
  | Round(Farm.t, YellowCards.t)
  | Flow(flow);