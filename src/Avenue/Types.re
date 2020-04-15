type yellow_cards =
  | Zero
  | One
  | Two
  | Three
  | Four;

type flow_stage =
  | Created
  | Begin
  | RoundEnd
  | End;

type stage =
  | Round(Farm.t, yellow_cards)
  | Flow(flow_stage);

type play_action =
  | PeekFarm
  | FlipFarm
  | FlipRoad
  | DrawRoad(int, int);

type control_action =
  | Start
  | Restart
  | Undo;

type action =
  | Play(play_action)
  | Control(control_action);

type castles = {
  purple: Cell.t,
  green: Cell.t,
};

type event_action =
  | GameStarted
  | RoundStarted(string)
  | TurnSkipped
  | RoundIsOver(string)
  | ScoredZero(string)
  | ScoredNotEnough(int, int)
  | GameIsOver;

type log_entry = (action, list(event_action));

type board = {
  farmer: string,
  grid: Cell.grid,
  lookahead: bool,
  turn: int,
  farm_points: list((Farm.t, int)),
};

type game = {
  players: list(board),
  deck: list(Road.Card.t),
  turn: int,
  round_deck: list(Farm.t),
  stage,
  current_card: option(Road.Card.t),
  castles,
  farms: list(Cell.t),
  log: list(log_entry),
  guide: list(action),
};