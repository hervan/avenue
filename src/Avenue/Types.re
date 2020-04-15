type side =
  | Top
  | Right
  | Bottom
  | Left;

type grape_color =
  | Purple
  | Green;

type card_color =
  | Grey
  | Yellow;

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

type cell_content =
  | Empty
  | Grapes(list(grape_color))
  | Castle(grape_color)
  | Farm(Farm.t);

type road = (side, side);

type road_orientation =
  | Forward
  | Backward;

type card = (road, card_color);

type cell = {
  row: int,
  col: int,
  content: cell_content,
  road: option(road),
};

type grid = array(array(cell));

type castles = {
  purple: cell,
  green: cell,
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
  grid,
  lookahead: bool,
  turn: int,
  farm_points: list((Farm.t, int)),
};

type game = {
  players: list(board),
  deck: list(card),
  turn: int,
  round_deck: list(Farm.t),
  stage,
  current_card: option(card),
  castles,
  farms: list(cell),
  log: list(log_entry),
  guide: list(action),
};