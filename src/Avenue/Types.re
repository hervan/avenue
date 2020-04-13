type farm =
  | A
  | B
  | C
  | D
  | E
  | F;

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

type stage =
  | Begin
  | Round(farm, yellow_cards)
  | RoundEnd(farm)
  | End(farm);

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
  | Farm(farm);

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

type log_entry =
  | Play(play_action)
  | Event(event_action);

type suggestion =
  | Play(play_action)
  | Control(control_action);

type board = {
  farmer: string,
  grid,
  lookahead: bool,
  turn: int,
  farm_points: list((farm, int)),
};

type game = {
  players: list(board),
  deck: list(card),
  turn: int,
  round_deck: list(farm),
  stage,
  current_card: option(card),
  castles,
  farms: list(cell),
  log: list(log_entry),
  guide: list(suggestion),
};

exception Impossible(string);