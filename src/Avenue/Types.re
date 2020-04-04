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

type action =
  | PeekFarm
  | FlipFarm
  | FlipRoad
  | DrawRoad(int, int);

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

type direction =
  | Beginning
  | Forward
  | Backward
  | From(side)
  | Nowhere;

type board = {
  farmer: string,
  grid,
  lookahead: bool,
  turn: int,
  farm_points: list((farm, int)),
};

type castles = {
  purple: cell,
  green: cell,
};

type message =
  | Impossible
  | Mistake
  | Info
  | Guide;

type history_item =
  | Action(action)
  | Message(message, string);

type game = {
  players: list(board),
  deck: list(card),
  turn: int,
  round_deck: list(farm),
  stage,
  current_card: option(card),
  castles,
  farms: list(cell),
  history: list(history_item),
};