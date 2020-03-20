let str = React.string;

let grid_columns = 6;
let grid_rows = 7;

type farm =
  | A
  | B
  | C
  | D
  | E
  | F;

type stage =
  | Begin
  | Phase(farm)
  | End;

type grape_color =
  | Red
  | Green;

type cell_content =
  | Empty
  | Grapes(list(grape_color))
  | Castle(grape_color)
  | Farm(farm);

let grid_contents = [|
  [|
    Grapes([Green, Green, Green, Red]),
    Grapes([Red]),
    Farm(A),
    Grapes([Green, Green]),
    Empty,
    Castle(Green),
  |],
  [|
    Grapes([Red]),
    Grapes([Green, Green]),
    Grapes([Green]),
    Grapes([Red]),
    Grapes([Green, Green, Red]),
    Empty,
  |],
  [|
    Grapes([Green]),
    Empty,
    Grapes([Red, Red, Green]),
    Farm(B),
    Grapes([Green]),
    Grapes([Green, Green]),
  |],
  [|
    Farm(C),
    Grapes([Red, Red]),
    Grapes([Green, Green]),
    Empty,
    Grapes([Red, Red]),
    Farm(D),
  |],
  [|
    Grapes([Red, Red]),
    Empty,
    Farm(E),
    Grapes([Red]),
    Grapes([Green, Green, Red]),
    Grapes([Red]),
  |],
  [|
    Empty,
    Grapes([Red, Red, Green]),
    Grapes([Green]),
    Grapes([Red, Red]),
    Grapes([Green]),
    Empty,
  |],
  [|
    Castle(Red),
    Grapes([Green]),
    Grapes([Red]),
    Farm(F),
    Empty,
    Grapes([Red, Red, Red, Green]),
  |],
|];

type side =
  | Top
  | Right
  | Bottom
  | Left;

type stretch = (side, side);

type card_color =
  | Grey
  | Yellow;

type card = (stretch, card_color);

type cell = {
  row: int,
  col: int,
  content: cell_content,
  stretch: option(stretch),
};

type grid = array(array(cell));

type board = {
  farmer: string,
  grid,
  lookahead: bool,
  round: int,
};

type game = {
  players: list(board),
  deck: list(card),
  stage,
  phase_deck: list(farm),
  yellow_cards: int,
  base_grid: grid,
};

let create_player = (player_name, base_grid) => {
  farmer: player_name,
  grid: base_grid,
  lookahead: false,
  round: 0,
};

let farm_of_int =
  fun
  | 0 => A
  | 1 => B
  | 2 => C
  | 3 => D
  | 4 => E
  | _ => F;

let random_farm = () => farm_of_int(Random.int(6));

let create_farms_deck = () => {
  let rec aux = deck =>
    fun
    | 0 => deck
    | n => {
        let farm_card = random_farm();
        List.for_all(card => card != farm_card, deck)
          ? aux([farm_card, ...deck], n - 1) : aux(deck, n);
      };
  aux([], 6);
};

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

let create_paths_deck = () => {
  let rec aux = (deck, available_cards) => {
    let (stretch, color) = (Random.int(6), Random.int(2));
    List.length(deck) == grid_columns * grid_rows
      ? deck
      : available_cards[stretch][color] == 0
          ? aux(deck, available_cards)
          : {
            available_cards[stretch][color] =
              available_cards[stretch][color] - 1;
            aux(
              [stretch_card_of_ints(stretch, color), ...deck],
              available_cards,
            );
          };
  };
  aux(
    [],
    [|[|4, 3|], [|4, 3|], [|4, 3|], [|4, 3|], [|3, 4|], [|3, 4|]|],
  );
};

let create_base_grid = () =>
  Array.init(grid_rows, row =>
    Array.init(grid_columns, col =>
      {row, col, content: grid_contents[row][col], stretch: None}
    )
  );

let base_grid = create_base_grid();

let create_game = player_name => {
  base_grid,
  players: [create_player(player_name, base_grid)],
  deck: create_paths_deck(),
  stage: Begin,
  phase_deck: create_farms_deck(),
  yellow_cards: 0,
};

[@react.component]
let make = () => {
  let _ = Random.self_init();
  <svg width="100vmin" height="100vmin" viewBox="0 0 100 100">
    <title> "avenue"->str </title>
    <line x1="5" y1="5" x2="95" y2="95" stroke="black" strokeWidth="0.1" />
  </svg>;
};