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
  | Purple
  | Green;

type cell_content =
  | Empty
  | Grapes(list(grape_color))
  | Castle(grape_color)
  | Farm(farm);

let grid_contents = [|
  [|
    Grapes([Green, Green, Green, Purple]),
    Grapes([Purple]),
    Farm(A),
    Grapes([Green, Green]),
    Empty,
    Castle(Green),
  |],
  [|
    Grapes([Purple]),
    Grapes([Green, Green]),
    Grapes([Green]),
    Grapes([Purple]),
    Grapes([Green, Green, Purple]),
    Empty,
  |],
  [|
    Grapes([Green]),
    Empty,
    Grapes([Purple, Purple, Green]),
    Farm(B),
    Grapes([Green]),
    Grapes([Green, Green]),
  |],
  [|
    Farm(C),
    Grapes([Purple, Purple]),
    Grapes([Green, Green]),
    Empty,
    Grapes([Purple, Purple]),
    Farm(D),
  |],
  [|
    Grapes([Purple, Purple]),
    Empty,
    Farm(E),
    Grapes([Purple]),
    Grapes([Green, Green, Purple]),
    Grapes([Purple]),
  |],
  [|
    Empty,
    Grapes([Purple, Purple, Green]),
    Grapes([Green]),
    Grapes([Purple, Purple]),
    Grapes([Green]),
    Empty,
  |],
  [|
    Castle(Purple),
    Grapes([Green]),
    Grapes([Purple]),
    Farm(F),
    Empty,
    Grapes([Purple, Purple, Purple, Green]),
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

type action =
  | RevealPath
  | DrawPath(int, int)
  | PeekPhase
  | CountPhase
  | CountGame;

type game = {
  players: list(board),
  deck: list(card),
  stage,
  phase_deck: list(farm),
  yellow_cards: int,
  base_grid: grid,
  history: list(action),
};

let create_game = player_name => {
  base_grid,
  players: [create_player(player_name, base_grid)],
  deck: create_paths_deck(),
  stage: Begin,
  phase_deck: create_farms_deck(),
  yellow_cards: 0,
  history: [],
};

type state = {game};

let flatten_grid = grid => grid |> Array.to_list |> Array.concat;

let string_of_farm =
  fun
  | A => "A"
  | B => "B"
  | C => "C"
  | D => "D"
  | E => "E"
  | F => "F";

let string_of_grape_color =
  fun
  | Purple => "purple"
  | Green => "green";

let string_of_card_color =
  fun
  | Grey => "grey"
  | Yellow => "yellow";

module Farm = {
  [@react.component]
  let make = (~farm, ~x, ~y) => {
    let x0 = x + 5;
    let y0 = y + 9;
    let x1 = x0 + 4;
    let y1 = y0;
    let x2 = x1;
    let y2 = y1 - 3;
    let x3 = x2 - 2;
    let y3 = y2 - 1;
    let x4 = x3 - 2;
    let y4 = y3 + 1;
    <>
      <polygon
        points={j|$x0 $y0 $x1 $y1 $x2 $y2 $x3 $y3 $x4 $y4|j}
        fillOpacity="0"
      />
      <text
        x={(x0 + 1)->string_of_int}
        y={(y0 - 1)->string_of_int}
        strokeWidth="0"
        fillOpacity="0.5"
        stroke="black"
        style={ReactDOMRe.Style.make(
          ~fontSize="2.3",
          ~fontFamily="Verdana",
          (),
        )}>
        {farm->string_of_farm->str}
      </text>
    </>;
  };
};

module Grape = {
  [@react.component]
  let make = (~color, ~i, ~x, ~y) =>
    <circle
      cx={(x + (i + 1) * 2)->string_of_int}
      cy={(y + (i + 1) * 2)->string_of_int}
      r="1"
      fill={color->string_of_grape_color}
      stroke={color->string_of_grape_color}
      fillOpacity="0.5"
    />;
};

module Castle = {
  [@react.component]
  let make = (~color, ~x, ~y) => {
    let x0 = x + 2;
    let y0 = y + 8;
    let x1 = x0 + 6;
    let y1 = y0;
    let x2 = x1;
    let y2 = y1 - 4;
    let x3 = x2 - 1;
    let y3 = y2 - 2;
    let x4 = x3 - 1;
    let y4 = y3 + 2;
    let x5 = x4 - 2;
    let y5 = y4;
    let x6 = x5 - 1;
    let y6 = y5 - 2;
    let x7 = x6 - 1;
    let y7 = y6 + 2;
    <polygon
      points={j|$x0 $y0 $x1 $y1 $x2 $y2 $x3 $y3 $x4 $y4 $x5 $y5 $x6 $y6 $x7 $y7|j}
      fill={color->string_of_grape_color}
      stroke={color->string_of_grape_color}
      fillOpacity="0.5"
    />;
  };
};

module CellContent = {
  [@react.component]
  let make = (~cell) => {
    let x = cell.col * 10;
    let y = cell.row * 10;
    switch (cell.content) {
    | Empty => React.null
    | Grapes(colors) =>
      colors
      |> List.mapi((i, color) => <Grape color i x y />)
      |> Array.of_list
      |> ReasonReact.array
    | Castle(color) => <Castle color x y />
    | Farm(farm) => <Farm farm x y />
    };
  };
};

module Cell = {
  [@react.component]
  let make = (~cell, ~dispatch) => {
    let x = cell.col * 10;
    let y = cell.row * 10;
    let edge = 10;
    <g
      onClick={_evt => dispatch(DrawPath(cell.row, cell.col))}
      fillOpacity="0"
      stroke="green"
      strokeWidth="0.25">
      <rect
        x={x->string_of_int}
        y={y->string_of_int}
        width={edge->string_of_int}
        height={edge->string_of_int}
        rx="1"
      />
      <CellContent cell />
    </g>;
  };
};

[@react.component]
let make = () => {
  let _ = Random.self_init();

  let ({game}, dispatch) =
    React.useReducer(
      (state, action) =>
        switch (action) {
        | RevealPath => {
            game: {
              ...state.game,
              yellow_cards: 0,
            },
          }
        | DrawPath(row, col) =>
          Js.log2(row, col);
          {
            game: {
              ...state.game,
              yellow_cards: 0,
            },
          };
        | PeekPhase => {
            game: {
              ...state.game,
              yellow_cards: 0,
            },
          }
        | CountPhase => {
            game: {
              ...state.game,
              yellow_cards: 0,
            },
          }
        | CountGame => {
            game: {
              ...state.game,
              yellow_cards: 0,
            },
          }
        },
      {game: create_game("hervan")},
    );

  <svg width="100vmin" height="100vmin" viewBox="-5 -5 105 105">
    <title> "avenue"->str </title>
    {(game.players |> List.hd).grid
     |> flatten_grid
     |> Array.map(cell => <Cell cell dispatch />)
     |> ReasonReact.array}
  </svg>;
};