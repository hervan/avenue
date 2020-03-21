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
  | EndPhase
  | End;

type action =
  | RevealPhase
  | RevealStretchCard
  | DrawStretch(int, int)
  | PeekPhase
  | EndPhase
  | EndGame;

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

let create_stretches_deck = () => {
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

type game = {
  players: list(board),
  deck: list(card),
  phase_deck: list(farm),
  stage,
  current_card: option(stretch),
  yellow_cards: int,
  phase_points: list(int),
  castle_points: (int, int),
  base_grid: grid,
  history: list(action),
};

let create_game = player_name => {
  players: [create_player(player_name, base_grid)],
  deck: create_stretches_deck(),
  phase_deck: create_farms_deck(),
  stage: Begin,
  current_card: None,
  yellow_cards: 0,
  phase_points: [],
  castle_points: (0, 0),
  base_grid,
  history: [],
};

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
  | Grey => "lightgrey"
  | Yellow => "lightyellow";

module Farm = {
  [@react.component]
  let make = (~farm) => {
    let x0 = 5.;
    let y0 = 9.;
    let x1 = x0 +. 4.;
    let y1 = y0;
    let x2 = x1;
    let y2 = y1 -. 3.;
    let x3 = x2 -. 2.;
    let y3 = y2 -. 1.;
    let x4 = x3 -. 2.;
    let y4 = y3 +. 1.;
    <>
      <polygon
        points={j|$x0 $y0 $x1 $y1 $x2 $y2 $x3 $y3 $x4 $y4|j}
        fillOpacity="0"
        stroke="grey"
      />
      <text
        x={(x0 +. 1.2)->Js.Float.toString}
        y={(y0 -. 0.8)->Js.Float.toString}
        strokeWidth="0"
        fillOpacity="0.5"
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
  let make = (~color, ~i) =>
    <circle
      cx={((i + 1) * 2)->string_of_int}
      cy={((i + 1) * 2)->string_of_int}
      r="1"
      fill={color->string_of_grape_color}
      stroke={color->string_of_grape_color}
      fillOpacity="0.5"
    />;
};

module Castle = {
  [@react.component]
  let make = (~color) => {
    let x0 = 2;
    let y0 = 8;
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
  let make = (~content) => {
    switch (content) {
    | Empty => React.null
    | Grapes(colors) =>
      colors
      |> List.mapi((i, color) => <Grape key={i |> string_of_int} color i />)
      |> Array.of_list
      |> ReasonReact.array
    | Castle(color) => <Castle color />
    | Farm(farm) => <Farm farm />
    };
  };
};

let string_position_of_side =
  fun
  | Top => "5 0"
  | Left => "0 5"
  | Bottom => "5 10"
  | Right => "10 5";

let string_control_point_of_side =
  fun
  | Top => "6 4"
  | Left => "4 4"
  | Bottom => "4 6"
  | Right => "6 6";

module Stretch = {
  [@react.component]
  let make = (~stretch as (entry, exit)) =>
    <path
      stroke="lightslategrey"
      fill="transparent"
      d={
        "M "
        ++ string_position_of_side(entry)
        ++ " Q "
        ++ string_control_point_of_side(entry)
        ++ " 5 5 T "
        ++ string_position_of_side(exit)
      }
    />;
};

module StretchCard = {
  [@react.component]
  let make = (~stretch) => <g strokeWidth="1"> <Stretch stretch /> </g>;
};

module StretchDraw = {
  [@react.component]
  let make = (~stretch) => <g strokeWidth="0.5"> <Stretch stretch /> </g>;
};

module Cell = {
  [@react.component]
  let make = (~cell, ~dispatch) => {
    <g
      onClick={_evt => dispatch(DrawStretch(cell.row, cell.col))}
      fillOpacity="0"
      strokeWidth="0.25"
      transform={
        "translate("
        ++ (cell.col * 10)->string_of_int
        ++ " "
        ++ (cell.row * 10)->string_of_int
        ++ ")"
      }>
      <rect width="10" height="10" stroke="green" strokeWidth="0.1" />
      <CellContent content={cell.content} />
      {switch (cell.stretch) {
       | None => React.null
       | Some(stretch) => <StretchDraw stretch />
       }}
    </g>;
  };
};

module Deck = {
  let card_thickness = 0.05;
  [@react.component]
  let make = (~deck, ~current_card, ~dispatch) => {
    <g
      onClick={_evt => dispatch(RevealStretchCard)}
      transform="translate(65 0)">
      {deck
       |> List.mapi((i, _) =>
            <rect
              key={i |> string_of_int}
              x={card_thickness *. (i |> float_of_int) |> Js.Float.toString}
              y={card_thickness *. (i |> float_of_int) |> Js.Float.toString}
              width="15"
              height="20"
              rx="2"
              fill="lightblue"
              stroke="black"
              strokeWidth="0.025"
            />
          )
       |> Array.of_list
       |> ReasonReact.array}
      {switch (current_card) {
       | None => React.null
       | Some(stretch) =>
         <>
           <rect
             x={
               card_thickness
               *. (List.length(deck) |> float_of_int)
               |> Js.Float.toString
             }
             y={
               card_thickness
               *. (List.length(deck) |> float_of_int)
               |> Js.Float.toString
             }
             width="15"
             height="20"
             rx="2"
             fill="lightblue"
             stroke="black"
             strokeWidth={card_thickness |> Js.Float.toString}
           />
           <g
             transform={
               "translate("
               ++ (
                 2.5
                 +. 0.15
                 *. (List.length(deck) |> float_of_int)
                 |> Js.Float.toString
               )
               ++ " "
               ++ (
                 5.0
                 +. 0.15
                 *. (List.length(deck) |> float_of_int)
                 |> Js.Float.toString
               )
               ++ ")"
             }>
             <rect
               width="10"
               height="10"
               rx="1"
               fill="white"
               stroke="black"
               strokeWidth="0.1"
             />
             <StretchCard stretch />
           </g>
         </>
       }}
    </g>;
  };
};

module PhaseDeck = {
  let card_thickness = 0.5;
  [@react.component]
  let make = (~deck, ~current_phase, ~dispatch) => {
    <g onClick={_evt => dispatch(RevealPhase)} transform="translate(65 25)">
      {deck
       |> List.mapi((i, _) =>
            <rect
              key={i |> string_of_int}
              x={card_thickness *. (i |> float_of_int) |> Js.Float.toString}
              y={card_thickness *. (i |> float_of_int) |> Js.Float.toString}
              width="15"
              height="20"
              rx="2"
              fill="cornflowerblue"
              stroke="black"
              strokeWidth="0.025"
            />
          )
       |> Array.of_list
       |> ReasonReact.array}
      {switch (current_phase) {
       | Phase(farm) =>
         <>
           <rect
             x={
               card_thickness
               *. (List.length(deck) |> float_of_int)
               |> Js.Float.toString
             }
             y={
               card_thickness
               *. (List.length(deck) |> float_of_int)
               |> Js.Float.toString
             }
             width="15"
             height="20"
             rx="2"
             fill="black"
             stroke="black"
             strokeWidth={card_thickness |> Js.Float.toString}
           />
           <g
             transform={
               "translate("
               ++ (
                 2.5
                 +. 0.15
                 *. (List.length(deck) |> float_of_int)
                 |> Js.Float.toString
               )
               ++ " "
               ++ (
                 5.0
                 +. 0.15
                 *. (List.length(deck) |> float_of_int)
                 |> Js.Float.toString
               )
               ++ ")"
             }>
             <rect
               width="10"
               height="10"
               rx="1"
               fill="white"
               stroke="black"
               strokeWidth="0.1"
             />
             <Farm farm />
           </g>
         </>
       | _ => React.null
       }}
    </g>;
  };
};

let reveal_phase = game =>
  switch (game.phase_deck) {
  | []
  | [_] => {...game, stage: End}
  | [farm, ...rest_phase_deck] => {
      ...game,
      phase_deck: rest_phase_deck,
      stage: Phase(farm),
      current_card: None,
      yellow_cards: 0,
    }
  };

let reveal_stretch = game =>
  switch (game.deck) {
  | [] => {...game, stage: End}
  | [(stretch, color), ...rest_deck] => {
      ...game,
      deck: rest_deck,
      current_card: Some(stretch),
      yellow_cards:
        color == Yellow ? game.yellow_cards + 1 : game.yellow_cards,
    }
  };

let draw_stretch = (game, row, col) => {
  ...game,
  players:
    switch (game.players) {
    | [me, ...rest_players] => [
        {
          ...me,
          round: me.round + 1,
          grid:
            me.grid
            |> Array.mapi((i, grid_row) =>
                 i == row
                   ? grid_row
                     |> Array.mapi((j, cell) =>
                          j == col
                            ? {...cell, stretch: game.current_card} : cell
                        )
                   : grid_row
               ),
        },
        ...rest_players,
      ]
    | [] => []
    },
};

// type board = {
//   farmer: string,
//   grid,
//   lookahead: bool,
//   round: int,
// };

// players: list(board),
// deck: list(card),
// phase_deck: list(farm),
// stage,
// current_card: option(card),
// yellow_cards: int,
// phase_points: list(int),
// castle_points: (int, int),
// base_grid: grid,
// history: list(action),

let reducer = (game, action) =>
  switch (action) {
  | RevealPhase => reveal_phase(game)
  | RevealStretchCard => reveal_stretch(game)
  | DrawStretch(row, col) => draw_stretch(game, row, col)
  | PeekPhase =>
    Js.log("peek");
    {...game, yellow_cards: 0};
  | EndPhase =>
    Js.log("end phase");
    {...game, yellow_cards: 0};
  | EndGame =>
    Js.log("end game");
    {...game, yellow_cards: 0};
  };

[@react.component]
let make = () => {
  let _ = Random.self_init();

  let (game, dispatch) = React.useReducer(reducer, create_game("me"));

  <svg width="100vmin" height="100vmin" viewBox="-5 -5 105 105">
    <title> "avenue"->str </title>
    {(game.players |> List.hd).grid
     |> flatten_grid
     |> Array.mapi((i, cell) =>
          <Cell key={i |> string_of_int} cell dispatch />
        )
     |> ReasonReact.array}
    <Deck deck={game.deck} current_card={game.current_card} dispatch />
    <PhaseDeck deck={game.phase_deck} current_phase={game.stage} dispatch />
  </svg>;
};