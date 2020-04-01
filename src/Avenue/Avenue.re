open Types;
open Converters;

let grid_columns = 6;
let grid_rows = 7;

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

let create_player = (player_name, base_grid) => {
  farmer: player_name,
  grid: base_grid,
  lookahead: false,
  round: 0,
  farm_points: [],
};

let create_farms_deck = () => {
  let rec aux = deck =>
    fun
    | 0 => deck
    | n => {
        let farm_card = Rules.random_farm();
        List.for_all(card => card != farm_card, deck)
          ? aux([farm_card, ...deck], n - 1) : aux(deck, n);
      };
  Random.self_init();
  aux([], 6);
};

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
  Random.self_init();
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
  players: [create_player(player_name, base_grid)],
  deck: create_stretches_deck(),
  round: 0,
  phase_deck: create_farms_deck(),
  stage: Begin,
  current_card: None,
  castles: {
    purple: base_grid[6][0],
    green: base_grid[0][5],
  },
  farms: [
    base_grid[0][2],
    base_grid[2][3],
    base_grid[3][0],
    base_grid[3][5],
    base_grid[4][2],
    base_grid[6][3],
  ],
  history: [],
};

let reducer = (game, action) =>
  Actions.(
    Rules.(
      switch (action) {
      | PeekFarm => peek_farm(game)
      | FlipFarm => flip_farm(game)->update_points
      | FlipStretch => flip_stretch(game)
      | DrawStretch(row, col) =>
        draw_stretch(row, col, game)->update_points->process_phase
      }
    )
  );

[@react.component]
let make = () => {
  let (game, dispatch) = React.useReducer(reducer, create_game("me"));

  let flatten_grid = grid => grid |> Array.to_list |> Array.concat;

  <svg width="100vmin" height="100vmin" viewBox="-2 -2 102 102">
    <defs>
      <filter id="shadow">
        <feDropShadow
          dx="0"
          dy="0"
          stdDeviation="0.25"
          floodColor="black"
          floodOpacity="0.5"
        />
      </filter>
      <filter id="text-shadow">
        <feDropShadow
          dx="0"
          dy="0"
          stdDeviation="0.2"
          floodColor="black"
          floodOpacity="0.2"
        />
      </filter>
    </defs>
    <g>
      {(game.players |> List.hd).grid
       |> flatten_grid
       |> Array.mapi((i, cell) =>
            <Cell key={i |> string_of_int} cell dispatch />
          )
       |> ReasonReact.array}
    </g>
    <Deck game dispatch />
    <PhaseDeck game dispatch />
    <Points game />
    <Status
      messages={
        game.history
        |> List.filter(
             fun
             | Action(_) => false
             | Message(_, _) => true,
           )
      }
    />
  </svg>;
};