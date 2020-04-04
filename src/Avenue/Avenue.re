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
  turn: 0,
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

let create_road_deck = () => {
  let rec aux = (deck, available_cards) => {
    let (road, color) = (Random.int(6), Random.int(2));
    List.length(deck) == grid_columns * grid_rows
      ? deck
      : available_cards[road][color] == 0
          ? aux(deck, available_cards)
          : {
            available_cards[road][color] = available_cards[road][color] - 1;
            aux([road_card_of_ints(road, color), ...deck], available_cards);
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
      {row, col, content: grid_contents[row][col], road: None}
    )
  );

let base_grid = create_base_grid();

let create_game = player_name =>
  {
    players: [create_player(player_name, base_grid)],
    deck: create_road_deck(),
    turn: 0,
    round_deck: create_farms_deck(),
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
    history: [Event(GameStarted)],
  }
  ->Actions.guide;

let reducer = (game, action) =>
  Actions.(
    switch (action) {
    | PeekFarm => peek_farm(game)->guide
    | FlipFarm => flip_farm(game)->count_points->guide
    | FlipRoad => flip_road(game)->guide
    | DrawRoad(row, col) =>
      draw_road(row, col, game)->count_points->end_round->end_game->guide
    }
  );

[@react.component]
let make = () => {
  let (game, dispatch) = React.useReducer(reducer, create_game("me"));

  let flatten_grid = grid => grid |> Array.to_list |> Array.concat;

  <svg width="98vmin" height="98vmin" viewBox="-2 -2 102 102">
    Theme.filters
    <g>
      {(game.players |> List.hd).grid
       |> flatten_grid
       |> Array.mapi((i, cell) =>
            <Cell key={i |> string_of_int} cell dispatch />
          )
       |> ReasonReact.array}
    </g>
    <Deck game dispatch />
    <RoundDeck game dispatch />
    <Points game />
    <Status history={game.history} />
  </svg>;
};