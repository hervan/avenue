open Types;
open Converters;

let grid_columns = 6;
let grid_rows = 7;

let map_A_grid_contents = [|
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

let create_base_grid = grid_contents =>
  Array.init(grid_rows, row =>
    Array.init(grid_columns, col =>
      {row, col, content: grid_contents[row][col], road: None}
    )
  );

let find_content = (cell_content, grid) =>
  grid
  |> Array.to_list
  |> List.map(row =>
       row
       |> Array.to_list
       |> List.filter(cell => cell.content == cell_content)
     )
  |> List.concat
  |> List.hd;

let create_game = (player_name, common_grid, road_deck, farms_deck) =>
  {
    players: [create_player(player_name, common_grid)],
    deck: road_deck,
    turn: 0,
    round_deck: farms_deck,
    stage: Begin,
    current_card: None,
    castles: {
      purple: find_content(Castle(Purple), common_grid),
      green: find_content(Castle(Green), common_grid),
    },
    farms: [
      find_content(Farm(A), common_grid),
      find_content(Farm(B), common_grid),
      find_content(Farm(C), common_grid),
      find_content(Farm(D), common_grid),
      find_content(Farm(E), common_grid),
      find_content(Farm(F), common_grid),
    ],
    history: [Event(GameStarted)],
  }
  ->Actions.guide;

let reducer = (game, action) =>
  Actions.(
    switch (action) {
    | PeekFarm => game->peek_farm->guide
    | FlipFarm => game->flip_farm->guide
    | FlipRoad => game->flip_road->guide
    | DrawRoad(row, col) =>
      (game |> draw_road(row, col))->end_round->end_game->guide
    }
  );

[@react.component]
let make = () => {
  let (game, dispatch) =
    React.useReducer(
      reducer,
      create_game(
        "me",
        create_base_grid(map_A_grid_contents),
        create_road_deck(),
        create_farms_deck(),
      ),
    );

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