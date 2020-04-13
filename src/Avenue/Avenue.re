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

let random_farm = () => farm_of_int(Random.int(6));

let create_farm_deck = () => {
  let rec aux = deck =>
    fun
    | 0 => deck
    | n => {
        let farm_card = random_farm();
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
  Array.init(grid_contents |> Array.length, row =>
    Array.init(grid_contents[0] |> Array.length, col =>
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

let create_game = (player_name, base_grid, road_deck, farm_deck) =>
  {
    players: [create_player(player_name, base_grid)],
    deck: road_deck,
    turn: 0,
    round_deck: farm_deck,
    stage: Begin,
    current_card: None,
    castles: {
      purple: find_content(Castle(Purple), base_grid),
      green: find_content(Castle(Green), base_grid),
    },
    farms: [
      find_content(Farm(A), base_grid),
      find_content(Farm(B), base_grid),
      find_content(Farm(C), base_grid),
      find_content(Farm(D), base_grid),
      find_content(Farm(E), base_grid),
      find_content(Farm(F), base_grid),
    ],
    log: [],
    guide: [],
  }
  |> Rules.guide
  |> Game.add_suggestion(Control(Start));

let reducer = game =>
  fun
  | PeekFarm => game |> Actions.peek_farm |> Rules.guide
  | FlipFarm => game |> Actions.flip_farm |> Rules.guide
  | FlipRoad => game |> Actions.flip_road |> Rules.guide
  | DrawRoad(row, col) =>
    game
    |> Actions.draw_road(row, col)
    |> Actions.end_round
    |> Actions.end_game
    |> Rules.guide;

[@react.component]
let make = () => {
  let (game, dispatch) =
    React.useReducer(
      reducer,
      create_game(
        "me",
        create_base_grid(map_A_grid_contents),
        create_road_deck(),
        create_farm_deck(),
      ),
    );

  <svg width="98vmin" height="98vmin" viewBox="-2 -2 102 102">
    Theme.filters
    <g>
      {(game.players |> List.hd).grid
       |> Array.to_list
       |> Array.concat
       |> Array.mapi((i, cell) =>
            <Cell key={i |> string_of_int} cell dispatch />
          )
       |> ReasonReact.array}
    </g>
    <Deck game dispatch />
    <RoundDeck game dispatch />
    <Points game />
    <Status game />
  </svg>;
};