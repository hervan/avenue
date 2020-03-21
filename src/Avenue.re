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
};

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