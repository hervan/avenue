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
  round: 0,
  phase_deck: create_farms_deck(),
  stage: Begin,
  current_card: None,
  yellow_cards: 0,
  phase_points: [],
  castle_points: (0, 0),
  history: [],
};

let reveal_phase =
    ({players, phase_deck, stage, current_card, yellow_cards} as game) =>
  switch (players, stage, phase_deck, current_card, yellow_cards) {
  | (
      [{lookahead: false}, ..._],
      Begin | PhaseEnd,
      [farm, next_farm, ...rest_phase_deck],
      None,
      0,
    ) => {
      ...game,
      phase_deck: [next_farm, ...rest_phase_deck],
      stage: Phase(farm),
      yellow_cards: 0,
    }
  | (_, _, _, _, _) => game
  };

let peek_phase = ({players, phase_deck, stage} as game) =>
  switch (players, phase_deck, stage) {
  | (
      [{lookahead: false} as me, ...other_players],
      [next_farm, ...rest_phase_deck],
      Phase(farm),
    )
      when me.round < game.round => {
      ...game,
      players: [
        {...me, round: game.round, lookahead: true},
        ...other_players,
      ],
      phase_deck: [farm, ...rest_phase_deck],
      stage: Phase(next_farm),
    }
  | (
      [{lookahead: true} as me, ...other_players],
      [farm, ...rest_phase_deck],
      Phase(next_farm),
    )
      when me.round == game.round => {
      ...game,
      players: [
        {...me, round: game.round, lookahead: false},
        ...other_players,
      ],
      phase_deck: [next_farm, ...rest_phase_deck],
      stage: Phase(farm),
    }
  | (_, _, _) => game
  };

let reveal_stretch = ({players, deck, stage, yellow_cards} as game) =>
  switch (players, deck, stage, yellow_cards) {
  | (
      [{lookahead: false} as me, ..._],
      [(_, color) as card, ...rest_deck],
      Phase(_),
      0 | 1 | 2 | 3,
    )
      when me.round == game.round => {
      ...game,
      deck: rest_deck,
      round: game.round + 1,
      current_card: Some(card),
      yellow_cards: color == Yellow ? yellow_cards + 1 : yellow_cards,
    }
  | (_, _, _, _) => game
  };

let draw_stretch = ({players, stage, current_card} as game, row, col) =>
  switch (players, stage, current_card) {
  | (
      [{grid, lookahead: false} as me, ...other_players],
      Phase(_),
      Some((stretch, _)),
    )
      when me.round < game.round && grid[row][col].stretch == None => {
      ...game,
      players: [
        {
          ...me,
          round: game.round,
          grid:
            grid
            |> Array.mapi((i, grid_row) =>
                 i == row
                   ? grid_row
                     |> Array.mapi((j, cell) =>
                          j == col ? {...cell, stretch: Some(stretch)} : cell
                        )
                   : grid_row
               ),
        },
        ...other_players,
      ],
    }
  | (_, _, _) => game
  };

let reducer = (game, action) =>
  switch (action) {
  | RevealPhase => reveal_phase(game)
  | RevealStretchCard => reveal_stretch(game)
  | DrawStretch(row, col) => draw_stretch(game, row, col)
  | PeekPhase => peek_phase(game)
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

  let flatten_grid = grid => grid |> Array.to_list |> Array.concat;

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