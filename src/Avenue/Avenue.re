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
  yellow_cards: 0,
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

let reveal_phase =
    (
      {players, phase_deck, stage, current_card, yellow_cards, history} as game,
    ) =>
  switch (players, stage, phase_deck, current_card, yellow_cards) {
  | (
      [{lookahead: false} as me, ...other_players],
      Begin | PhaseEnd,
      [farm, next_farm, ...rest_phase_deck],
      None,
      0,
    ) => {
      ...game,
      players: [
        {...me, farm_points: [(farm, 0), ...me.farm_points]},
        ...other_players,
      ],
      phase_deck: [next_farm, ...rest_phase_deck],
      stage: Phase(farm),
      yellow_cards: 0,
      history: [Action(RevealPhase), ...history],
    }
  | ([], _, _, _, _) => {
      ...game,
      history: [
        Message(
          Mistake,
          "there are no players in this game, you can't play without them",
        ),
        ...history,
      ],
    }
  | ([{lookahead: true}, ..._], _, _, _, _) => {
      ...game,
      history: [
        Message(
          Mistake,
          "you are peeking the next phase, you can't reveal the next",
        ),
        ...history,
      ],
    }
  | (_, End, _, _, _) => {
      ...game,
      history: [Message(Mistake, "the game is over"), ...history],
    }
  | (_, Phase(_), _, _, 0 | 1 | 2 | 3) => {
      ...game,
      history: [
        Message(
          Mistake,
          "the phase is over only after four yellow cards are played in the phase",
        ),
        ...history,
      ],
    }
  | (_, _, _, None, _) => {
      ...game,
      history: [
        Message(
          Mistake,
          "you need to reveal a stretch card to start the phase",
        ),
        ...history,
      ],
    }
  | (_, _, _, Some(_), _) => {
      ...game,
      history: [
        Message(Mistake, "you still need to draw the current card"),
        ...history,
      ],
    }
  };

let peek_phase = ({players, phase_deck, stage, history} as game) =>
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
      history: [Action(PeekPhase), ...history],
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
  | ([{lookahead: false} as me, ..._], _, _) when me.round == game.round => {
      ...game,
      history: [
        Message(Mistake, "you already played this round"),
        ...history,
      ],
    }
  | (_, _, _) => {
      ...game,
      history: [
        Message(
          Mistake,
          "you only can peek the next phase during an ongoing phase",
        ),
        ...history,
      ],
    }
  };

let reveal_stretch = ({players, deck, stage, yellow_cards, history} as game) =>
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
      history: [Action(RevealStretchCard), ...history],
    }
  | (_, _, Begin | PhaseEnd, _) => {
      ...game,
      history: [
        Message(
          Mistake,
          "you can only reveal a stretch card after a phase begins (revealing a phase card)",
        ),
        ...history,
      ],
    }
  | (_, _, End, _) => {
      ...game,
      history: [Message(Mistake, "the game is over"), ...history],
    }
  | ([me, ..._], _, _, _) when me.round < game.round => {
      ...game,
      history: [
        Message(
          Mistake,
          "you need to draw the current stretch card before revealing the next, or peek at the next phase card",
        ),
        ...history,
      ],
    }
  | (_, _, _, 4) => {
      ...game,
      history: [Message(Mistake, "the phase is over"), ...history],
    }
  | (_, _, _, _) => {
      ...game,
      history: [
        Message(Mistake, "you can't reveal the next stretch card now"),
        ...history,
      ],
    }
  };

let draw_stretch =
    ({players, stage, current_card, history} as game, row, col) =>
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
      history: [Action(DrawStretch(row, col)), ...history],
    }
  | (_, _, None) => {
      ...game,
      history: [
        Message(
          Mistake,
          "you need to reveal a stretch card to start playing the phase",
        ),
        ...history,
      ],
    }
  | ([me, ..._], _, _) when me.round == game.round => {
      ...game,
      history: [
        Message(
          Mistake,
          "you already played this round (drawn a stretch or peeked next phase)",
        ),
        ...history,
      ],
    }
  | ([{grid}, ..._], _, _) when grid[row][col].stretch != None => {
      ...game,
      history: [
        Message(
          Mistake,
          "you can't draw a stretch in a cell that has already been drawn",
        ),
        ...history,
      ],
    }
  | (_, _, _) => {
      ...game,
      history: [Message(Mistake, "you can't draw a stretch"), ...history],
    }
  };

let update_points = ({players, farms} as game) =>
  switch (players) {
  | [
      {farm_points: [(farm, _), ...previous_points], grid} as me,
      ...other_players,
    ] => {
      ...game,
      players: [
        {
          ...me,
          farm_points: [
            (
              farm,
              Summary.count_points(
                farms
                |> List.find(cell => cell.content == Farm(farm))
                |> to_pos,
                grid,
              ),
            ),
            ...previous_points,
          ],
        },
        ...other_players,
      ],
    }
  | _ => game
  };

let process_phase =
    ({players, phase_deck, stage, yellow_cards, history} as game) =>
  switch (players, stage, yellow_cards) {
  | ([me, ...other_players], Phase(current_farm), 4)
      when me.round == game.round => {
      ...game,
      players: [
        {
          ...me,
          farm_points:
            switch (me.farm_points) {
            | [(farm, points), ...previous_phases]
                when points <= 0 && current_farm == farm => [
                (farm, (-5)),
                ...previous_phases,
              ]
            | [
                (farm, points),
                (previous_farm, previous_points),
                ...previous_phases,
              ]
                when points <= previous_points && current_farm == farm => [
                (farm, (-5)),
                (previous_farm, previous_points),
                ...previous_phases,
              ]
            | _ => me.farm_points
            },
        },
        ...other_players,
      ],
      stage: phase_deck->List.length > 1 ? PhaseEnd : End,
      current_card: None,
      yellow_cards: phase_deck->List.length > 1 ? 0 : yellow_cards,
      history: [
        Message(
          Info,
          phase_deck->List.length > 1
            ? "current phase is over" : "game is over",
        ),
        ...history,
      ],
    }
  | (_, _, _) => game
  };

let reducer = (game, action) =>
  switch (action) {
  | RevealPhase => reveal_phase(game)->update_points
  | RevealStretchCard => reveal_stretch(game)
  | DrawStretch(row, col) =>
    draw_stretch(game, row, col)->update_points->process_phase
  | PeekPhase => peek_phase(game)
  };

[@react.component]
let make = () => {
  let (game, dispatch) = React.useReducer(reducer, create_game("me"));

  let flatten_grid = grid => grid |> Array.to_list |> Array.concat;

  <svg width="100vmin" height="100vmin" viewBox="-5 -5 105 105">
    {(game.players |> List.hd).grid
     |> flatten_grid
     |> Array.mapi((i, cell) =>
          <Cell key={i |> string_of_int} cell dispatch />
        )
     |> ReasonReact.array}
    <Deck deck={game.deck} current_card={game.current_card} dispatch />
    <PhaseDeck deck={game.phase_deck} current_phase={game.stage} dispatch />
    <Summary game />
    <Status game />
  </svg>;
};