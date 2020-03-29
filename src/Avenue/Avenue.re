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
        List.for_all(
          fun
          | Some(card) => card != farm_card
          | None => true,
          deck,
        )
          ? aux([Some(farm_card), ...deck], n - 1) : aux(deck, n);
      };
  Random.self_init();
  [None, ...aux([], 6)];
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

let add_history = (history_item, game) => {
  ...game,
  history: [history_item, ...game.history],
};

let reveal_phase = ({players, phase_deck, stage, history} as game) =>
  switch (stage, phase_deck) {
  | (Begin | PhaseEnd, [_, Some(farm), ...rest_phase_deck]) => {
      ...game,
      players:
        players
        |> List.map(player =>
             {
               ...player,
               farm_points: [(farm, 0), ...player.farm_points],
               lookahead: false,
             }
           ),
      phase_deck: [Some(farm), ...rest_phase_deck],
      stage: Phase(farm, Zero),
      history: [Action(RevealPhase), ...history],
    }
  | (_, [_]) =>
    game
    |> add_history(Message(Mistake, "the last farm card is never played"))
  | (End, _) => game |> add_history(Message(Mistake, "the game is over"))
  | (Phase(_, Zero | One | Two | Three), _) =>
    game
    |> add_history(
         Message(
           Mistake,
           "the phase is only over after four yellow cards are played in the phase",
         ),
       )
  | (Phase(_, Four), _) =>
    game
    |> add_history(
         Message(
           Mistake,
           "you need to play before advancing to the next phase",
         ),
       )
  | (_, [])
  | (_, [_, None, ..._]) =>
    game |> add_history(Message(Impossible, "this should never happen"))
  };

let peek_phase = ({players, phase_deck, stage, history} as game) => {
  switch (stage) {
  | Begin
  | PhaseEnd
  | End =>
    game
    |> add_history(
         Message(
           Mistake,
           "you can only peek the next farm when there's an already active farm",
         ),
       )
  | Phase(farm, _) =>
    switch (players) {
    | [] =>
      game |> add_history(Message(Impossible, "this should never happen"))
    | [me, ...rest_players] =>
      switch (phase_deck) {
      | [Some(current_card), Some(next_card), ...rest_cards] =>
        next_card == farm
          ? {
            ...game,
            phase_deck: [Some(farm), Some(current_card), ...rest_cards],
          }
          : current_card == farm
              ? me.lookahead
                  ? {
                    ...game,
                    phase_deck: [
                      Some(next_card),
                      Some(farm),
                      ...rest_cards,
                    ],
                  }
                  : me.round < game.round
                      ? {
                        ...game,
                        players: [
                          {...me, lookahead: true, round: game.round},
                          ...rest_players,
                        ],
                        phase_deck: [
                          Some(next_card),
                          Some(farm),
                          ...rest_cards,
                        ],
                        history: [Action(PeekPhase), ...history],
                      }
                      : game
                        |> add_history(
                             Message(
                               Mistake,
                               "you already played this round",
                             ),
                           )
              : game
                |> add_history(
                     Message(Impossible, "this should never happen"),
                   )
      | []
      | [_]
      | [_, None, ..._]
      | [None, ..._] =>
        game |> add_history(Message(Impossible, "this should never happen"))
      }
    }
  };
};

let reveal_stretch = ({players, deck, stage, history} as game) =>
  switch (stage) {
  | End => game |> add_history(Message(Mistake, "the game is over"))
  | Begin
  | PhaseEnd =>
    game
    |> add_history(
         Message(
           Mistake,
           "you need first to flip a farm card to begin the phase",
         ),
       )
  | Phase(_, Four) =>
    game |> add_history(Message(Mistake, "the phase is over"))
  | Phase(farm, yc) =>
    switch (deck, players) {
    | ([(_, color) as card, ...rest_deck], [me, ..._]) =>
      me.round == game.round
        ? {
          ...game,
          deck: rest_deck,
          round: game.round + 1,
          current_card: Some(card),
          stage: Phase(farm, color == Yellow ? add_yc(yc) : yc),
          history: [Action(RevealStretchCard), ...history],
        }
        : game
          |> add_history(
               Message(
                 Mistake,
                 "you need to draw the current stretch card before revealing the next, or peek at the next phase card",
               ),
             )
    | ([], _)
    | (_, []) =>
      game |> add_history(Message(Impossible, "this should never happen"))
    }
  };

// TODO refactor to remove guards
let draw_stretch =
    ({players, stage, current_card, history} as game, row, col) =>
  switch (players, stage, current_card) {
  | ([{grid} as me, ...other_players], Phase(_), Some((stretch, _)))
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
  | (_, _, None) =>
    game
    |> add_history(
         Message(
           Mistake,
           "you need to reveal a stretch card to start playing the phase",
         ),
       )
  | ([me, ..._], _, _) when me.round == game.round =>
    game
    |> add_history(
         Message(
           Mistake,
           "you already played this round (drawn a stretch or peeked next phase)",
         ),
       )
  | ([{grid}, ..._], _, _) when grid[row][col].stretch != None =>
    game
    |> add_history(
         Message(
           Mistake,
           "you can't draw a stretch in a cell that has already been drawn",
         ),
       )
  | (_, _, _) =>
    game |> add_history(Message(Mistake, "you can't draw a stretch"))
  };

// TODO refactor to remove guards
let update_points = ({players, farms, stage} as game) =>
  switch (players, stage) {
  | (
      [
        {farm_points: [(farm, _), ...previous_points], grid} as me,
        ...other_players,
      ],
      Phase(farm_card, _),
    )
      when farm == farm_card => {
      ...game,
      players: [
        {
          ...me,
          farm_points: [
            (
              farm,
              Points.count_points(
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

// TODO refactor to remove guards
let process_phase = ({players, phase_deck, stage, history} as game) =>
  switch (players, stage) {
  | ([me, ...other_players], Phase(current_farm, Four))
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
      history: [
        Message(
          Info,
          phase_deck->List.length > 1
            ? "current phase is over" : "game is over",
        ),
        ...history,
      ],
    }
  | (_, _) => game
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
    </defs>
    {(game.players |> List.hd).grid
     |> flatten_grid
     |> Array.mapi((i, cell) =>
          <Cell key={i |> string_of_int} cell dispatch />
        )
     |> ReasonReact.array}
    <Deck deck={game.deck} current_card={game.current_card} dispatch />
    <PhaseDeck deck={game.phase_deck} dispatch />
    <Points game />
    <Status game />
  </svg>;
};