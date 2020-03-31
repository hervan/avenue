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
  history:
    switch (history_item) {
    | Action(_) => [history_item, ...game.history]
    | Message(_, _) =>
      history_item == (game.history |> List.hd)
        ? game.history : [history_item, ...game.history]
    },
};

let flip_farm = ({players, stage, phase_deck, history} as game) =>
  switch (stage) {
  | End(_) => game |> add_history(Message(Mistake, "the game is over"))
  | Phase(_, _) => game
  | Begin
  | PhaseEnd(_) =>
    switch (phase_deck) {
    | [farm, next_farm, ...rest_phase_deck] => {
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
        phase_deck: [next_farm, ...rest_phase_deck],
        stage: Phase(farm, Zero),
        history: [Action(FlipFarm), ...history],
      }
    | [_]
    | [] =>
      game
      |> add_history(
           Message(Impossible, "this should be an impossible state"),
         )
    }
  };

let peek_farm = ({players, stage, history} as game) =>
  switch (stage) {
  | End(_) => game |> add_history(Message(Mistake, "the game is over"))
  | Begin
  | PhaseEnd(_) => game
  | Phase(_, _) =>
    switch (players) {
    | [] =>
      game
      |> add_history(
           Message(Impossible, "this should be an impossible state"),
         )
    | [me, ...rest_players] =>
      switch (me) {
      | {lookahead: false} =>
        me.round < game.round
          ? {
            ...game,
            players: [
              {...me, lookahead: true, round: game.round},
              ...rest_players,
            ],
            history: [Action(FlipFarm), ...history],
          }
          : game
            |> add_history(Message(Mistake, "you already played this round"))
      | {lookahead: true} => game
      }
    }
  };

let reveal_stretch = ({players, deck, stage, history} as game) =>
  switch (stage) {
  | End(_) => game |> add_history(Message(Mistake, "the game is over"))
  | Begin
  | PhaseEnd(_) =>
    game
    |> add_history(
         Message(
           Mistake,
           "you need first to flip a farm card to begin the next phase",
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
          history: [Action(FlipStretchCard), ...history],
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
      game
      |> add_history(
           Message(Impossible, "this should be an impossible state"),
         )
    }
  };

let draw_stretch = ({players, stage, current_card} as game, row, col) =>
  switch (current_card) {
  | None =>
    game
    |> add_history(
         Message(
           Mistake,
           "you need to reveal a stretch card to start playing the phase",
         ),
       )
  | Some((stretch, _)) =>
    switch (stage) {
    | Phase(_, _) =>
      switch (players) {
      | [] =>
        game
        |> add_history(
             Message(Impossible, "this should be an impossible state"),
           )
      | [{round, grid} as me, ...other_players] =>
        round < game.round
          ? switch (grid[row][col].stretch) {
            | Some(_) =>
              game
              |> add_history(
                   Message(
                     Mistake,
                     "you can't draw a stretch in a cell that has already been drawn",
                   ),
                 )
            | None => {
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
                                    j == col
                                      ? {...cell, stretch: Some(stretch)}
                                      : cell
                                  )
                             : grid_row
                         ),
                  },
                  ...other_players,
                ],
                history: [Action(DrawStretch(row, col)), ...game.history],
              }
            }
          : game
            |> add_history(
                 Message(
                   Mistake,
                   "you already played this round (either drawn a stretch or peeked next phase)",
                 ),
               )
      }
    | _ =>
      game
      |> add_history(
           Message(
             Mistake,
             "in order to draw stretches, you need to start a phase by revealing a farm",
           ),
         )
    }
  };

let update_points = ({players, farms, stage} as game) =>
  switch (stage) {
  | Phase(farm_card, _) =>
    switch (players) {
    | [] =>
      game
      |> add_history(
           Message(Impossible, "this should be an impossible state"),
         )
    | [{farm_points, grid} as me, ...other_players] =>
      switch (farm_points) {
      | [(farm, _), ...previous_points] =>
        farm == farm_card
          ? {
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
          : game
      | [] =>
        game
        |> add_history(
             Message(Impossible, "this should be an impossible state"),
           )
      }
    }
  | _ => game
  };

let process_phase = ({players, phase_deck, stage, history} as game) =>
  switch (players) {
  | [] =>
    game
    |> add_history(Message(Impossible, "this should be an impossible state"))
  | [me, ...other_players] =>
    switch (stage) {
    | Phase(current_farm, yc) =>
      switch (yc) {
      | Four =>
        me.round == game.round
          ? {
            ...game,
            players: [
              {
                ...me,
                farm_points:
                  switch (me.farm_points) {
                  | [(farm, points) as current_phase, ...previous_phases] => [
                      current_farm != farm
                        ? current_phase
                        : points <= 0
                            ? (farm, (-5))
                            : (
                              switch (previous_phases) {
                              | [(_, previous_points), ..._] =>
                                points <= previous_points
                                  ? (farm, (-5)) : current_phase
                              | [] => current_phase
                              }
                            ),
                      ...previous_phases,
                    ]
                  | [] => me.farm_points
                  },
              },
              ...other_players,
            ],
            stage:
              phase_deck->List.length > 1
                ? PhaseEnd(current_farm) : End(current_farm),
            history: [
              Message(
                Info,
                phase_deck->List.length > 1
                  ? "current phase is over" : "game is over",
              ),
              ...history,
            ],
          }
          : game
      | _ => game
      }
    | Begin
    | PhaseEnd(_)
    | End(_) => game
    }
  };

let reducer = (game, action) =>
  switch (action) {
  | PeekFarm => peek_farm(game)
  | FlipFarm => flip_farm(game)->update_points
  | FlipStretchCard => reveal_stretch(game)
  | DrawStretch(row, col) =>
    draw_stretch(game, row, col)->update_points->process_phase
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
    <g>
      {(game.players |> List.hd).grid
       |> flatten_grid
       |> Array.mapi((i, cell) =>
            <Cell key={i |> string_of_int} cell dispatch />
          )
       |> ReasonReact.array}
    </g>
    <Deck deck={game.deck} current_card={game.current_card} dispatch />
    <PhaseDeck game dispatch />
    <Points game />
    <Status game />
  </svg>;
};