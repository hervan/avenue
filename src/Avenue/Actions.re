open Types;
open Converters;
open Rules;

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
            history: [Action(PeekFarm), ...history],
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