open Types;
open Rules;

let flip_farm = game =>
  game->can_flip_farm
    ? game
      |> set_stage_phase_farm
      |> discard_top_farm
      |> add_players_phase_points
      |> reset_players_lookahead
      |> add_history(Action(FlipFarm))
    : game;

let peek_farm = game =>
  game->can_peek_farm
    ? game
      |> enable_player_lookahead
      |> advance_player_round
      |> add_history(Action(PeekFarm))
    : game;

let flip_stretch = game =>
  game->can_flip_stretch
    ? game
      |> set_current_stretch
      |> discard_top_stretch
      |> advance_yc_stage
      |> advance_game_round
      |> add_history(Action(FlipStretch))
    : game;

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