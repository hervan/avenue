open Types;
open Converters;

let random_farm = () => farm_of_int(Random.int(6));

let add_yc =
  fun
  | Zero => One
  | One => Two
  | Two => Three
  | Three => Four
  | Four => Four;

let add_history = (history_item, {history} as game) => {
  ...game,
  history:
    switch (history_item) {
    | Action(_) => [history_item, ...history]
    | Message(_, _) =>
      history->List.length > 0 && history_item == (history |> List.hd)
        ? history : [history_item, ...history]
    },
};

let can_flip_farm = ({phase_deck, stage}) =>
  switch (stage) {
  | Begin
  | PhaseEnd(_) =>
    switch (phase_deck) {
    | [_, _, ..._] => true
    | _ => false
    }
  | _ => false
  };

let set_stage_phase_farm = ({phase_deck} as game) => {
  ...game,
  stage: Phase(phase_deck |> List.hd, Zero),
};

let discard_top_farm = ({phase_deck} as game) => {
  ...game,
  phase_deck: phase_deck |> List.tl,
};

let add_players_phase_points = ({players, stage} as game) =>
  switch (stage) {
  | Phase(farm, Zero) => {
      ...game,
      players:
        players
        |> List.map(player =>
             {...player, farm_points: [(farm, 0), ...player.farm_points]}
           ),
    }
  | _ => game
  };

let reset_players_lookahead = ({players} as game) => {
  ...game,
  players: players |> List.map(player => {...player, lookahead: false}),
};

let can_peek_farm = ({players, stage} as game) =>
  switch (stage) {
  | Phase(_, _) =>
    switch (players) {
    | [me, ..._] => !me.lookahead && me.round < game.round
    | _ => false
    }
  | _ => false
  };

let enable_player_lookahead = ({players} as game) => {
  ...game,
  players: [{...players |> List.hd, lookahead: true}, ...players |> List.tl],
};

let advance_player_round = ({players} as game) => {
  ...game,
  players: [
    {...players |> List.hd, round: game.round},
    ...players |> List.tl,
  ],
};

let can_flip_road = ({players, deck, stage} as game) =>
  switch (stage) {
  | Phase(_, _) =>
    switch (deck) {
    | [_, ..._] =>
      switch (players) {
      | [me, ..._] => me.round == game.round
      | _ => false
      }
    | _ => false
    }
  | _ => false
  };

let set_current_road = ({deck} as game) => {
  ...game,
  current_card: Some(deck |> List.hd),
};

let discard_top_road = ({deck} as game) => {...game, deck: deck |> List.tl};

let advance_yc_stage = ({stage, current_card} as game) => {
  ...game,
  stage:
    switch (stage) {
    | Phase(farm, yc) =>
      switch (current_card) {
      | Some((_, Yellow)) => Phase(farm, yc->add_yc)
      | _ => stage
      }
    | _ => stage
    },
};

let advance_game_round = ({round} as game) => {...game, round: round + 1};

let can_draw_road = (row, col, {players, stage, current_card} as game) =>
  switch (current_card) {
  | Some((_, _)) =>
    switch (stage) {
    | Phase(_, _) =>
      switch (players) {
      | [{round, grid}, ..._] =>
        round < game.round && grid[row][col].road == None
      | [] => false
      }
    | _ => false
    }
  | None => false
  };

let can_draw_road_somewhere = ({players} as game) =>
  switch (players) {
  | [{grid}, ..._] =>
    grid
    |> Array.to_list
    |> List.exists(grid_row =>
         grid_row
         |> Array.to_list
         |> List.exists(cell => can_draw_road(cell.row, cell.col, game))
       )
  | _ => false
  };

let draw_road_on_grid_cell = (row, col, {players, current_card} as game) =>
  switch (current_card) {
  | Some((road, _)) => {
      ...game,
      players: [
        {
          ...players |> List.hd,
          grid:
            (players |> List.hd).grid
            |> Array.mapi((i, grid_row) =>
                 i == row
                   ? grid_row
                     |> Array.mapi((j, cell) =>
                          j == col ? {...cell, road: Some(road)} : cell
                        )
                   : grid_row
               ),
        },
        ...players |> List.tl,
      ],
    }
  | None => game
  };

let update_points = ({players, farms, stage} as game) =>
  switch (stage) {
  | Phase(farm_card, _) =>
    switch (players) {
    | [] => game
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
      | [] => game
      }
    }
  | _ => game
  };

let process_phase = ({players, phase_deck, stage, history} as game) =>
  switch (players) {
  | [] => game
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

let guide_peek_farm = game =>
  game |> can_peek_farm
    ? game |> add_history(Message(Info, "or " ++ PeekFarm->describe_action))
    : game;

let guide_flip_farm = game =>
  game |> can_flip_farm
    ? game |> add_history(Message(Guide, FlipFarm->describe_action)) : game;

let guide_flip_road = game =>
  game |> can_flip_road
    ? game |> add_history(Message(Guide, FlipRoad->describe_action)) : game;

let guide_draw_road = game =>
  game |> can_draw_road_somewhere
    ? game |> add_history(Message(Guide, DrawRoad(0, 0)->describe_action))
    : game;

let guide = game =>
  game
  |> guide_peek_farm
  |> guide_flip_farm
  |> guide_flip_road
  |> guide_draw_road;