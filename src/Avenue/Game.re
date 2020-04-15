open Common;
open Types;
open Converters;

let add_action = action =>
  fun
  | {log} as game => {...game, log: [(action, []), ...log]};

let add_event = event =>
  fun
  | {log: [(last_action, events), ...previous_actions]} as game => {
      ...game,
      log: [(last_action, [event, ...events]), ...previous_actions],
    }
  | {log: []} =>
    raise(
      Impossible(
        "an event must only occur as a consequence of another action",
      ),
    );

let add_suggestion = guide_entry =>
  fun
  | {guide} as game => {...game, guide: [guide_entry, ...guide]};

let add_round_start_event =
  fun
  | {stage: Round(farm, _)} as game =>
    game |> add_event(RoundStarted(farm->Farm.string_of_farm))
  | game => game;

let add_round_over_event =
  fun
  | {stage: Round(farm, _)} as game =>
    game |> add_event(RoundIsOver(farm->Farm.string_of_farm))
  | game => game;

let discard_top_farm = ({round_deck} as game) => {
  ...game,
  round_deck: round_deck |> List.tl,
};

let add_players_round_points =
  fun
  | {stage: Round(farm, Zero), players} as game => {
      ...game,
      players:
        players
        |> List.map(player =>
             {...player, farm_points: [(farm, 0), ...player.farm_points]}
           ),
    }
  | game => game;

let reset_players_lookahead = ({players} as game) => {
  ...game,
  players: players |> List.map(player => {...player, lookahead: false}),
};

let enable_player_lookahead = ({players} as game) => {
  ...game,
  players: [{...players |> List.hd, lookahead: true}, ...players |> List.tl],
};

let advance_player_turn = ({players, turn} as game) => {
  ...game,
  players: [{...players |> List.hd, turn}, ...players |> List.tl],
};

let set_current_road = ({deck} as game) => {
  ...game,
  current_card: Some(deck |> List.hd),
};

let discard_top_road = ({deck} as game) => {...game, deck: deck |> List.tl};

let advance_game_turn = ({turn} as game) => {...game, turn: turn + 1};

let draw_road_on_grid_cell = (row, col) =>
  fun
  | {players, current_card: Some((road, _))} as game => {
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
  | {current_card: None} as game => game;

let set_stage = (stage, game) => {...game, stage};

let advance_stage =
  fun
  | {stage: Flow(Begin), round_deck: [next_farm, ..._]} as game
  | {stage: Flow(RoundEnd), round_deck: [next_farm, _, ..._]} as game =>
    game |> set_stage(Round(next_farm, Zero))
  | {stage: Round(_, Four)} as game => game |> set_stage(Flow(RoundEnd))
  | {stage: Round(farm, yc), current_card: Some((_, Yellow))} as game =>
    game |> set_stage(Round(farm, yc->add_yc))
  | {stage: Flow(RoundEnd), round_deck: _} as game =>
    game |> set_stage(Flow(End))
  | game => game;

let recount_points = ({players, farms, stage} as game) =>
  switch (stage) {
  | Round(farm_card, _) =>
    switch (players) {
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
    | [] => game
    }
  | Flow(_) => game
  };

let round_penalty =
  fun
  | {
      players: [
        {farm_points: [(farm, 0), ...previous_rounds]} as me,
        ...other_players,
      ],
      stage: Round(_, _),
    } as game =>
    {
      ...game,
      players: [
        {...me, farm_points: [(farm, (-5)), ...previous_rounds]},
        ...other_players,
      ],
    }
    |> add_event(ScoredZero(farm->Farm.string_of_farm))
  | {
      players: [
        {
          farm_points: [
            (farm, points),
            (previous_farm, previous_points),
            ...previous_rounds,
          ],
        } as me,
        ...other_players,
      ],
      stage: Round(_, _),
    } as game
      when points <= previous_points =>
    {
      ...game,
      players: [
        {
          ...me,
          farm_points: [
            (farm, (-5)),
            (previous_farm, previous_points),
            ...previous_rounds,
          ],
        },
        ...other_players,
      ],
    }
    |> add_event(ScoredNotEnough(previous_points, points))
  | game => game;