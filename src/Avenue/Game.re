open Types;
open Converters;

let rec clear_suggestions =
  fun
  | {history: [Suggestion(_), ...history]} as game =>
    {...game, history} |> clear_suggestions
  | game => game;

let add_history =
  fun
  | Suggestion(_) as history_item => (
      fun
      | {history} as game => {...game, history: [history_item, ...history]}
    )
  | history_item => (
      fun
      | game =>
        game
        |> clear_suggestions
        |> (
          fun
          | {history} as game => {
              ...game,
              history: [history_item, ...history],
            }
        )
    );

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
  | _ as game => game;

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
  | {stage: Begin, round_deck: [next_farm, ..._]} as game
  | {stage: RoundEnd(_), round_deck: [next_farm, _, ..._]} as game =>
    game |> set_stage(Round(next_farm, Zero))
  | {stage: Round(farm, Four)} as game => game |> set_stage(RoundEnd(farm))
  | {stage: Round(farm, yc), current_card: Some((_, Yellow))} as game =>
    game |> set_stage(Round(farm, yc->add_yc))
  | {stage: RoundEnd(farm), round_deck: _} as game =>
    game |> set_stage(End(farm))
  | _ as game => game;

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
      | _ => game
      }
    | _ => game
    }
  | _ => game
  };

// TODO refactor round_penalty to be modular like actions are now
let round_penalty = ({players, round_deck, stage, history} as game) =>
  switch (players) {
  | [] => game
  | [me, ...other_players] =>
    switch (stage) {
    | Round(current_farm, yc) =>
      switch (yc) {
      | Four =>
        me.turn == game.turn
          ? {
            ...game,
            players: [
              {
                ...me,
                farm_points:
                  switch (me.farm_points) {
                  | [(farm, points) as current_round, ...previous_rounds] => [
                      current_farm != farm
                        ? current_round
                        : points <= 0
                            ? (farm, (-5))
                            : (
                              switch (previous_rounds) {
                              | [(_, previous_points), ..._] =>
                                points <= previous_points
                                  ? (farm, (-5)) : current_round
                              | [] => current_round
                              }
                            ),
                      ...previous_rounds,
                    ]
                  | [] => me.farm_points
                  },
              },
              ...other_players,
            ],
            stage:
              round_deck->List.length > 1
                ? RoundEnd(current_farm) : End(current_farm),
            history: [
              Event(
                round_deck->List.length > 1
                  ? RoundIsOver(current_farm) : GameIsOver,
              ),
              ...history,
            ],
          }
          : game
      | _ => game
      }
    | Begin
    | RoundEnd(_)
    | End(_) => game
    }
  };