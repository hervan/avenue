open Types;
open Game;
open Converters;

let random_farm = () => farm_of_int(Random.int(6));

let can_flip_farm = ({round_deck, stage}) =>
  switch (stage) {
  | Begin
  | RoundEnd(_) =>
    switch (round_deck) {
    | [_, _, ..._] => true
    | _ => false
    }
  | _ => false
  };

let can_peek_farm = ({players, stage} as game) =>
  switch (stage) {
  | Round(_, Four) => false
  | Round(_, _) =>
    switch (players) {
    | [me, ..._] => !me.lookahead && me.turn < game.turn
    | _ => false
    }
  | _ => false
  };

let can_flip_road = ({players, deck, stage} as game) =>
  switch (stage) {
  | Round(_, _) =>
    switch (deck) {
    | [_, ..._] =>
      switch (players) {
      | [me, ..._] => me.turn == game.turn
      | _ => false
      }
    | _ => false
    }
  | _ => false
  };

let can_draw_road = (row, col, {players, stage, current_card} as game) =>
  switch (current_card) {
  | Some((_, _)) =>
    switch (stage) {
    | Round(_, _) =>
      switch (players) {
      | [{turn, grid}, ..._] =>
        turn < game.turn && grid[row][col].road == None
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

let can_end_round =
  fun
  | {players: [me, ..._], stage: Round(_, Four), turn} => me.turn == turn
  | _ => false;

let can_end_game =
  fun
  | {stage: RoundEnd(_), round_deck} => round_deck->List.length == 1
  | _ => false;

// TODO rework process_round to be modular like actions are now
let process_round = ({players, round_deck, stage, history} as game) =>
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

let guide_peek_farm = game =>
  game |> can_peek_farm ? game |> add_history(Action(PeekFarm)) : game;

let guide_flip_farm = game =>
  game |> can_flip_farm ? game |> add_history(Action(FlipFarm)) : game;

let guide_flip_road = game =>
  game |> can_flip_road ? game |> add_history(Action(FlipRoad)) : game;

let guide_draw_road = game =>
  game |> can_draw_road_somewhere
    ? game |> add_history(Action(DrawRoad(0, 0))) : game;