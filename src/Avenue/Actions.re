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

let flip_road = game =>
  game->can_flip_road
    ? game
      |> set_current_road
      |> discard_top_road
      |> advance_yc_stage
      |> advance_game_round
      |> add_history(Action(FlipRoad))
    : game;

let draw_road = (row, col, game) =>
  game |> can_draw_road(row, col)
    ? game
      |> draw_road_on_grid_cell(row, col)
      |> advance_player_round
      |> add_history(Action(DrawRoad(row, col)))
    : game;