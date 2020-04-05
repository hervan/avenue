open Types;
open Game;
open Rules;

let flip_farm = game =>
  game->can_flip_farm
    ? game
      |> advance_stage
      |> discard_top_farm
      |> add_players_round_points
      |> reset_players_lookahead
      |> recount_points
      |> add_history(Action(FlipFarm))
    : game;

let peek_farm = game =>
  game->can_peek_farm
    ? game
      |> enable_player_lookahead
      |> advance_player_turn
      |> add_history(Action(PeekFarm))
    : game;

let flip_road = game =>
  game->can_flip_road
    ? game
      |> set_current_road
      |> discard_top_road
      |> advance_stage
      |> advance_game_turn
      |> add_history(Action(FlipRoad))
    : game;

let draw_road = (row, col, game) =>
  game |> can_draw_road(row, col)
    ? game
      |> draw_road_on_grid_cell(row, col)
      |> advance_player_turn
      |> recount_points
      |> add_history(Action(DrawRoad(row, col)))
    : game;

let end_round = game =>
  game->can_end_round ? game |> round_penalty |> advance_stage : game;

let end_game = game => game->can_end_game ? game |> advance_stage : game;

let guide = game =>
  game
  |> clear_suggestions
  |> guide_peek_farm
  |> guide_flip_farm
  |> guide_flip_road
  |> guide_draw_road;