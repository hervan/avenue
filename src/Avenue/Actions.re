open Types;
open Rules;

let flip_farm = game =>
  game->can_flip_farm
    ? game
      |> set_stage_round_farm
      |> discard_top_farm
      |> add_players_round_points
      |> reset_players_lookahead
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
      |> advance_yc_stage
      |> advance_game_turn
      |> add_history(Action(FlipRoad))
    : game;

let draw_road = (row, col, game) =>
  game |> can_draw_road(row, col)
    ? game
      |> draw_road_on_grid_cell(row, col)
      |> advance_player_turn
      |> add_history(Action(DrawRoad(row, col)))
    : game;

let end_round = game => game;

let end_game = game => game;

let count_points = game => game;

let guide = game =>
  game
  |> guide_peek_farm
  |> guide_flip_farm
  |> guide_flip_road
  |> guide_draw_road;