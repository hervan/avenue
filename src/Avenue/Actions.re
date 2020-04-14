open Types;
open Game;

let start_game = game =>
  game->Rules.can_start_game
    ? {...game, stage: Begin}
      |> add_action(Control(Start))
      |> add_event(GameStarted)
    : game;

let flip_farm = game =>
  game->Rules.can_flip_farm
    ? game
      |> add_action(Play(FlipFarm))
      |> advance_stage
      |> add_round_start_event
      |> discard_top_farm
      |> add_players_round_points
      |> reset_players_lookahead
      |> recount_points
    : game;

let peek_farm = game =>
  game->Rules.can_peek_farm
    ? game
      |> enable_player_lookahead
      |> advance_player_turn
      |> add_action(Play(PeekFarm))
      |> add_event(TurnSkipped)
    : game;

let flip_road = game =>
  game->Rules.can_flip_road
    ? game
      |> set_current_road
      |> discard_top_road
      |> advance_stage
      |> advance_game_turn
      |> add_action(Play(FlipRoad))
    : game;

let draw_road = (row, col, game) =>
  game |> Rules.can_draw_road(row, col)
    ? game
      |> draw_road_on_grid_cell(row, col)
      |> advance_player_turn
      |> recount_points
      |> add_action(Play(DrawRoad(row, col)))
    : game;

let end_round = game =>
  game->Rules.can_end_round
    ? game |> round_penalty |> add_round_over_event |> advance_stage : game;

let end_game = game =>
  game->Rules.can_end_game
    ? game |> advance_stage |> add_event(GameIsOver) : game;