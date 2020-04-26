// TODO: PARENT MODULE SHOULD BE OF THE RETURNING TYPE
// so, either return type should be t,
// or only parameter (apart from core types) should be t

type t = {
  avenue: Avenue.t,
  me: Player.t,
  players: list(Player.t),
  log: list(Status.t),
  guide: list(Avenue.action),
};

type action =
  | Start
  | Restart
  | Undo;

let setup = (player_name, base_grid, road_deck, farm_deck) => {
  let avenue: Avenue.t = {
    turn: 0,
    road_deck,
    farm_deck,
    stage: Flow(Begin),
    current_card: None,
    castles: {
      purple: Grid.find(Castle(Purple), base_grid),
      green: Grid.find(Castle(Green), base_grid),
    },
    farms: [
      Grid.find(Farm(A), base_grid),
      Grid.find(Farm(B), base_grid),
      Grid.find(Farm(C), base_grid),
      Grid.find(Farm(D), base_grid),
      Grid.find(Farm(E), base_grid),
      Grid.find(Farm(F), base_grid),
    ],
  };
  let me = Player.setup(player_name, base_grid);
  {avenue, log: [], guide: avenue |> Status.guide(me), me, players: []};
};

let flip_farm = ({me, avenue: {farm_deck} as avenue, log} as t) =>
  avenue->Avenue.Rules.can_flip_farm
    ? {
      ...t,
      avenue: avenue |> Avenue.advance_stage |> Avenue.discard_top_farm,
      me:
        me
        |> Player.keep_round_points
        |> Player.add_round_points(farm_deck->List.nth(0))
        |> Player.reset_lookahead
        |> Player.recount_points(avenue.farms),
      log:
        log
        |> Status.add_action(Avenue.FlipFarm)
        |> Status.add_round_start_event(farm_deck |> List.hd),
    }
    : t;

let peek_farm = ({me, avenue, log} as t) =>
  avenue |> Avenue.Rules.can_peek_farm(me)
    ? {
      ...t,
      me: me |> Player.enable_lookahead |> Player.advance_turn(avenue.turn),
      log:
        log |> Status.add_action(PeekFarm) |> Status.add_event(TurnSkipped),
    }
    : t;

let flip_road = ({me, avenue, log} as t) =>
  avenue |> Avenue.Rules.can_flip_road(me)
    ? {
      ...t,
      avenue:
        avenue
        |> Avenue.set_current_road
        |> Avenue.discard_top_road
        |> Avenue.advance_stage
        |> Avenue.advance_game_turn,
      log: log |> Status.add_action(FlipRoad),
    }
    : t;

let draw_road = (row, col) =>
  fun
  | {me, avenue: {turn, current_card: Some((road, _))} as avenue, log} as t =>
    avenue |> Avenue.Rules.can_draw_road(me, row, col)
      ? {
        ...t,
        me:
          me
          |> Player.draw_road_on_grid_cell(road, row, col)
          |> Player.advance_turn(turn)
          |> Player.recount_points(avenue.farms),
        log: log |> Status.add_action(DrawRoad(row, col)),
      }
      : t
  | {avenue: {current_card: None}} as t => t;

let score_zero_penalty =
  fun
  | {me: {current_round_points: Some((farm, _))} as me, log} as t =>
    me->Avenue.Rules.has_scored_zero
      ? {
        ...t,
        me: {
          ...me,
          current_round_points: Some((farm, (-5))),
        },
        log: log |> Status.add_event(ScoredZero(farm->Farm.string_of_farm)),
      }
      : t
  | {me: {current_round_points: None}} as t => t;

let score_less_penalty =
  fun
  | {
      me:
        {current_round_points: Some((farm, points)), previous_round_points} as me,
      log,
    } as t =>
    me->Avenue.Rules.has_scored_less
      ? {
        ...t,
        me: {
          ...me,
          current_round_points: Some((farm, (-5))),
        },
        log:
          log
          |> Status.add_event(
               ScoredNotEnough(
                 points,
                 farm->Farm.string_of_farm,
                 switch (previous_round_points |> List.hd) {
                 | (_, previous) => previous
                 },
               ),
             ),
      }
      : t
  | {me: {current_round_points: None}} as t => t;

let end_round =
  fun
  | {me, avenue: {stage: Round(farm, _)} as avenue, log} as t =>
    avenue |> Avenue.Rules.can_end_round(me)
      ? {
          ...t,
          avenue: avenue |> Avenue.advance_stage,
          log: log |> Status.add_round_over_event(farm),
        }
        |> score_zero_penalty
        |> score_less_penalty
      : t
  | {avenue: {stage: Flow(_)}} as t => t;

let end_game = ({avenue, log} as t) => {
  avenue->Avenue.Rules.can_end_game
    ? {
      ...t,
      avenue: avenue |> Avenue.advance_stage,
      log: log |> Status.add_event(GameIsOver),
    }
    : t;
};

let guide = ({me, avenue} as t) => {
  ...t,
  guide: avenue |> Status.guide(me),
};

// TODO: create reducers for parts of game state (namely log, but also anywhere else creating dependency cycles)
// actions can potentially replace existing game update functions in Avenue module
// this should decentralize state from game type
// root reducer should look like
// reduce game action = {
//   turn: reduce_turn turn action
//   players: reduce_players players action
//   road_deck: reduce_road_deck road_deck action
//   farm_deck: reduce_farm_deck farm_deck action
//   stage: reduce_stage stage action
//   current_card: reduce_current_card current_card action
//   castles: reduce_castles castles action
//   farms: reduce_farms farms action
//   log: reduce_log log action
//   guide: reduce_guide guide action
// }
// with this, keep components with all logic related to it;
// consider using submodules for different types of related components:
// in Farm.re
// module SVG { let make ... }
// which will be <Farm.SVG />
// but only in case it still causes conflicts or dependency cycles
let reducer = game =>
  fun
  | Avenue.PeekFarm => game |> peek_farm |> guide
  | FlipFarm => game |> flip_farm |> guide
  | FlipRoad => game |> flip_road |> guide
  | DrawRoad(row, col) =>
    game |> draw_road(row, col) |> end_round |> end_game |> guide;

[@react.component]
let make = () => {
  let (game, dispatch) =
    React.useReducer(
      reducer,
      setup(
        "me",
        Grid.setup(Grid.map_A),
        RoadDeck.setup(),
        FarmDeck.setup(),
      ),
    );

  <svg width="98vmin" height="98vmin" viewBox="-2 -2 102 102">
    Theme.filters
    <g>
      {game.me.grid
       |> Array.to_list
       |> Array.concat
       |> Array.mapi((i, cell) =>
            <Cell
              key={i |> string_of_int}
              cell
              dispatch={(row, col) => dispatch(DrawRoad(row, col))}
            />
          )
       |> ReasonReact.array}
    </g>
    <RoadDeck
      road_deck={game.avenue.road_deck}
      current_card={game.avenue.current_card}
      dispatch
    />
    <FarmDeck
      me={game.me}
      farm_deck={game.avenue.farm_deck}
      stage={game.avenue.stage}
      dispatch
    />
    <Points
      stage={game.avenue.stage}
      grid={game.me.grid}
      round_points={
        switch (game.me.current_round_points) {
        | Some(points) => [points, ...game.me.previous_round_points]
        | None => game.me.previous_round_points
        }
      }
      castles={game.avenue.castles}
      farm_deck={game.avenue.farm_deck}
    />
    <Status guide={game.guide} log={game.log} />
  </svg>;
};