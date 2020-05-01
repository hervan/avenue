open Common;

type t = {
  seed: int,
  avenue: Avenue.t,
  me: Player.t,
  players: list(Player.t),
  log: list((play_action, list(Stage.event))),
  guide: list(play_action),
};

let load_setup = (seed, player_name, base_grid, road_deck, farm_deck) => {
  let avenue = Avenue.setup(base_grid, road_deck, farm_deck);
  let me = Player.setup(player_name, base_grid);
  {
    seed,
    avenue,
    log: [],
    guide: avenue |> Status.guide(me),
    me,
    players: [],
  };
};

let setup = (seed, player_name) => {
  Random.init(seed);
  let base_grid = Grid.setup(Grid.map_A);
  let road_deck = RoadDeck.setup();
  let farm_deck = FarmDeck.setup();
  load_setup(seed, player_name, base_grid, road_deck, farm_deck);
};

let start = t => {...t, avenue: t.avenue |> Avenue.advance_stage};

let flip_farm = ({me, avenue: {farm_deck} as avenue, log} as t) =>
  avenue->Avenue.Rules.can_flip_farm
    ? {
      ...t,
      avenue: avenue->Avenue.reducer(Avenue.FlipFarm),
      me: me->Player.reducer(FlipFarm(farm_deck->List.nth(0))),
      log:
        log
        |> Status.add_action(FlipFarm)
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
      avenue: avenue->Avenue.reducer(FlipRoad),
      log: log |> Status.add_action(FlipRoad),
    }
    : t;

let draw_road = (row, col) =>
  fun
  | {me, avenue: {turn, current_card: Some((road, _))} as avenue, log} as t =>
    avenue |> Avenue.Rules.can_draw_road(me, row, col)
      ? {
        ...t,
        me: me->Player.reducer(DrawRoad(road, row, col, turn)),
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
        log: log |> Status.add_event(ScoredZero(farm->Farm.to_string)),
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
                 farm->Farm.to_string,
                 switch (previous_round_points |> List.hd) {
                 | (_, previous) => previous
                 },
               ),
             ),
      }
      : t
  | {me: {current_round_points: None}} as t => t;

let round_penalty = t => t |> score_zero_penalty |> score_less_penalty;

let end_round =
  fun
  | {me, avenue: {stage: Round(farm, _)} as avenue, log} as t =>
    avenue |> Avenue.Rules.can_end_round(me)
      ? {
          ...t,
          avenue: avenue |> Avenue.advance_stage,
          log: log |> Status.add_round_over_event(farm),
        }
        |> round_penalty
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

let play_reducer = t =>
  fun
  | PeekFarm => t |> peek_farm |> guide
  | FlipFarm => t |> flip_farm |> guide
  | FlipRoad => t |> flip_road |> guide
  | DrawRoad(row, col) =>
    t |> draw_road(row, col) |> end_round |> end_game |> guide;

let undo = t => {
  let previous_actions = t.log |> List.tl |> List.rev;
  {
    ...
      previous_actions
      |> List.fold_left(
           (acc, (action, _)) => play_reducer(acc, action),
           setup(t.seed, t.me.farmer),
         ),
    log: t.log |> List.tl,
  };
};

let control_reducer = t =>
  fun
  | Start => t |> start |> guide
  | Restart => setup(t.seed, t.me.farmer) |> guide
  | Undo => t |> undo |> guide;

let reducer = t =>
  fun
  | Play(play_action) => t->play_reducer(play_action)
  | Control(control_action) => t->control_reducer(control_action);

[@react.component]
let make = () => {
  let url = ReasonReactRouter.useUrl();
  let seed_from_url =
    switch (url.path->List.nth_opt(0)) {
    | Some(seed) => seed->int_of_string_opt
    | None => None
    };
  let seed =
    switch (seed_from_url) {
    | Some(seed) => seed
    | None =>
      Random.self_init();
      let seed = Random.bits();
      ReasonReactRouter.replace({j|/$seed|j});
      seed;
    };
  let me = "me";

  let (game, dispatch) = React.useReducer(reducer, setup(seed, me));

  let dispatch_undo = () => dispatch(Control(Undo));
  let dispatch_flip_farm = () => dispatch(Play(FlipFarm));
  let dispatch_peek_farm = () => dispatch(Play(PeekFarm));
  let dispatch_flip_road = () => dispatch(Play(FlipRoad));
  let gridScale =
    10.
    /. float_of_int(
         max(game.me.grid->Array.length, game.me.grid[0]->Array.length),
       );

  <svg
    width="100vw"
    height="100vh"
    viewBox="0 0 360 100"
    preserveAspectRatio="xMinYMin slice">
    Theme.filters
    <g transform={j|scale($gridScale)|j}>
      {game.me.grid
       |> Array.to_list
       |> Array.concat
       |> Array.mapi((i, cell) =>
            <Cell
              key={i |> string_of_int}
              cell
              dispatch={(row, col) => dispatch(Play(DrawRoad(row, col)))}
            />
          )
       |> ReasonReact.array}
    </g>
    <RoadDeck
      road_deck={game.avenue.road_deck}
      current_card={game.avenue.current_card}
      dispatch_flip_road
    />
    <FarmDeck
      me={game.me}
      farm_deck={game.avenue.farm_deck}
      stage={game.avenue.stage}
      dispatch_peek_farm
      dispatch_flip_farm
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
    <Status guide={game.guide} log={game.log} dispatch_undo />
  </svg>;
};