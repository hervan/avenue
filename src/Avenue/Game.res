open Common

type t = {
  avenue: Avenue.t,
  me: Player.t,
  players: list<Player.t>,
  log: list<(option<play_action>, list<Status.event>)>,
  guide: list<action>,
}

let can_create = x =>
  switch x {
  | {avenue: {stage: Flow(Created | Ready | End)}} => true
  | {avenue: {stage: Flow(RoundEnd)}}
  | {avenue: {stage: Round(_, _)}} => false
  }

let can_undo = ({log}) =>
  switch log {
  | list{(Some(DrawRoad(_, _)), _), ..._} => true
  | _ => false
  }

let can_restart = x =>
  switch x {
  | {avenue: {stage: Flow(RoundEnd | End)}}
  | {avenue: {stage: Round(_, _)}} => true
  | {avenue: {stage: Flow(Created | Ready)}} => false
  }

let guide_create = (t, guide) =>
  can_create(t) ? guide |> Status.add_suggestion(Control(Create)) : guide

let guide_undo = (t, guide) => can_undo(t) ? guide |> Status.add_suggestion(Control(Undo)) : guide

let guide_restart = (t, guide) =>
  can_restart(t) ? guide |> Status.add_suggestion(Control(Restart)) : guide

let guide = ({me, avenue} as t) => {
  ...t,
  guide: avenue |> Status.guide(me) |> guide_create(t) |> guide_undo(t) |> guide_restart(t),
}

let load_setup = (seed, player_name, base_grid, road_deck, farm_deck) => {
  let avenue = Avenue.setup(seed, base_grid, road_deck, farm_deck)
  let me = Player.setup(player_name, base_grid)
  {avenue: avenue, log: list{(None, list{})}, guide: list{}, me: me, players: list{}}
}

let init_seed = x =>
  switch x {
  | None => Random.init(0)
  | Some(seed) => Random.init(seed)
  }

let begin_game = x =>
  switch x {
  | {avenue: {seed: Some(_), stage: Flow(Created)}} as t => {
      ...t,
      avenue: t.avenue |> Avenue.advance_stage,
    }
  | {avenue: {seed: Some(_), stage: Flow(Ready | RoundEnd | End) | Round(_, _)}} as t
  | {avenue: {seed: None}} as t => t
  }

let setup = (seed, player_name) => {
  init_seed(seed)
  let base_grid = Grid.setup(Grid.map_A)
  let road_deck = RoadDeck.setup()
  let farm_deck = FarmDeck.setup()
  load_setup(seed, player_name, base_grid, road_deck, farm_deck) |> begin_game |> guide
}

let create_game = () => {
  let seed = {
    Random.self_init()
    let seed = Random.bits()
    ReasonReactRouter.replace(j`/$seed`)
    Some(seed)
  }

  setup(seed, "me") |> (
    t => {
      ...t,
      log: t.log |> Status.add_action(None) |> Status.add_event(GameStarted),
    }
  )
}

let restart_game = t => setup(t.avenue.seed, t.me.farmer)

let flip_farm = ({me, avenue: {farm_deck} as avenue, log} as t) =>
  avenue->Avenue.Rules.can_flip_farm
    ? {
        ...t,
        avenue: avenue->Avenue.reducer(Avenue.FlipFarm),
        me: me->Player.reducer(FlipFarm(farm_deck->List.nth(0))),
        log: log
        |> Status.add_action(Some(FlipFarm))
        |> Status.add_round_start_event(farm_deck |> List.hd),
      }
    : t

let peek_farm = ({me, avenue, log} as t) =>
  avenue |> Avenue.Rules.can_peek_farm(me)
    ? {
        ...t,
        me: me |> Player.enable_lookahead |> Player.advance_turn(avenue.turn),
        log: log |> Status.add_action(Some(PeekFarm)) |> Status.add_event(TurnSkipped),
      }
    : t

let flip_road = ({me, avenue, log} as t) =>
  avenue |> Avenue.Rules.can_flip_road(me)
    ? {
        ...t,
        avenue: avenue->Avenue.reducer(FlipRoad),
        log: log |> Status.add_action(Some(FlipRoad)),
      }
    : t

let draw_road = (row, col, x) =>
  switch x {
  | {me, avenue: {turn, current_card: Some((road, _))} as avenue, log} as t =>
    avenue |> Avenue.Rules.can_draw_road(me, row, col)
      ? {
          ...t,
          me: me->Player.reducer(DrawRoad(road, row, col, turn)),
          log: log |> Status.add_action(Some(DrawRoad(row, col))),
        }
      : t
  | {avenue: {current_card: None}} as t => t
  }

let score_zero_penalty = x =>
  switch x {
  | {me: {current_round_points: Some((farm, _))} as me, log} as t =>
    me->Avenue.Rules.has_scored_zero
      ? {
          ...t,
          me: {
            ...me,
            current_round_points: Some((farm, -5)),
          },
          log: log |> Status.add_event(ScoredZero(farm->Farm.to_string)),
        }
      : t
  | {me: {current_round_points: None}} as t => t
  }

let score_less_penalty = x =>
  switch x {
  | {me: {current_round_points: Some((farm, points)), previous_round_points} as me, log} as t =>
    me->Avenue.Rules.has_scored_less
      ? {
          ...t,
          me: {
            ...me,
            current_round_points: Some((farm, -5)),
          },
          log: log |> Status.add_event(
            ScoredNotEnough(
              points,
              farm->Farm.to_string,
              switch previous_round_points |> List.hd {
              | (_, previous) => previous
              },
            ),
          ),
        }
      : t
  | {me: {current_round_points: None}} as t => t
  }

let round_penalty = t => t |> score_zero_penalty |> score_less_penalty

let end_round = x =>
  switch x {
  | {me, avenue: {stage: Round(farm, _)} as avenue, log} as t =>
    avenue |> Avenue.Rules.can_end_round(me)
      ? {
          ...t,
          avenue: avenue |> Avenue.advance_stage,
          log: log |> Status.add_round_over_event(farm),
        }
        |> round_penalty
        |> (
          x =>
            switch x {
            | t => {...t, me: t.me |> Player.keep_round_points}
            }
        )
      : t
  | {avenue: {stage: Flow(_)}} as t => t
  }

let end_game = ({avenue, log} as t) =>
  avenue->Avenue.Rules.can_end_game
    ? {
        ...t,
        avenue: avenue |> Avenue.advance_stage,
        log: log |> Status.add_event(
          GameIsOver(
            Points.total_points(t.me.previous_round_points, t.avenue.castles, t.me.grid),
            switch t.avenue.seed {
            | Some(seed) => seed
            | None => raise(Impossible("every game must have a seed"))
            },
          ),
        ),
      }
    : t

let play_reducer = (t, x) =>
  switch x {
  | PeekFarm => t |> peek_farm |> guide
  | FlipFarm => t |> flip_farm |> guide
  | FlipRoad => t |> flip_road |> guide
  | DrawRoad(row, col) => t |> draw_road(row, col) |> end_round |> end_game |> guide
  }

let undo = t => {
  let previous_actions = t.log |> List.tl |> List.rev
  {
    ...previous_actions |> List.fold_left((acc, (action, _)) =>
      switch action {
      | Some(action) => acc->play_reducer(action)
      | None => acc
      }
    , t |> restart_game),
    log: t.log |> List.tl,
  }
}

let control_reducer = (t, x) =>
  switch x {
  | Create => create_game() |> guide
  | Restart => t |> restart_game |> guide
  | Undo => t |> undo |> guide
  }

let reducer = (t, x) =>
  switch x {
  | Play(play_action) => t->play_reducer(play_action)
  | Control(control_action) => t->control_reducer(control_action)
  }

@react.component
let make = () => {
  let url = ReasonReactRouter.useUrl()
  let seed = switch url.path->List.nth_opt(0) {
  | Some(seed) => seed->int_of_string_opt
  | None => None
  }

  let (game, dispatch) = React.useReducer(reducer, setup(seed, "me"))

  let gridScale =
    10. /. float_of_int(max(game.me.grid->Array.length, game.me.grid[0]->Array.length))

  <svg width="100vw" height="100vh" viewBox="0 0 360 100" preserveAspectRatio="xMinYMin slice">
    Theme.filters
    <g transform=j`scale($gridScale)`>
      {game.me.grid
      |> Array.to_list
      |> Array.concat
      |> Array.mapi((i, cell) => <Cell key={i |> string_of_int} cell dispatch />)
      |> ReasonReact.array}
    </g>
    <RoadDeck road_deck=game.avenue.road_deck current_card=game.avenue.current_card dispatch />
    <FarmDeck me=game.me farm_deck=game.avenue.farm_deck stage=game.avenue.stage dispatch />
    <Points
      stage=game.avenue.stage
      grid=game.me.grid
      round_points={switch game.me.current_round_points {
      | Some(points) => list{points, ...game.me.previous_round_points}
      | None => game.me.previous_round_points
      }}
      castles=game.avenue.castles
      farm_deck=game.avenue.farm_deck
    />
    <Status guide=game.guide log=game.log dispatch />
  </svg>
}
