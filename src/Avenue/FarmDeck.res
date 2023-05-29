open Common

let setup = () => {
  let rec aux = (farm_deck, x) =>
    switch x {
    | 0 => farm_deck
    | n =>
      let farm_card = Farm.random_farm()
      List.for_all(card => card != farm_card, farm_deck)
        ? aux(list{farm_card, ...farm_deck}, n - 1)
        : aux(farm_deck, n)
    }
  aux(list{}, 6)
}

@react.component
let make = (~me as {lookahead}: Player.t, ~farm_deck, ~stage: Stage.t, ~dispatch) => {
  let (rotation, setRotation) = React.useState(_ => 0)
  let will_peek = switch stage {
  | Round(_, _) => lookahead
  | Flow(Ready | RoundEnd) => true
  | Flow(Created | End) => false
  }
  React.useEffect1(() => {
    switch stage {
    | Flow(End) => setRotation(_ => 90)
    | Flow(_)
    | Round(_, _) => ()
    }
    None
  }, [stage])
  <g
    onClick={_evt =>
      switch stage {
      | Round(_, _) =>
        dispatch(Play(PeekFarm))
        setRotation(_ => 90)
        let _ = Js.Global.setTimeout(() => setRotation(_ => 0), 500)
      | Flow(Created)
      | Flow(Ready)
      | Flow(RoundEnd)
      | Flow(End) =>
        setRotation(_ => 90)
        let _ = Js.Global.setTimeout(() => {
          dispatch(Play(FlipFarm))
          setRotation(_ => 0)
        }, 500)
      }}
    transform="translate(110 2)">
    {farm_deck
    |> List.rev
    |> List.mapi((i, farm) =>
      <g
        key={i->string_of_int}
        transform={"translate(" ++
        ((Theme.farm_card_thickness *. i->float_of_int |> Js.Float.toString) ++
        (" " ++ ((Theme.farm_card_thickness *. i->float_of_int |> Js.Float.toString) ++ ")")))}>
        <rect
          key={i |> string_of_int}
          width="15"
          height="20"
          rx="2"
          fill={will_peek ? "yellow" : "cornflowerblue"}
          stroke="white"
          strokeWidth="1"
          style=Theme.shadow
        />
        {will_peek
          ? <g transform="translate(4.5 12.5)" strokeWidth="0.1">
              <text
                strokeWidth={Theme.farm_card_thickness /. 2. |> Js.Float.toString}
                fillOpacity="1"
                fill="cornflowerblue"
                style=Theme.big_text>
                {farm->Farm.to_string->str}
              </text>
            </g>
          : React.null}
      </g>
    )
    |> arr}
    <g
      key={farm_deck |> List.length |> string_of_int}
      transform={"translate(" ++
      ((Theme.farm_card_thickness *. (farm_deck |> List.length)->float_of_int
        |> Js.Float.toString) ++
      (" " ++
      ((Theme.farm_card_thickness *. (farm_deck |> List.length)->float_of_int
        |> Js.Float.toString) ++
      ")")))}>
      <g style={Theme.rotate_card(rotation)}>
        <rect
          key={farm_deck |> List.length |> string_of_int}
          width="15"
          height="20"
          rx="2"
          fill={switch stage {
          | Round(_, _) => "yellow"
          | Flow(_) => "cornflowerblue"
          }}
          stroke="white"
          strokeWidth="1"
          style=Theme.shadow
        />
        {switch stage {
        | Round(farm, _) =>
          <g transform="translate(4.5 12.5)" strokeWidth="0.1">
            <text
              strokeWidth={Theme.farm_card_thickness /. 2. |> Js.Float.toString}
              fillOpacity="1"
              fill="cornflowerblue"
              style=Theme.big_text>
              {farm->Farm.to_string->str}
            </text>
          </g>
        | Flow(_) => React.null
        }}
      </g>
    </g>
  </g>
}
