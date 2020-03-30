open Types;
open Converters;

let card_thickness = 0.5;

[@react.component]
let make = (~deck, ~stage, ~dispatch) => {
  let (topRotation, setTopRotation) = React.useState(_ => 0);
  let (fastReturn, setFastReturn) = React.useState(_ => false);
  <g
    onClick={_evt =>
      switch (stage) {
      | Phase(_, _) =>
        dispatch(PeekFarm);
        setTopRotation(_ => 90);
        let _ = Js.Global.setTimeout(() => setTopRotation(_ => 0), 500);
        ();
      | Begin
      | PhaseEnd =>
        setTopRotation(_ => 90);
        let _ =
          Js.Global.setTimeout(
            () => {
              dispatch(FlipFarm);
              setFastReturn(_ => true);
              setTopRotation(_ => 0);
              setFastReturn(_ => false);
            },
            500,
          );
        ();
      | End => ()
      }
    }
    transform="translate(63 25)">
    {deck
     |> List.rev
     |> List.mapi((i, farm) =>
          <g
            key={i->string_of_int}
            transform={
              "translate("
              ++ (card_thickness *. i->float_of_int |> Js.Float.toString)
              ++ " "
              ++ (card_thickness *. i->float_of_int |> Js.Float.toString)
              ++ ")"
            }>
            <g
              style={
                fastReturn
                  ? Theme.no_transition
                  : Theme.rotate_card(
                      i == (deck |> List.length) - 1 ? topRotation : 0,
                    )
              }>
              <rect
                key={i |> string_of_int}
                width="15"
                height="20"
                rx="2"
                fill={
                  switch (farm) {
                  | None => "cornflowerblue"
                  | Some(_) => "yellow"
                  }
                }
                stroke="white"
                strokeWidth="1"
                style=Theme.shadow
              />
              {switch (farm) {
               | None => React.null
               | Some(farm) =>
                 <g transform="translate(4.5 12.5)" strokeWidth="0.1">
                   <text
                     strokeWidth={card_thickness /. 2. |> Js.Float.toString}
                     fillOpacity="1"
                     fill="cornflowerblue"
                     style=Theme.big_text>
                     {farm->string_of_farm->str}
                   </text>
                 </g>
               }}
            </g>
          </g>
        )
     |> Array.of_list
     |> ReasonReact.array}
  </g>;
};