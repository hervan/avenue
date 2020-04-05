open Types;
open Converters;

[@react.component]
let make = (~game as {players, round_deck, stage}, ~dispatch) => {
  let (rotation, setRotation) = React.useState(_ => 0);
  let can_peek =
    switch (stage) {
    | Round(_, _) =>
      switch (players) {
      | [{lookahead}, ..._] => lookahead
      | [] => false
      }
    | RoundEnd(_)
    | Begin => true
    | End(_) => false
    };
  React.useEffect1(
    () => {
      switch (stage) {
      | End(_) => setRotation(_ => 90)
      | RoundEnd(_)
      | Begin
      | Round(_, _) => ()
      };
      None;
    },
    [|stage|],
  );
  <g
    onClick={_evt =>
      switch (stage) {
      | Round(_, _) =>
        dispatch(PeekFarm);
        setRotation(_ => 90);
        let _ = Js.Global.setTimeout(() => setRotation(_ => 0), 500);
        ();
      | Begin
      | RoundEnd(_) =>
        setRotation(_ => 90);
        let _ =
          Js.Global.setTimeout(
            () => {
              dispatch(FlipFarm);
              setRotation(_ => 0);
            },
            500,
          );
        ();
      | End(_) => ()
      }
    }
    transform="translate(63 25)">
    {round_deck
     |> List.rev
     |> List.mapi((i, farm) =>
          <g
            key={i->string_of_int}
            transform={
              "translate("
              ++ (
                Theme.farm_card_thickness
                *. i->float_of_int
                |> Js.Float.toString
              )
              ++ " "
              ++ (
                Theme.farm_card_thickness
                *. i->float_of_int
                |> Js.Float.toString
              )
              ++ ")"
            }>
            <rect
              key={i |> string_of_int}
              width="15"
              height="20"
              rx="2"
              fill={can_peek ? "yellow" : "cornflowerblue"}
              stroke="white"
              strokeWidth="1"
              style=Theme.shadow
            />
            {can_peek
               ? <g transform="translate(4.5 12.5)" strokeWidth="0.1">
                   <text
                     strokeWidth={
                       Theme.farm_card_thickness /. 2. |> Js.Float.toString
                     }
                     fillOpacity="1"
                     fill="cornflowerblue"
                     style=Theme.big_text>
                     {farm->string_of_farm->str}
                   </text>
                 </g>
               : React.null}
          </g>
        )
     |> arr}
    <g
      key={round_deck |> List.length |> string_of_int}
      transform={
        "translate("
        ++ (
          Theme.farm_card_thickness
          *. (round_deck |> List.length)->float_of_int
          |> Js.Float.toString
        )
        ++ " "
        ++ (
          Theme.farm_card_thickness
          *. (round_deck |> List.length)->float_of_int
          |> Js.Float.toString
        )
        ++ ")"
      }>
      <g style={Theme.rotate_card(rotation)}>
        <rect
          key={round_deck |> List.length |> string_of_int}
          width="15"
          height="20"
          rx="2"
          fill={
            switch (stage) {
            | Round(_, _)
            | RoundEnd(_)
            | End(_) => "yellow"
            | _ => "cornflowerblue"
            }
          }
          stroke="white"
          strokeWidth="1"
          style=Theme.shadow
        />
        {switch (stage) {
         | Round(farm, _)
         | RoundEnd(farm)
         | End(farm) =>
           <g transform="translate(4.5 12.5)" strokeWidth="0.1">
             <text
               strokeWidth={
                 Theme.farm_card_thickness /. 2. |> Js.Float.toString
               }
               fillOpacity="1"
               fill="cornflowerblue"
               style=Theme.big_text>
               {farm->string_of_farm->str}
             </text>
           </g>
         | _ => React.null
         }}
      </g>
    </g>
  </g>;
};