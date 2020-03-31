open Types;
open Converters;

let card_thickness = 0.5;

[@react.component]
let make = (~game as {players, phase_deck, stage}, ~dispatch) => {
  let can_peek =
    switch (stage) {
    | Phase(_, _) =>
      switch (players) {
      | [{lookahead}, ..._] => lookahead
      | [] => false
      }
    | PhaseEnd(_)
    | Begin => true
    | End(_) => false
    };
  let (rotation, setRotation) = React.useState(_ => 0);
  React.useEffect1(
    () => {
      switch (stage) {
      | End(_) => setRotation(_ => 90)
      | PhaseEnd(_)
      | Begin
      | Phase(_, _) => ()
      };
      None;
    },
    [|stage|],
  );
  <g
    onClick={_evt =>
      switch (stage) {
      | Phase(_, _) =>
        dispatch(PeekFarm);
        setRotation(_ => 90);
        let _ = Js.Global.setTimeout(() => setRotation(_ => 0), 500);
        ();
      | Begin
      | PhaseEnd(_) =>
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
    {phase_deck
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
                     strokeWidth={card_thickness /. 2. |> Js.Float.toString}
                     fillOpacity="1"
                     fill="cornflowerblue"
                     style=Theme.big_text>
                     {farm->string_of_farm->str}
                   </text>
                 </g>
               : React.null}
          </g>
        )
     |> Array.of_list
     |> ReasonReact.array}
    <g
      key={phase_deck |> List.length |> string_of_int}
      transform={
        "translate("
        ++ (
          card_thickness
          *. (phase_deck |> List.length)->float_of_int
          |> Js.Float.toString
        )
        ++ " "
        ++ (
          card_thickness
          *. (phase_deck |> List.length)->float_of_int
          |> Js.Float.toString
        )
        ++ ")"
      }>
      <g style={Theme.rotate_card(rotation)}>
        <rect
          key={phase_deck |> List.length |> string_of_int}
          width="15"
          height="20"
          rx="2"
          fill={
            switch (stage) {
            | Phase(_, _)
            | PhaseEnd(_)
            | End(_) => "yellow"
            | _ => "cornflowerblue"
            }
          }
          stroke="white"
          strokeWidth="1"
          style=Theme.shadow
        />
        {switch (stage) {
         | Phase(farm, _)
         | PhaseEnd(farm)
         | End(farm) =>
           <g transform="translate(4.5 12.5)" strokeWidth="0.1">
             <text
               strokeWidth={card_thickness /. 2. |> Js.Float.toString}
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