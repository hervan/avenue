open Types;

let card_thickness = 0.5;
[@react.component]
let make = (~deck, ~current_phase, ~dispatch) => {
  <g onClick={_evt => dispatch(RevealPhase)} transform="translate(65 25)">
    {deck
     |> List.mapi((i, _) =>
          <rect
            key={i |> string_of_int}
            x={card_thickness *. (i |> float_of_int) |> Js.Float.toString}
            y={card_thickness *. (i |> float_of_int) |> Js.Float.toString}
            width="15"
            height="20"
            rx="2"
            fill="cornflowerblue"
            stroke="black"
            strokeWidth="0.025"
          />
        )
     |> Array.of_list
     |> ReasonReact.array}
    {switch (current_phase) {
     | Phase(farm) =>
       <>
         <rect
           x={
             card_thickness
             *. (List.length(deck) |> float_of_int)
             |> Js.Float.toString
           }
           y={
             card_thickness
             *. (List.length(deck) |> float_of_int)
             |> Js.Float.toString
           }
           width="15"
           height="20"
           rx="2"
           fill="black"
           stroke="black"
           strokeWidth={card_thickness |> Js.Float.toString}
         />
         <g
           transform={
             "translate("
             ++ (
               2.5
               +. 0.15
               *. (List.length(deck) |> float_of_int)
               |> Js.Float.toString
             )
             ++ " "
             ++ (
               5.0
               +. 0.15
               *. (List.length(deck) |> float_of_int)
               |> Js.Float.toString
             )
             ++ ")"
           }>
           <rect
             width="10"
             height="10"
             rx="1"
             fill="white"
             stroke="black"
             strokeWidth="0.1"
           />
           <Farm farm />
         </g>
       </>
     | _ => React.null
     }}
  </g>;
};