open Types;
open Converters;

let card_thickness = 0.5;

[@react.component]
let make = (~deck, ~current_phase, ~dispatch) => {
  <g
    onClick={_evt => dispatch(RevealPhase)}
    onMouseDown={_evt => dispatch(PeekPhase)}
    onTouchStart={_evt => dispatch(PeekPhase)}
    onMouseUp={_evt => dispatch(PeekPhase)}
    onTouchEnd={_evt => dispatch(PeekPhase)}
    transform="translate(63 25)">
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
            stroke="white"
            strokeWidth="1"
            style=Theme.shadow
          />
        )
     |> Array.of_list
     |> ReasonReact.array}
    {switch (current_phase) {
     | Phase(farm) =>
       <g
         transform={
           "translate("
           ++ (
             card_thickness
             *. (List.length(deck) |> float_of_int)
             |> Js.Float.toString
           )
           ++ " "
           ++ (
             card_thickness
             *. (List.length(deck) |> float_of_int)
             |> Js.Float.toString
           )
           ++ ")"
         }>
         <rect
           width="15"
           height="20"
           rx="2"
           fill="yellow"
           stroke="white"
           strokeWidth="1"
           style=Theme.shadow
         />
         <g transform="translate(4.5 12.5)" strokeWidth="0.1">
           <text
             strokeWidth={card_thickness /. 2. |> Js.Float.toString}
             fillOpacity="1"
             fill="cornflowerblue"
             style=Theme.big_text>
             {farm->string_of_farm->str}
           </text>
         </g>
       </g>
     | _ => React.null
     }}
  </g>;
};