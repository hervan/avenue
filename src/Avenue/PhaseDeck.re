open Types;
open Converters;

let card_thickness = 0.5;

[@react.component]
let make = (~deck, ~current_phase, ~dispatch) => {
  <g
    onClick={_evt => dispatch(RevealPhase)}
    onMouseDown={_evt => dispatch(PeekPhase)}
    onMouseUp={_evt => dispatch(PeekPhase)}
    transform="translate(65 25)">
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
           fill="lightyellow"
           stroke="black"
           strokeWidth="0.025"
         />
         <g transform="translate(2.5 5)" strokeWidth="0.1">
           <text
             x="2.9"
             y="7.3"
             strokeWidth="0.1"
             stroke="black"
             fillOpacity="1"
             fill="cornflowerblue"
             style={ReactDOMRe.Style.make(
               ~fontSize="6px",
               ~fontFamily="Verdana",
               ~userSelect="none",
               (),
             )}>
             {farm->string_of_farm->str}
           </text>
         </g>
       </g>
     | _ => React.null
     }}
  </g>;
};