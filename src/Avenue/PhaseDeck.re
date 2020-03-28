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
    <defs>
      <filter id="phase-shadow">
        <feDropShadow
          dx="0"
          dy="0"
          stdDeviation={card_thickness |> Js.Float.toString}
          floodColor="black"
          floodOpacity="0.5"
        />
      </filter>
    </defs>
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
            style={ReactDOMRe.Style.make(~filter="url(#phase-shadow)", ())}
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
           style={ReactDOMRe.Style.make(~filter="url(#phase-shadow)", ())}
         />
         <g transform="translate(4.5 12.5)" strokeWidth="0.1">
           <text
             strokeWidth={card_thickness /. 2. |> Js.Float.toString}
             fillOpacity="1"
             fill="cornflowerblue"
             style={ReactDOMRe.Style.make(
               ~fontSize="8px",
               ~fontWeight="bold",
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