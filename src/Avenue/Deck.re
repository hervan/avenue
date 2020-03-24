open Types;
open Converters;

let card_thickness = 0.05;
[@react.component]
let make = (~deck, ~current_card, ~dispatch) => {
  <g
    onClick={_evt => dispatch(RevealStretchCard)} transform="translate(65 0)">
    {deck
     |> List.mapi((i, _) =>
          <rect
            key={i |> string_of_int}
            x={card_thickness *. (i |> float_of_int) |> Js.Float.toString}
            y={card_thickness *. (i |> float_of_int) |> Js.Float.toString}
            width="15"
            height="20"
            rx="2"
            fill="lightblue"
            stroke="black"
            strokeWidth="0.025"
          />
        )
     |> Array.of_list
     |> ReasonReact.array}
    {switch (current_card) {
     | None => React.null
     | Some((stretch, color)) =>
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
           fill={color->string_of_card_color}
           stroke="black"
           strokeWidth={card_thickness *. 5. |> Js.Float.toString}
         />
         <g
           transform={
             "translate("
             ++ (
               2.5
               +. card_thickness
               *. (List.length(deck) |> float_of_int)
               |> Js.Float.toString
             )
             ++ " "
             ++ (
               5.0
               +. card_thickness
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
             strokeWidth="0.25"
           />
           <StretchCard stretch />
         </g>
       </>
     }}
  </g>;
};