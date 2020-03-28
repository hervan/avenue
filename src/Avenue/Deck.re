open Types;
open Converters;

let card_thickness = 0.05;

let stretch_shadow =
  ReactDOMRe.Style.make(~filter="url(#stretch-shadow)", ());

[@react.component]
let make = (~deck, ~current_card, ~dispatch) => {
  <g
    onClick={_evt => dispatch(RevealStretchCard)} transform="translate(65 0)">
    <defs>
      <filter id="stretch-shadow">
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
            fill="lightblue"
            stroke="white"
            strokeWidth="1"
            style=stretch_shadow
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
           stroke="white"
           strokeWidth="1"
           style=stretch_shadow
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
             stroke="white"
             strokeWidth="0"
             style=Theme.shadow
           />
           <StretchCard stretch />
         </g>
       </>
     }}
  </g>;
};