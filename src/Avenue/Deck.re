open Types;
open Converters;

let card_thickness = 0.05;

let road_shadow = ReactDOMRe.Style.make(~filter="url(#road-shadow)", ());

[@react.component]
let make = (~game as {deck, current_card}, ~dispatch) => {
  let (rotation, setRotation) = React.useState(_ => 90);
  React.useEffect1(
    _ => {
      switch (current_card) {
      | None => setRotation(_ => 90)
      | Some(_) => setRotation(_ => 0)
      };
      None;
    },
    [|current_card|],
  );
  <g
    transform="translate(63 0)"
    onClick={_evt => {
      setRotation(_ => 90);
      let _ =
        Js.Global.setTimeout(
          _ => {
            dispatch(FlipRoad);
            setRotation(_ => 0);
          },
          500,
        );
      ();
    }}>
    <defs>
      <filter id="road-shadow">
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
            style=road_shadow
          />
        )
     |> arr}
    {switch (current_card) {
     | None =>
       <g style={Theme.rotate_card(rotation)}>
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
           fill="lightblue"
           stroke="white"
           strokeWidth="1"
           style=road_shadow
         />
       </g>
     | Some((road, color)) =>
       <g style={Theme.rotate_card(rotation)}>
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
           style=road_shadow
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
           <RoadCard road />
         </g>
       </g>
     }}
  </g>;
};