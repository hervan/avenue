open Converters;

[@react.component]
let make = (~farm) => {
  let x0 = 5.;
  let y0 = 9.;
  let x1 = x0 +. 4.;
  let y1 = y0;
  let x2 = x1;
  let y2 = y1 -. 3.;
  let x3 = x2 -. 2.;
  let y3 = y2 -. 1.;
  let x4 = x3 -. 2.;
  let y4 = y3 +. 1.;
  <>
    <polygon
      points={j|$x0 $y0 $x1 $y1 $x2 $y2 $x3 $y3 $x4 $y4|j}
      fillOpacity="0"
      stroke="grey"
    />
    <text
      x={(x0 +. 1.2)->Js.Float.toString}
      y={(y0 -. 0.8)->Js.Float.toString}
      strokeWidth="0"
      fillOpacity="0.5"
      style={ReactDOMRe.Style.make(
        ~fontSize="2.3",
        ~fontFamily="Verdana",
        ~userSelect="none",
        (),
      )}>
      {farm->string_of_farm->str}
    </text>
  </>;
};