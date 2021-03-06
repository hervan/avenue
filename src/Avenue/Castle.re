[@react.component]
let make = (~grape) => {
  let x0 = 0;
  let y0 = 0;
  let x1 = x0 + 6;
  let y1 = y0;
  let x2 = x1;
  let y2 = y1 - 4;
  let x3 = x2 - 1;
  let y3 = y2 - 2;
  let x4 = x3 - 1;
  let y4 = y3 + 2;
  let x5 = x4 - 2;
  let y5 = y4;
  let x6 = x5 - 1;
  let y6 = y5 - 2;
  let x7 = x6 - 1;
  let y7 = y6 + 2;
  <polygon
    points={j|$x0 $y0 $x1 $y1 $x2 $y2 $x3 $y3 $x4 $y4 $x5 $y5 $x6 $y6 $x7 $y7|j}
    fill={grape->Grape.color_of_grape}
    stroke={grape->Grape.color_of_grape}
    strokeWidth="0.25"
    fillOpacity="0.5"
  />;
};