let road_card_thickness = 0.05;

let farm_card_thickness = 0.5;

let link = ReactDOMRe.Style.make(~cursor="pointer", ());

let text = fontSize =>
  ReactDOMRe.Style.make(
    ~fontSize,
    ~fontFamily="Verdana",
    ~userSelect="none",
    (),
  );

let log_text =
  ReactDOMRe.Style.make(
    ~fontSize="2.4px",
    ~fontFamily="Verdana",
    ~filter="url(#text-shadow)",
    (),
  );

let big_text =
  text("8px")
  |> ReactDOMRe.Style.combine(ReactDOMRe.Style.make(~fontWeight="bold", ()));

let quick_transition = property =>
  ReactDOMRe.Style.make(~transition={j|$property 0.5s|j}, ());

let no_transition = ReactDOMRe.Style.make();

let shadow = ReactDOMRe.Style.make(~filter="url(#shadow)", ());

let road_shadow = ReactDOMRe.Style.make(~filter="url(#road-shadow)", ());

let rotate_card = rotation =>
  ReactDOMRe.Style.make(
    ~transitionDuration="0.5s",
    ~transitionProperty="transform",
    ~transform="rotateY(" ++ rotation->string_of_int ++ "deg)",
    (),
  );

let button_depth = 0.25;

let filters =
  <defs>
    <filter id="shadow">
      <feDropShadow
        dx="0"
        dy="0"
        stdDeviation="0.25"
        floodColor="black"
        floodOpacity="0.5"
      />
    </filter>
    <filter id="text-shadow">
      <feDropShadow
        dx="0"
        dy="0"
        stdDeviation="0.2"
        floodColor="black"
        floodOpacity="0.2"
      />
    </filter>
    <filter id="road-shadow">
      <feDropShadow
        dx="0"
        dy="0"
        stdDeviation={road_card_thickness |> Js.Float.toString}
        floodColor="black"
        floodOpacity="0.5"
      />
    </filter>
  </defs>;