let app_style = j`
  body {
    position: fixed;
    border: 0;
    padding: 0;
    margin: 0;
  }

  @viewport {
    user-zoom: fixed;
  }

  @media (hover: none) {
    -webkit-tap-highlight-color: transparent;
    -webkit-overflow-scrolling: auto;
    *::-webkit-scrollbar {
      width: 0px;
    }
  }
`

let road_card_thickness = 0.075

let farm_card_thickness = 0.5

let line_height = 3.2

let text = fontSize =>
  ReactDOMRe.Style.make(~fontSize, ~fontFamily="Verdana", ~userSelect="none", ())

let quick_transition = property => ReactDOMRe.Style.make(~transition=j`$property 0.5s`, ())

let log_text =
  ReactDOMRe.Style.make(~fontSize="2.4px", ~fontFamily="Verdana", ()) |> ReactDOMRe.Style.combine(
    quick_transition("transform"),
  )

let guide_text =
  log_text |> ReactDOMRe.Style.combine(ReactDOMRe.Style.make(~filter="url(#text-background)", ()))

let big_text =
  text("8px") |> ReactDOMRe.Style.combine(ReactDOMRe.Style.make(~fontWeight="bold", ()))

let no_transition = ReactDOMRe.Style.make()

let shadow = ReactDOMRe.Style.make(~filter="url(#shadow)", ())

let button_shadow = ReactDOMRe.Style.make(~filter="url(#button-shadow)", ())

let link = button_shadow |> ReactDOMRe.Style.combine(ReactDOMRe.Style.make(~cursor="pointer", ()))

let road_shadow = ReactDOMRe.Style.make(~filter="url(#road-shadow)", ())

let rotate_card = rotation =>
  ReactDOMRe.Style.make(
    ~transitionDuration="0.5s",
    ~transitionProperty="transform",
    ~transform="rotateY(" ++ (rotation->string_of_int ++ "deg)"),
    (),
  )

let button_depth = 0.25

let filters =
  <defs>
    <filter id="shadow">
      <feDropShadow dx="0" dy="0" stdDeviation="0.25" floodColor="black" floodOpacity="0.5" />
    </filter>
    <filter id="button-shadow">
      <feDropShadow dx="0.05" dy="0.05" stdDeviation="0.075" floodColor="black" floodOpacity="1" />
    </filter>
    <filter id="text-shadow">
      <feDropShadow dx="0" dy="0" stdDeviation="0.2" floodColor="black" floodOpacity="0.2" />
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
    <filter x="0" y="0" width="1" height="1" id="text-background">
      <feFlood floodColor="lightgreen" /> <feComposite in_="SourceGraphic" operator="xor" />
    </filter>
  </defs>
