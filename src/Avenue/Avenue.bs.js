'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var React = require("react");
var Random = require("bs-platform/lib/js/random.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");

function str(prim) {
  return prim;
}

var grid_contents = [
  [
    /* Grapes */Block.__(0, [/* :: */[
          /* Green */1,
          /* :: */[
            /* Green */1,
            /* :: */[
              /* Green */1,
              /* :: */[
                /* Red */0,
                /* [] */0
              ]
            ]
          ]
        ]]),
    /* Grapes */Block.__(0, [/* :: */[
          /* Red */0,
          /* [] */0
        ]]),
    /* Farm */Block.__(2, [/* A */0]),
    /* Grapes */Block.__(0, [/* :: */[
          /* Green */1,
          /* :: */[
            /* Green */1,
            /* [] */0
          ]
        ]]),
    /* Empty */0,
    /* Castle */Block.__(1, [/* Green */1])
  ],
  [
    /* Grapes */Block.__(0, [/* :: */[
          /* Red */0,
          /* [] */0
        ]]),
    /* Grapes */Block.__(0, [/* :: */[
          /* Green */1,
          /* :: */[
            /* Green */1,
            /* [] */0
          ]
        ]]),
    /* Grapes */Block.__(0, [/* :: */[
          /* Green */1,
          /* [] */0
        ]]),
    /* Grapes */Block.__(0, [/* :: */[
          /* Red */0,
          /* [] */0
        ]]),
    /* Grapes */Block.__(0, [/* :: */[
          /* Green */1,
          /* :: */[
            /* Green */1,
            /* :: */[
              /* Red */0,
              /* [] */0
            ]
          ]
        ]]),
    /* Empty */0
  ],
  [
    /* Grapes */Block.__(0, [/* :: */[
          /* Green */1,
          /* [] */0
        ]]),
    /* Empty */0,
    /* Grapes */Block.__(0, [/* :: */[
          /* Red */0,
          /* :: */[
            /* Red */0,
            /* :: */[
              /* Green */1,
              /* [] */0
            ]
          ]
        ]]),
    /* Farm */Block.__(2, [/* B */1]),
    /* Grapes */Block.__(0, [/* :: */[
          /* Green */1,
          /* [] */0
        ]]),
    /* Grapes */Block.__(0, [/* :: */[
          /* Green */1,
          /* :: */[
            /* Green */1,
            /* [] */0
          ]
        ]])
  ],
  [
    /* Farm */Block.__(2, [/* C */2]),
    /* Grapes */Block.__(0, [/* :: */[
          /* Red */0,
          /* :: */[
            /* Red */0,
            /* [] */0
          ]
        ]]),
    /* Grapes */Block.__(0, [/* :: */[
          /* Green */1,
          /* :: */[
            /* Green */1,
            /* [] */0
          ]
        ]]),
    /* Empty */0,
    /* Grapes */Block.__(0, [/* :: */[
          /* Red */0,
          /* :: */[
            /* Red */0,
            /* [] */0
          ]
        ]]),
    /* Farm */Block.__(2, [/* D */3])
  ],
  [
    /* Grapes */Block.__(0, [/* :: */[
          /* Red */0,
          /* :: */[
            /* Red */0,
            /* [] */0
          ]
        ]]),
    /* Empty */0,
    /* Farm */Block.__(2, [/* E */4]),
    /* Grapes */Block.__(0, [/* :: */[
          /* Red */0,
          /* [] */0
        ]]),
    /* Grapes */Block.__(0, [/* :: */[
          /* Green */1,
          /* :: */[
            /* Green */1,
            /* :: */[
              /* Red */0,
              /* [] */0
            ]
          ]
        ]]),
    /* Grapes */Block.__(0, [/* :: */[
          /* Red */0,
          /* [] */0
        ]])
  ],
  [
    /* Empty */0,
    /* Grapes */Block.__(0, [/* :: */[
          /* Red */0,
          /* :: */[
            /* Red */0,
            /* :: */[
              /* Green */1,
              /* [] */0
            ]
          ]
        ]]),
    /* Grapes */Block.__(0, [/* :: */[
          /* Green */1,
          /* [] */0
        ]]),
    /* Grapes */Block.__(0, [/* :: */[
          /* Red */0,
          /* :: */[
            /* Red */0,
            /* [] */0
          ]
        ]]),
    /* Grapes */Block.__(0, [/* :: */[
          /* Green */1,
          /* [] */0
        ]]),
    /* Empty */0
  ],
  [
    /* Castle */Block.__(1, [/* Red */0]),
    /* Grapes */Block.__(0, [/* :: */[
          /* Green */1,
          /* [] */0
        ]]),
    /* Grapes */Block.__(0, [/* :: */[
          /* Red */0,
          /* [] */0
        ]]),
    /* Farm */Block.__(2, [/* F */5]),
    /* Empty */0,
    /* Grapes */Block.__(0, [/* :: */[
          /* Red */0,
          /* :: */[
            /* Red */0,
            /* :: */[
              /* Red */0,
              /* :: */[
                /* Green */1,
                /* [] */0
              ]
            ]
          ]
        ]])
  ]
];

function create_player(player_name, base_grid) {
  return {
          farmer: player_name,
          grid: base_grid,
          lookahead: false,
          round: 0
        };
}

function farm_of_int(param) {
  if (param > 4 || param < 0) {
    return /* F */5;
  } else {
    return param;
  }
}

function random_farm(param) {
  return farm_of_int(Random.$$int(6));
}

function create_farms_deck(param) {
  var _deck = /* [] */0;
  var _n = 6;
  while(true) {
    var n = _n;
    var deck = _deck;
    if (n !== 0) {
      var farm_card = farm_of_int(Random.$$int(6));
      if (List.for_all((function(farm_card){
            return function (card) {
              return card !== farm_card;
            }
            }(farm_card)), deck)) {
        _n = n - 1 | 0;
        _deck = /* :: */[
          farm_card,
          deck
        ];
        continue ;
      } else {
        continue ;
      }
    } else {
      return deck;
    }
  };
}

function stretch_of_int(param) {
  switch (param) {
    case 0 :
        return /* tuple */[
                /* Top */0,
                /* Left */3
              ];
    case 1 :
        return /* tuple */[
                /* Top */0,
                /* Right */1
              ];
    case 2 :
        return /* tuple */[
                /* Right */1,
                /* Bottom */2
              ];
    case 3 :
        return /* tuple */[
                /* Left */3,
                /* Bottom */2
              ];
    case 4 :
        return /* tuple */[
                /* Left */3,
                /* Right */1
              ];
    default:
      return /* tuple */[
              /* Top */0,
              /* Bottom */2
            ];
  }
}

function card_color_of_int(param) {
  if (param !== 0) {
    return /* Grey */0;
  } else {
    return /* Yellow */1;
  }
}

function stretch_card_of_ints(stretch, color) {
  return /* tuple */[
          stretch_of_int(stretch),
          card_color_of_int(color)
        ];
}

function create_paths_deck(param) {
  var _deck = /* [] */0;
  var available_cards = [
    [
      4,
      3
    ],
    [
      4,
      3
    ],
    [
      4,
      3
    ],
    [
      4,
      3
    ],
    [
      3,
      4
    ],
    [
      3,
      4
    ]
  ];
  while(true) {
    var deck = _deck;
    var stretch = Random.$$int(6);
    var color = Random.$$int(2);
    if (List.length(deck) === 42) {
      return deck;
    } else if (Caml_array.caml_array_get(Caml_array.caml_array_get(available_cards, stretch), color) === 0) {
      continue ;
    } else {
      Caml_array.caml_array_set(Caml_array.caml_array_get(available_cards, stretch), color, Caml_array.caml_array_get(Caml_array.caml_array_get(available_cards, stretch), color) - 1 | 0);
      _deck = /* :: */[
        stretch_card_of_ints(stretch, color),
        deck
      ];
      continue ;
    }
  };
}

function create_base_grid(param) {
  return $$Array.init(7, (function (row) {
                return $$Array.init(6, (function (col) {
                              return {
                                      row: row,
                                      col: col,
                                      content: Caml_array.caml_array_get(Caml_array.caml_array_get(grid_contents, row), col),
                                      stretch: undefined
                                    };
                            }));
              }));
}

var base_grid = create_base_grid(/* () */0);

function create_game(player_name) {
  return {
          players: /* :: */[
            create_player(player_name, base_grid),
            /* [] */0
          ],
          deck: create_paths_deck(/* () */0),
          stage: /* Begin */0,
          phase_deck: create_farms_deck(/* () */0),
          yellow_cards: 0,
          base_grid: base_grid
        };
}

function Avenue(Props) {
  Random.self_init(/* () */0);
  return React.createElement("svg", {
              height: "100vmin",
              width: "100vmin",
              viewBox: "0 0 100 100"
            }, React.createElement("title", undefined, "avenue"), React.createElement("line", {
                  stroke: "black",
                  strokeWidth: "0.1",
                  x1: "5",
                  x2: "95",
                  y1: "5",
                  y2: "95"
                }));
}

var grid_columns = 6;

var grid_rows = 7;

var make = Avenue;

exports.str = str;
exports.grid_columns = grid_columns;
exports.grid_rows = grid_rows;
exports.grid_contents = grid_contents;
exports.create_player = create_player;
exports.farm_of_int = farm_of_int;
exports.random_farm = random_farm;
exports.create_farms_deck = create_farms_deck;
exports.stretch_of_int = stretch_of_int;
exports.card_color_of_int = card_color_of_int;
exports.stretch_card_of_ints = stretch_card_of_ints;
exports.create_paths_deck = create_paths_deck;
exports.create_base_grid = create_base_grid;
exports.base_grid = base_grid;
exports.create_game = create_game;
exports.make = make;
/* base_grid Not a pure module */
