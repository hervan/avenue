'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");

function Avenue(Props) {
  var match = React.useState((function () {
          return false;
        }));
  var setEditing = match[1];
  var match$1 = React.useState((function () {
          return "#FFFF00";
        }));
  var onChange = match$1[1];
  var color = match$1[0];
  var onCancel = function (_evt) {
    return Curry._1(setEditing, (function (param) {
                  return false;
                }));
  };
  var onFocus = function ($$event) {
    return $$event.target.select();
  };
  return React.createElement("div", undefined, React.createElement("svg", {
                  height: "100px",
                  width: "100px",
                  version: "1.1",
                  viewBox: "0 0 100 100"
                }, React.createElement("title", undefined, "My Ordinary SVG"), React.createElement("circle", {
                      onDoubleClick: (function (_evt) {
                          return Curry._1(setEditing, (function (param) {
                                        return true;
                                      }));
                        }),
                      cx: "50",
                      cy: "50",
                      fill: color,
                      r: "50"
                    })), match[0] ? React.createElement("form", {
                    onBlur: onCancel,
                    onSubmit: (function (param) {
                        return Curry._1(setEditing, (function (param) {
                                      return false;
                                    }));
                      })
                  }, React.createElement("input", {
                        value: color,
                        onFocus: onFocus,
                        onBlur: onCancel,
                        onChange: (function ($$event) {
                            return Curry._1(onChange, $$event.target.value);
                          })
                      })) : null);
}

var make = Avenue;

exports.make = make;
/* react Not a pure module */
