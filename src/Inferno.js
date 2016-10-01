// module Inferno

var Inferno = require("inferno");
var InfernoDOM = require("inferno-dom");

exports._staticVElement = Inferno.createStaticVElement;
exports.createOptBlueprint = Inferno.createOptBlueprint;
exports.prop = function(str){
  return function(a){
    return [str, a];
  };
};
exports.props = function(props){
  var propObj = {};
  for (var i = 0; i < props.length; i++){
    propObj[props[i][0]] = props[i][1];
  }
  return propObj;
};
exports.render = function(inode){
  return function(elem){
    return function(){
      InfernoDOM.render(inode, elem);
    };
  };
};
