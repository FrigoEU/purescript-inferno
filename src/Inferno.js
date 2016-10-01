// module Inferno

var Inferno = require("Inferno");

exports.staticVElement = Inferno.createStaticVElement;
exports.createOptBlueprint = Inferno.createOptBlueprint;
exports.prop = function(str){
  return function(a){
    return [str, a];
  };
};
exports.props = function(props){
  var propObj = {};
  for (var i = 0; i < props.length; i++){
    propObj[props[i][0]] = propObj[props[i][1]];
  }
  return propObj;
};
export.render = function(inode, elem){
  return function(){
    Inferno.render(inode, elem);
  };
};
