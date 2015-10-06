// module DOM.Node.Element.Experimental

exports.clientHeight = function(el) {
  return function() {
    return el.clientHeight;
  };
};

exports.clientWidth = function(el) {
  return function() {
    return el.clientWidth;
  };
};

exports.requestFullscreen = function(el) {
  return function() {
    var fullscreenFunction = el.requestFullscreen || el.webkitRequestFullScreen || el.mozRequestFullScreen || el.msRequestFullScreen;
    fullscreenFunction.call(el);
    return {};
  };
};

exports.requestPointerLock = function(el) {
  return function() {
    var pointerLockFunction = el.requestPointerLock || el.webkitRequestPointerLock || el.mozRequestPointerLock || el.msRequestPointerLock;
    pointerLockFunction.call(el);
    return {};
  };
};

