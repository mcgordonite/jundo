// module DOM.Event.MouseEvent

exports.movementX = function(event) {
  return event.movementX;
};

exports.movementY = function(event) {
  return event.movementY;
};

exports.mouseEventListener = function (fn) {
  return function (event) {
    return fn(event)();
  };
};

exports.addMouseEventListenerImpl = function (type) {
  return function (listener) {
    return function (useCapture) {
      return function (target) {
        return function () {
          target.addEventListener(type, listener, useCapture);
          return {};
        };
      };
    };
  };
};

exports.removeMouseEventListenerImpl = function (type) {
  return function (listener) {
    return function (useCapture) {
      return function (target) {
        return function () {
          target.removeEventListener(type, listener, useCapture);
          return {};
        };
      };
    };
  };
};
