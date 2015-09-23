// module DOM.Event.KeyboardEvent

exports.keyCode = function(event) {
  return event.keyCode;
};

exports.keyboardEventListener = function (fn) {
  return function (event) {
    return fn(event)();
  };
};

exports.addKeyboardEventListenerImpl = function (type) {
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
