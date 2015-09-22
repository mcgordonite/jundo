// module DOM.Event.MouseEvent

exports.movementX = function(event) {
	return function() {
		return event.movementX;
	};
};

exports.movementY = function(event) {
	return function() {
		return event.movementY;
	};
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
