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

var fullscreenFunction = Element.prototype.requestFullscreen || Element.prototype.webkitRequestFullScreen || Element.prototype.mozRequestFullScreen || Element.prototpe.msRequestFullScreen;

exports.requestFullscreen = function(el) {
	return function() {
		fullscreenFunction.call(el);
	};
};

var pointerLockFunction = Element.prototype.requestPointerLock || Element.ptototype.webkitRequestPointerLock || Element.prototype.mozRequestPointerLock || Element.prototype.msRequestPointerLock;

exports.requestPointerLock = function(el) {
	return function() {
		pointerLockFunction.call(el);
	};
};

