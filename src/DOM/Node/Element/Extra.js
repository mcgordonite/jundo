// module DOM.Node.Element.Extra

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

