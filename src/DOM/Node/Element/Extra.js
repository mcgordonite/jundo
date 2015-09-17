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

exports.requestFullscreen = function(el) {
	var requestFullscreen = el.requestFullscreen || el.mozRequestFullScreen || el.webkitRequestFullscreen || el.msRequestFullscreen;

	return function() {
		requestFullscreen.call(el);
	};
};

