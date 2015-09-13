// module Graphics.Canvas.Extra

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
