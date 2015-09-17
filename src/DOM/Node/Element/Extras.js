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

