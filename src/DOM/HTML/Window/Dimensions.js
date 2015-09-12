// module DOM.HTML.Window.Dimensions

exports.innerWidth = function(w) {
	return function() {
		return w.innerWidth;
	};
};

exports.innerHeight = function(w) {
	return function() {
		return w.innerHeight;
	};
};
