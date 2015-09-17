// module DOM.Node.Document.Extra

exports.fullscreenEnabled = function(doc) {
	return function() {
		if (doc.fullscreenEnabled !== undefined) {
			return doc.fullscreenEnabled;
		} else if (doc.webkitFullscreenEnabled !== undefined) {
			return doc.webkitFullscreenEnabled;
		} else if (doc.mozFullscreenEnabled !== undefined) {
			return doc.mozFullscreenEnabled;
		} else {
			return doc.msFullscreenEnabled;
		}
	};
};

exports.fullscreenElement = function(doc) {
	return function() {
		if (doc.fullscreenElement !== undefined) {
			return doc.fullscreenElement;
		} else if (doc.webkitFullscreenEnabled !== undefined) {
			return doc.webkitFullscreenEnabled;
		} else if (doc.mozFullscreenEnabled !== undefined) {
			return doc.mozFullscreenEnabled;
		} else {
			return doc.msFullscreenEnabled;
		}
	};
};

