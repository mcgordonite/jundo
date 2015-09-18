// module DOM.Node.Document.Extra

var fullscreenPrefix = 'fullscreen';

if (Element.prototype.webkitRequestFullscreen !== undefined) {
	fullscreenPrefix = 'webkitFullscreen';
} else if (Element.prototype.mozRequestFullscreen !== undefined) {
	fullscreenPrefix = 'mozFullscreen';
} else if (Element.prototype.msRequestFullscreen !== undefined) {
	fullscreenPrefix = 'msFullscreen';
}

exports.fullscreenEnabled = function(doc) {
	return function() {
		return doc[fullscreenPrefix + 'Enabled'];
	};
};

exports.fullscreenElement = function(doc) {
	return function() {
		return doc[fullscreenPrefix + 'Element';
	};
};

var exitPointerLockFunction = Document.prototype.exitPointerLock || Document.prototype.webkitExitPointerLock || Document.prototype.mozExitPointerLock || Document.prototype.msExitPointerLock;

exports.exitPointerLock = function(doc) {
	return function() {
		exitPointerLockFunction.call(doc);
	};
};

var pointerLockPrefix = 'pointerLock';

if (Document.prototype.webkitPointerLockElement !== undefined) {
	pointerLockPrefix = 'webkitPointerLock';
} else if (Document.prototype.mozPointerLockElement !== undefined) {
	pointerLockPrefix = 'mozPointerLock';
} else if (Document.prototype.msPointerLockElement !== undefined) {
	pointerLockPrefix = 'msPointerLock';
}

exports.pointerLockElement = function(doc) {
	return function() {
		return doc[pointerLockPrefix + 'Element';
	};
};

