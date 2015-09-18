// module DOM.Event.Extra

// Hackily determine if we need an event prefix
var prefix;

if (Element.prototype.requestFullscreen) {
	prefix = '';
} else if (Element.prototype.webkitRequestFullScreen) {
	prefix = 'webkit';
} else if (Element.prototype.mozRequestFullScreen) {
	prefix = 'moz';
} else {
	prefix = 'ms';
}

exports.fullscreenChangeName = prefix + 'fullscreenchange';

