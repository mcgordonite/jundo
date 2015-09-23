// module DOM.Event.Experimental

var fullscreenChangeEvent = 'fullscreenchange';

// Use the requestFullScreen function to guess if we need to use a browser-specific prefix
if (Element.prototype.webkitRequestFullScreen) {
  fullscreenChangeEvent = 'webkitfullscreenchange';
} else if (Element.prototype.mozRequestFullScreen) {
  fullscreenChangeEvent = 'mozfullscreenchange';
} else {
  fullscreenChangeEvent = 'msfullscreenchange';
}

exports.fullscreenChangeEvent = fullscreenChangeEvent;

