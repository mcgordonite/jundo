// module DOM.Event.Experimental

var fullscreenChangeEvent = 'fullscreenchange';

// Use the requestFullScreen function to guess if we need to use a browser-specific prefix
if (Element.prototype.webkitRequestFullScreen) {
  fullscreenChangeEvent = 'webkitfullscreenchange';
} else if (Element.prototype.mozRequestFullScreen) {
  fullscreenChangeEvent = 'mozfullscreenchange';
} else if (Element.prototype.msRequestFullScreen) {
  fullscreenChangeEvent = 'msfullscreenchange';
}

exports.fullscreenChangeEvent = fullscreenChangeEvent;

var pointerLockChangeName = 'pointerlockchange';

// Use the requestPointerLock function to guess if we need to use a browser-specific prefix
if (Element.prototype.webkitRequestPointerLock) {
  pointerLockChangeName = 'webkitpointerlockchange';
} else if (Element.prototype.mozRequestPointerLock) {
  pointerLockChangeName = 'mozpointerlockchange';
} else if (Element.prototype.msRequestPointerLock) {
  pointerLockChangeName = 'mspointerlockchange';
}

exports.pointerLockChangeName = pointerLockChangeName;
