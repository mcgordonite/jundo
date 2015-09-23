// module DOM.Node.Document.Experimental

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
    return doc[fullscreenPrefix + 'Element'];
  };
};

var exitPointerLockFunction = Document.prototype.exitPointerLock || Document.prototype.webkitExitPointerLock || Document.prototype.mozExitPointerLock || Document.prototype.msExitPointerLock;

exports.exitPointerLock = function(doc) {
  return function() {
    exitPointerLockFunction.call(doc);
    return {};
  };
};

var pointerLockElementProperty = 'pointerLockElement';

if (Document.prototype.webkitPointerLockElement !== undefined) {
  pointerLockElementProperty = 'webkitPointerLockElement';
} else if (Document.prototype.mozPointerLockElement !== undefined) {
  pointerLockElementProperty = 'mozPointerLockElement';
} else if (Document.prototype.msPointerLockElement !== undefined) {
  pointerLockElementProperty = 'msPointerLockElement';
}

exports.pointerLockElement = function(doc) {
  return function() {
    return doc[pointerLockElementProperty];
  };
};

