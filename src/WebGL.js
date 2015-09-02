// module WebGL

exports.createBufferImpl = function(gl) {
	return gl.createBuffer();
};


// WARNING: This module becomes uncool from this point on

exports.unsafeGetContext = function(canvasId) {
	return document.getElementById(canvasId).getContext('webgl');
};

exports.logBuffer = function(buffer) {
	return function() {
		console.log(buffer);
		return {};
	};
};
