// module Graphics.WebGL.Context

exports.getWebGLContextWithAttrs = function(canvas, attrs) {
	return function() {
		return canvas.getContext('webgl', attrs);
	};
};
