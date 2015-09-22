// module Graphics.WebGL.Context

exports.getWebGLContextWithAttrs = function(canvas) {
	return function(attrs) {
		return function() {
			return canvas.getContext('webgl', attrs);
		};
	};
};
