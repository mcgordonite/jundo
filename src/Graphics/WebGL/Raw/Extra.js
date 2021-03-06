// module Graphics.WebGL.Raw.Extra

exports.getCanvas = function(gl) {
  return function() {
    return gl.canvas;
  };
};

exports.getDrawingBufferWidth = function(gl) {
  return function() {
    return gl.drawingBufferWidth;
  };
};

exports.getDrawingBufferHeight = function(gl) {
  return function() {
    return gl.drawingBufferHeight;
  };
};

exports.bufferData = function(gl) {
  return function(target) {
    return function(data) {
      return function(usage) {
        return function() {
          gl.bufferData(target, data, usage);
        };
      };
    };
  };
};
