# ?? (purity)

An experiment with purely functional programming and 3D graphics using PureScript and WebGL.

## Building

This project is built against PureScript versions greater than 0.7 using the pulp build tool:

	npm install -g pulp

Then a development server can be started using:

	pulp server -m .//Main

## Disclaimer

This is my first purely functional project, and my first experiment with WebGL. It probably doesn't reflect best practices!

## To do

+ Handle context loss.
+ Add more specific enum types (eg: WebGL error codes) to replace generic Graphics.WebGL.Raw.Enums.
+ Replace matrix library with something backed by Float32Arrays rather than plain-old-arrays.
