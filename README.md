# 純度 (purity)

An experiment with functional programming and 3D graphics using [PureScript](http://www.purescript.org/) and WebGL.

A live build can be found [here](http://mcgordonite.github.io/jundo).

## About

I want to learn about functional programming, and I want to learn about 3D graphics. Why not do both, and AWESOME, at the
same time?

As this is a hobby project, I don't want to get bogged down in browser compatibility issues. For example, I assume the
browser supports "standard" WebGL and make no attempt to deal with the case that it doesn't. However, the intention is
that error cases such as context loss should be handled.

## Building

This project is built using PureScript (version 0.7.4.1) and the pulp build tool (version 4.4.0):

	npm install -g pulp

Then a development server can be started using:

	pulp server

On Windows, I have to specify the main module as:

	pulp server -m .//Main

## Disclaimer

This is my first purely functional project, and my first experiment with WebGL. It probably doesn't reflect best practices,
but it seems to work...
