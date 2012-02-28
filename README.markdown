# props - Property structure library for Erlang

This is a library for manipulating JSON like structures, called props.

## Using

To include props in your project, add the following to your `deps`
section in `rebar.config`:

    {props, ".*", {git, "https://github.com/greyarea/props.git", "master"}}

## Building

props uses rebar controlled by a Makefile for building the code.

`make depends` will grab the dependencies and `make` will build props.

## License

props is licensed under the Apache 2.0 license.
