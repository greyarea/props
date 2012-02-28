# props - Property structure library for Erlang

This is a library for manipulating JSON like structures, called props.

## Building

Note that because rebar applies compiler options to downstream
dependencies in some cases, you'll need to build neotoma manually
with:

    cd deps/neotoma && rebar compile
    
Or else the spec errors (which we want on props itself) will prevent
the build from completing.

## License

props is licensed under the Apache 2.0 license.
