# vmensa
Very simple program to connect to the [studentenwerk
api](https://www.studentenwerk-dresden.de/mensen/speiseplan-api.html), fetch a
couple of different canteens, filter the results according to the authors
dietary requirements, and print out the relevant meals to stdout.

For all command line options, see `vmensa --help`.  For more information, please
see the man page.

# Building
Build with `stack build`, then copy the executable to a convenient location.
Alternatively, you may use `stack install`, which will copy the executable to
`local-bin-path` (probably `$HOME/.local/bin`, see output of `stack path
--local-bin` for certainty).

There's also a small build script available (which takes case of installing the
man page as well), use at your own risk.  You may use it as follows:
```shell
./build /path/of/executable
```

Note that if you want to query the different canteens concurrently you will have
to explicitly tell GHC to use a threaded runtime.  You can do this by executing
`vmensa +RTS -N -RTS [OPTIONS]` or, alternatively, `vmensa [OPTIONS] +RTS -N`.
This way the runtime will set the number of threads itself.  You may also
specify how many threads to use by explicitly telling the `-N` option,
i.e. `-N2` (usually this should be the number of cores you have).
