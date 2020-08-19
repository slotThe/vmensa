# vmensa
Very simple program to connect to the [Studentenwerk
API](https://www.studentenwerk-dresden.de/mensen/speiseplan-api.html), fetch a
couple of different canteens, filter the results according to the authors
dietary requirements, and print out the relevant meals to stdout.

For a quick overview of all command line options, see `vmensa --help`.  For more
information, please see the man page.

# Building
It is recommended to build the project with `stack`, though there is a `cabal`
freeze-file available.

Build with `stack build`, then copy the executable to a convenient location.
Alternatively, you may use `stack install`, which will copy the executable to
`local-bin-path` (probably `$HOME/.local/bin`, see output of `stack path
--local-bin` for certainty).

There is also a small build script available (which takes case of installing the
man page as well), use at your own risk.  It expects the path where it should
store the `vmensa` executable as an input.  As an example,

```shell
./build ~/.local/bin
```

will symlink the executable to `~/.local/bin/vmensa`.
