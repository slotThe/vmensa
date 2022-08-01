# vmensa

Very simple program to connect to the [Studentenwerk API], fetch a
couple of different canteens, filter the results according to the
authors dietary requirements, and print out the relevant meals to
stdout.

![example image of a query](./example.png)

For a quick overview of all command line options, see `vmensa --help`.
For more information, please consult the man page.

# Building

It is recommended to build the project with `stack`, though building
with `cabal` should also work.

Build with `stack build`, then copy the executable to a convenient
location.  Alternatively, you may use `stack install`, which will copy
the executable to `local-bin-path` (probably `$HOME/.local/bin`, see
output of `stack path --local-bin` for certainty).

There is also a small build script available, which takes case of
installing the man page as well; use at your own risk.  It expects the
path where it should move the executable to as an input.  For example,

``` console
  $ ./build ~/.local/bin/vmensa
```

will symlink the executable to `~/.local/bin/vmensa`.

[Studentenwerk API]: https://www.studentenwerk-dresden.de/mensen/speiseplan-api.html

# Examples

What follows are some examples, in increasing complexity, to help you
get a feel for what's possible.

``` console
$ vmensa

$ vmensa --mensen siedepunkt --wrap 62 --time lunch

$ vmensa --diet vegan --no-additives --sections name,notes --ikat Suppe

$ vmensa --no-additives --columns 4 --wrap 50 --sections name,price,notes \
> --ikat Suppe,Pasta,Terrine --mensen wu,siede,zelt,alte,bio
```

The output of the last command will be something along the lines of

![2022-08-01-151408_1499x662_scrot](https://user-images.githubusercontent.com/50166980/182156000-17fdec3e-86d6-40f7-bb7b-be7376f05024.png)

For some more examples, refer to the man page.
