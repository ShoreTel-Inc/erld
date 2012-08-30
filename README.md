ErlD
====

See the [github wiki](https://github.com/ShoreTel-Inc/erld/wiki) for
documentation.  You can also check out the example code under the
`example` directory and run `erld -h` for an explanation of
parameters.

To build and install from a clean checkout:

        ./bootstrap.sh
        ./configure
        make
        sudo make install

REQUIREMENTS
------------

erld doesn't require much to build. In addition to Erlang, you will
need libproc and its development files (headers etc) and that should
be all (aside from standard stuff like a compiler and autotools which
pretty much any developer will already have installed).

There are no specific version restrictions on Erlang that we're aware
of - we've been running this code since R13 and are still using it
without signficant modification on R15. If you do encounter a problem
with a particular version, please let us know.
