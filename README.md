gpstime
---

A program to parse the time from `gpsd` output.

## Usage

    gpstime [options]
      -h <host>  --host=<host>  The GPSd host IP
      -p <port>  --port=<port>  The port GPSd is listening on
      -v         --version      Print version information
      -h         --help         Print this usage information


## What's it good for?

One might set up a cron job that sets the system time, like so:
```
0 0 3 * * root date -u -f "%FT%T" $(gpstime -h <ip of gpsd server>)
```

## Compiling

A working GHC is required.  It is probably easiest to install the haskell platform if you don't already have it.

These modules are required: `base >=4.5`, `bytestring`, `aeson`, `network`, `text`

To compile:

```
$ runhaskell Setup.hs configure
$ runhaskell Setup.hs build
```

The `gpstime` binary will be in `dist/build/gpstime/`

OR

```
$ cabal install
```

