CRAN2OBS
========

An R package to bridge CRAN and OBS (https://build.opensuse.org)

Installation
------------

For the moment there is only a very early version available on
github. You can install with

``` r
# install.packages("devtools")
devtools::install_github("dsteuer/CRAN2OBS")
```

Intended use
------------

A version 1.0 should present users of flavours of SUSE Linux, either OpenSUSE
or SLES, a seamless integration of packages present in OBS and packages
in CRAN or even in github.

Already implemented
-------------------

For a start we implemented a function showOBSstatus, giving
for each package in OBS current version and version in CRAN.

### Author

Detlef Steuer

### License

GPL (&gt;= 2)