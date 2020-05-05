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

External dependencies
---------------------

- rpmbuild, rpmbuild requires the directories
  $RPMBULDROOT/{SOURCES, SPECS, BUILD, BUILDROOT, OTHER, RPMS, SRPMS}
  to exist

Intended use
------------

A version 1.0 should present users of flavours of SUSE Linux, either OpenSUSE
or SLES, a seamless integration of packages present in OBS and packages
in CRAN, Bioconductor, or even in github.

Already implemented
-------------------

For a start we implemented a function showOBSstatus, giving
for each package in OBS current version and version in CRAN.

Second a function desc2spec which generates a rpm and therefore a
working spec file if all dependencies are fulfilled.

Implementation decisions
------------------------

## desc2spec
- If a package specifies a URL, should that be honoured?
  At the moment we use a generic URL
- We decided to expand the dependencies to include all recursive
  dependencies.
  Those must be installed anyway, so the longer list is not really
  longer, but makes invisible dependencies visible. Therefore, if
  you work an a new package with a lot of dependencies, you know from the
  beginning, not only if you try to build these dependencies piece by piece.
- Requires: R-base, BuildRequires: R-base-devel are always addded
- Still to decide: Remove the 'Group:' tag?


### Author

Detlef Steuer

### License

GPL (&gt;= 2)