# CRAN2OBS

This project tries to automate, as far as possible, the complete
mirroring of CRAN into OBS.

This is not even work-in-progress, it is just the beginning of some work.

## External Resources

- osc , interface to OBS
- R2spec , generates spec files from R packages description
  R2spec, resp. R2rpm use locally installed files to build the spec
  using a testbuild.
- rpmbuild , low level rpm build tool, used by R2spec

## Scripts

In scripts/ are shell scripts to help with automation

## R-Package

In a supplementary package is explore if a replacement of external
scripts with an R package is possible.

Furthermore such a package should help to install R-*.rpm packages
automatically as an enhancemnt to {install|update|etc.}.packages().