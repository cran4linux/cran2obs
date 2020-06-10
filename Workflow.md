
* Usage of CRAN2OBS

This package has one goal: As automatically as possible sync
CRAN (cran.r-project.org) to OBS (build.opensuse.org).

At the moment it is in experimental status and pushes successfully
built packages to PBS project home:dsteuer:AutomaticCRAN.

Experiments show, that around 40% of CRAN can be imported to CRAN easily.
These are the packages with only very few dependencies and without
complicated external dependencies. Complicated in this sense means
only the fundamentel compilers gcc, g++, and gfortran are needed.

In this document I want to show the workflow for the main focus of the package
and for the secondary usage to build single packages with it. This second
usage replaces R2Spec, which was the standard tool to add packages to OBS.

** How to sync CRAN to OBS
(Work in progress.)

** How to build a new package with CRAN2OBS
There are two different ways to generate a prinstine spec file
for a package not in OBS so far.

*** Recommended: createOBSpac
This newer way uses 'osc' to build and check newly created packages.

*** More traditional: desc2spec
This traditional tries to mimic R2spec in R itself. It uses 'rpmbuild'
to build and check newly created packages.