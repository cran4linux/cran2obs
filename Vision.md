# My vision for R in openSUSE/SLE

## Current infrastructure
### R2spec/R2rpm
May be use R-core instead of R-base for building rpms.
Should be (a bit) faster.
    
### rpmbuild
    
## Vision
In my vision the most useful setting for R usage in openSUSE would
include an opensuse.install.packages() that first tries to install 
from d:l:R:released (or a dedicated repo) and only, if not found,
downloads from CRAN.
May be even a check of available versions included.

The same should work for special versions of available.packages(),
update/uninstall etc.

Installation could happen vis sudo or even play around with relocatable
packages into ~/.R/lib for simple users.

Leap would ship R, rstudio, rkward, all packages would come from
d:l:R:released. To be useful d:l:R:released must be populated and be
kept up-to-date automatically. Not a small task, but may be a possible
one.

If R-* packages are in Factory, they should be in Maintenance, too.
But I never managed to get even R-base into Maintenance. And that would
be really useful!

For installation of packages a format like tex-packages ""zypper in 'tex(multirow.sty)'" could be possible: zypper install 'R(jsonlite)' 
