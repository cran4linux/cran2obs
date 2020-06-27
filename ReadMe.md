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
  and by osc

## Scripts

In scripts/ are shell scripts to help with automation

## R-Package

In a supplementary package is explore if a replacement of external
scripts with an R package is possible.

Furthermore such a package should help to install R-*.rpm packages
automatically as an enhancemnt to {install|update|etc.}.packages().

## Goal

At any moment it must be possible to start with a local version
of CRAN and catch up to the status of CRANinOBS and CRAN itself.
We try to minimise network traffic with OBS.

During the operation each package has three version
CRAN-version LocalOBSVErsion RemoteOBSVersion (costly to retrieve)
(May be a fourth state in d:l:R:r?)

We pay with increased storage use locally. All of remote CRAN will
finally be on the local machine, if built from scratch.

## Developter Notes
### Update Workflow



read.table(oldstatus, file="CRANinOBSStatus.csv", sep=";",row.names=FALSE, header=TRUE)
> names (oldstatus)
[1] "Package"          "Version"          "License"          "NeedsCompilation" "recDep"           "depLen"           "OBSpkg"          
[8] "File"             "OBSVersion"      
     cap <- as.data.frame(available.packages())
> names(cap)
 [1] "Package"               "Version"               "Priority"              "Depends"               "Imports"              
 [6] "LinkingTo"             "Suggests"              "Enhances"              "License"               "License_is_FOSS"      
[11] "License_restricts_use" "OS_type"               "Archs"                 "MD5sum"                "NeedsCompilation"     
[16] "File"                  "Repository"

     newall <- merge(cap[, !(names(cap) %in% c("Priority", "Enhances", "License_is_FOSS", "License_restricts_use", "OS_type", "Archs", "MD5sum", "File", "Repository")) ], oldstatus[, c("Package", "recDep", "depLen","OBSpkg","File", "OBSVersion"  )], by="Package", all.x=TRUE)

     write.table(oldstatus, file="CRANinOBSStatus.csv", sep=";",row.names=FALSE)