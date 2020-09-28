#' buildforfiles performs a build without %files section
#' and tries to contstruct all files from the build errors.
#' 
#' @param pkg Name of a package in a R repo like CRAN
#' @param pac directory for OBS
#' @param specfile Location of created specfile
#' @param rpmbuildroot The directory where rpmbuild should do its work
#' @param localOBSdir The top level directory of a checkout of the project
#' you want to create your package in. 
#' @param remoteprj Name of the OBS project
#' @export
buildforfiles <- function(pkg, pac, specfile, localOBS=getOption("c2o.localOBSdir"),
                          remoteprj=getOption("c2o.auto"), download.cache=getOption("c2o.download.cache"),
                          binary.cache=getOption("c2o.binary.cache"),
                          ap = getOption("c2o.status"),
                          log=getOption("c2o.logfile")){
    logger("** Build to construct files section")
    speclines <- readLines( specfile)
    result <- dropFileSection( speclines)
    
    if ( result$status != "done") {
        logger("dropping file section failed")
        return (list(status="failed", value=result$problem))
    }
    writeLines( result$speclines, specfile)
    
    result <- testbuild( pkg, pac, specfile, localOBS=localOBS, remoteprj=remoteprj, download.cache=download.cache,
                        binary.cache=binary.cache, ap=ap, log=log)
    
    ## we dont need to check for success, because %files section is empty and therefore fail by design
    
    if (! result$value == "unpackaged files"){
        logger(paste0( "buildforfiles: Failed to build files section for ", pkg))
        return(list(status="failed", problem=result$problem))
    }

    filelist <- extractFilesFromLog( result$buildlog, pkg)
    version <- gsub( "-", ".", ap[ap$Package == pkg, "Version"])
    gsub(version, "%{version}", filelist) # if there are files with version in name, this will be caught, see abcrlda
    
    speclines <- c( speclines , filelist)
    writeLines( speclines, specfile)
    return( list( status="done", value=specfile))
}

#' testbuild performs a testbuild and analyses the result
#' 
#' @param pkg Name of a package in a R repo like CRAN
#' @param specfile Location of created specfile
#' @param rpmbuildroot The directory where rpmbuild should do its work
#' @param localOBSdir The top level directory of a checkout of the project
#' you want to create your package in. 
#' @param remoteprj Name of the OBS project
#' @return list of status, value, buildlog
#' @export
testbuild <- function(pkg, pac, specfile,
                      localOBS=getOption("c2o.localOBSdir"),
                      remoteprj=getOption("c2o.auto"),
                      download.cache=getOption("c2o.download.cache"),
                      binary.cache=getOption("c2o.binary.cache"),
                      ap = getOption("c2o.status"),
                      log=getOption("c2o.logfile")){
    logger(paste0("** testbuild for pkg ", pkg))
    cmd <- paste0("\""," cd ", pac,
                 "; osc build --prefer-pkgs=", binary.cache, " --keep-pkgs=", binary.cache,
                 " --local-package --ccache ", specfile, "\"" )

    suppressWarnings(
        buildlog <- system2("bash", args=c("-c", cmd), stdout=TRUE, stderr=TRUE)
    )

    logger("after testbuild")
    
##    write(buildlog, file="oscbuildutf8")
    
    buildlog <- trimws( gsub( "^\\[.*\\]", "", buildlog), which="left")
    logger( buildlog, log)
                         
    
    if ( any ( grep( "ERROR: dependency", buildlog, fixed=TRUE)) ) { ### missing some dependency, no chance
        logger(paste0( "The following missing dependencies must be available to build R-", pkg, "\n", sep=""))

        for (line in buildlog[ grep( "ERROR: dependency", buildlog, fixed=TRUE)] ) {
            logger(line)
            logger("testbuild failed: Missing dependency")
        }
        return(list(status="failed", value="Missing dependencies in R CMD INSTALL", buildlog=buildlog))
    }
    
    if ( any ( grep( "nothing provides", buildlog, fixed=TRUE)) ) { ### missing R-rpms, no chance
        logger(paste0( "The following missing dependencies must be built first to build R-", pkg))

        for (line in buildlog[ grep( "nothing provides", buildlog, fixed=TRUE)] ) {
            logger( line)
            logger("testbuild Failed: Missing R-packages")
        }
        return(list(status="failed", value="missing R-packages", buildlog=buildlog))
    }
    
    if ( length( grep( "Failed build dependencies", buildlog, fixed=TRUE)) >0 ) { ### missing dependencies, no chance
        cat( "The following missing dependencies must be installed to build R-", pkg, "\n", sep="")
        cat( "The following missing dependencies must be installed to build R-", pkg, "\n", sep="", file=log, append=TRUE)
        if (any ( grep( "needed", buildlog, fixed=TRUE))) {
            for (line in buildlog[ grep( "needed", buildlog, fixed=TRUE)] ) {
                cat( line,"\n")
                cat( line,"\n", file=log, append=TRUE)
                cat("testbuild Failed: Missing dependencies\n", file=log, append=TRUE)
            }
        }
        return(list(status="failed", value="missing dependencies", buildlog=buildlog))
    }

    if ( any( grep("Bad exit status", buildlog, fixed=TRUE))) {
        cat( "Bad exit status from build R-", pkg, "\n", sep="")
        cat( "Bad exit status from build R-", pkg, "\n", sep="", file=log, append=TRUE)
        return(list(status="failed", value="bad exit status", buildlog=buildlog))
    }
    ## no fatal flaws, may be lucky?

    if ( any( grep( "error: Installed (but unpackaged) file(s) found:", buildlog, fixed=TRUE))) {
        cat( "Unpackaged file(s) in build R-", pkg, "\n", sep="")
        cat( "Unpackaged file(s) in build R-", pkg, "\n", sep="", file=log, append=TRUE)
        return(list(status="failed", value="unpackaged files", buildlog=buildlog)) 
    }
    
    if (length( grep( "Wrote:", buildlog, fixed=TRUE)) == 2) {
        cat("Success: ", pkg, " rpm package created\n")
        cat("Success: ", pkg, " rpm package created\n", file=log, append=TRUE)
        print(buildlog[grep("Wrote:", buildlog, fixed=TRUE)])
        for (line in  buildlog[grep("Wrote:", buildlog, fixed=TRUE)]){
            cat(line, "\n", file=log, append=TRUE)
        }
        return(list(status="done", value=NA, buildlog=buildlog))
    }
    ## if we end here some unknown condition has prevented a successful build
    cat( "Failed to build R-", pkg, " unknown problem\n", sep="")
    cat( "Failed to build R-", pkg, " unknown problem\n", sep="", file=log, append=TRUE)
    return(list(status="failed", value="unknown problem", buildlog=buildlog))    
}
