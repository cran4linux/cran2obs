
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
#' @param build.clean default FALSE, wheter obs should be called with --clean
#'
#' @return
#'
#' @export
buildforfiles <- function(pkg, pac, specfile, localOBS=getOption("c2o.localOBSdir"),
                          remoteprj=getOption("c2o.auto"), download.cache=getOption("c2o.download.cache"),
                          binary.cache=getOption("c2o.binary.cache"),
                          ap = getOption("c2o.status"),
                          log=getOption("c2o.logfile"),
                          build.clean=FALSE){
    logger("** Build to construct files section")
    speclines <- readLines( specfile)
    result <- dropFileSection( speclines) ### only needed if manual constructed spec is used
    
    if ( result$status != "done") {
        logger("dropping file section failed")
        return (list(status="failed", value=result$problem))
    }
    writeLines( result$speclines, specfile)
    
    result <- testbuild( pkg, pac, specfile, localOBS=localOBS, remoteprj=remoteprj, download.cache=download.cache,
                        binary.cache=binary.cache, ap=ap, log=log, build.clean=build.clean)
    
    ## we dont need to check for success, because %files section is empty and therefore fail by design
    
    if (! result$value == "unpackaged files"){ ## every other error is fatal at this point
        logger(paste0( "Failed initial build of R-", pkg, " Error was: ", result$value))
        return(list(status="failed", value=result$value))
    }

    filelist <- extractFilesFromLog( result$buildlog, pkg)
    version <- gsub( "-", ".", ap[ap$Package == pkg, "Version"])
    gsub(version, "%{version}", filelist) # if there are files with version in name, this will be caught, see abcrlda
    
    speclines <- c( speclines[1:(length(speclines)-2)] , filelist, speclines[(length(speclines)-1):length(speclines)])
    ## the last two lines contain the %changelog tag
    
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
#' @param build.clean default FALSE, wheter obs should be called with --clean
#'
#' @return list of status, value, buildlog
#' @export
testbuild <- function(pkg, pac, specfile,
                      localOBS=getOption("c2o.localOBSdir"),
                      remoteprj=getOption("c2o.auto"),
                      download.cache=getOption("c2o.download.cache"),
                      binary.cache=getOption("c2o.binary.cache"),
                      ap = getOption("c2o.status"),
                      log=getOption("c2o.logfile"),
                      build.clean = FALSE){
    logger(paste0("** testbuild for pkg ", pkg))

    cmd <- paste0("\""," cd ", pac,
                  "; osc build ",  if(build.clean) " --clean " else "" ,
                  " --prefer-pkgs=", binary.cache, " --keep-pkgs=", binary.cache,
                  " --local-package ", specfile, "\"" )

    suppressWarnings(
        buildlog <- system2("bash", args=c("-c", cmd), stdout=TRUE, stderr=TRUE)
    )

##    logger(buildlog, log)
##    writeLines(buildlog, "./buildlog")
    
    buildlog <- cleanBuildlog(buildlog) ## lots of lines, usually no useful info
    
    logger( buildlog, log)
##    writeLines(buildlog, "./buildlog.clean")                         
    
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
            logger( "testbuild failed: Missing R-packages")
        }
        return(list(status="fail", value="missing R-packages", buildlog=buildlog))
    }
    
    if ( length( grep( "Failed build dependencies", buildlog, fixed=TRUE)) >0 ) { ### missing dependencies, no chance
        logger( paste0( "The following missing dependencies must be installed to build R-", pkg))
        if (any ( grep( "needed", buildlog, fixed=TRUE))) {
            for (line in buildlog[ grep( "needed", buildlog, fixed=TRUE)] ) {
                logger( line)
            }
                logger("testbuild failed: Missing dependencies")
        }
        return(list(status="fail", value="missing dependencies", buildlog=buildlog))
    }

    if ( any( grep("Bad exit status", buildlog, fixed=TRUE))) {
        logger( paste0( "Bad exit status from build R-", pkg))
        return(list(status="fail", value="bad exit status", buildlog=buildlog))
    }
    ## no fatal flaws, may be lucky?

    if ( any( grep( "error: Installed (but unpackaged) file(s) found:", buildlog, fixed=TRUE))) {
        logger( paste0("Unpackaged file(s) in build R-", pkg))
        return(list(status="fail", value="unpackaged files", buildlog=buildlog)) 
    }

    if ( any( grep( "devel-file-in-non-devel-package", buildlog)) &
         any( grep( "adness.*exceeds threshold", buildlog))){
        logger( paste0("Probably must be split in ", pkg, " and ", pkg, "-devel"))
        return( list( status="fail", value="split devel", buildlog=buildlog))
    }

    if ( any( grep( "E: lto-no-text-in-archive", buildlog, fixed=TRUE))) {
        logger( paste0("Error: lto-no-text-in-archive ", pkg))
        return(list(status="fail", value="lto-no-text-in-archive", buildlog=buildlog)) 
    }

    if ( any( grep( "E: executable-docs", buildlog))){
        logger( paste0( "Error: executable-docs "))
        return( list( status="fail", value="executable-docs", buildlog=buildlog))
    }
    
    if ( any( grep( "badness.*exceeds threshold", buildlog))){
        logger( paste0("Some RPMlint problems "))
        return( list( status="fail", value="rpmlint problem", buildlog=buildlog))
    }

    
    if (length( grep( "Wrote:", buildlog, fixed=TRUE)) > 1) {
        logger( paste0("Success: ", pkg, " rpm package created"))

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
