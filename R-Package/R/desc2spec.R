#' statusOBS checks if a package already exists either as remote pac on build.opensuse.org
#'
#' @param pkg CRAN R package name
#' @param remoteproj Existing project to build packages for in OBS (build.opensuse.org)
#'
#' @return Version number of package in OBS or NA
#' @export

statusOBS <- function(pkg, remoteproj=getOption("c2o.auto")){
    suppressWarnings(
        osclist <- system2("osc", args=c("ls", paste0(remoteproj,"/R-",pkg)), stdout=TRUE, stderr=TRUE)
    )
    if ( is.null( attributes( osclist))) {
        return( gsub( ".tar.gz", "", gsub( paste0( pkg, "_"), "", osclist[ grep( "tar.gz", osclist)]))) # returns the version 
    } else {
        return(NA)
    }
}

#' versionFromTgz (deprecated) extracts the version info, normalizes it and return
#' a charater string containing that version
#'
#' @param pkg CRAN package name
#' @param tgzname filename to extract version from
#'
#' @return Version string
#' @export

versionFromTgz <- function(pkg, tgzname){
    gsub( ".tar.gz", "", gsub( paste0( pkg, "_"), "", tgzname))
}

#' createEmptySpec takes the name of an R package and creates an specfile
#' with empty file section from its DESCRIPTION file. The OBS directory must
#' be prepared by setupOBSdir
#'
#' @param packname A package name of a package in a R repo like CRAN
#' @param targetdir The directory where rpmbuild should do its work.
#' Sources of packages reside in rpmbuildroot/SOURCES, SPECS in rpmbuildroot/SPECS
#' @param download.cache Directory where sources are stored
#' @param ap A dataframe like the result of available.packages() or cleanDeps()
#' createEmptySpec uses Package, Version, and NeedsCompilation of that dataframe.
#' 
#' @return list of status, 'fail' and 'done' and value. 'value' is either the filename of the generated specfile
#' or a problem description
#' 
#' @export
createEmptySpec <- function(pkg,
                            pac=file.path(getOption("c2o.localOBSdir"), getOption("c2o.auto"), paste0("R-",packname)),
                            download.cache=getOption("c2o.download.cache"),
                            cran=getOption("c2o.cran"),
                            ap = getOption("c2o.status"),
                            log = getOption("c2o.logfile")) {
    ## ap should be a status file
                
    cat("Creating empty spec for pkg ", pkg, "\n")
    cat("Creating empty spec for pkg ", pkg, "\n", file=log, append=TRUE)
    
    if (! file.exists(system.file("specfile.tpl", package="CRAN2OBS"))){
        cat("specfile template not found ?!\n")
        cat("specfile template not found ?!\n", file=log, append=TRUE)

        return(list(status="fail", value="Specfile template not found."))
    }
    
    spectpl <- readLines(system.file("specfile.tpl", package="CRAN2OBS"))
        
    pkg.info <- ap[ ap$Package == pkg, ]
    
    source0 <-  pkg.info$File
    specfile <- paste0(pacdir,"/R-",packname,".spec")
    
    version <- gsub("-",".", pkg.info$Version)
    ## version must be normalized for rpm
    
    ## extract DESCRIPTION file, read it, erase it
    desc.file <- paste( pkg, "/DESCRIPTION", sep="")
    untar( file.path( pacdir, source0), desc.file)
    
    description <- readLines( desc.file)
    
    if ( any(grep("Encoding: ", description, fixed=TRUE))) {
        ## some DESCRIPTIONs seem to be encoded differently, i.e leerSIECyL
        encoding <- trimws( gsub( "Encoding: ", "",
                                 description[ grep( "Encoding: ", description, fixed=TRUE)]), which="both")
        if (encoding %in% c("latin1", "latin2")) {
            system2( "recode", args=c( "..UTF-8", descfile))
            ## re-read re-encoded DESCRITION
            description <- readLines(desc.file) 
        }
    }

    unlink( pkg, recursive=TRUE)
    
    ## template and descrition have been read in now populate the specfile tempalte
    
    spectpl <- gsub( "{{year}}", format( Sys.time(), "%Y" ), spectpl, fixed=TRUE)    
    spectpl <- gsub( "{{packname}}", pkg, spectpl, fixed=TRUE)
    spectpl <- gsub( "{{version}}", version, spectpl, fixed=TRUE)	
    
    summary.str <- sub( "Title: ", "", description[ grep("Title:", description) ], fixed=TRUE)
    spectpl <- gsub( "{{summary}}", summary.str, spectpl, fixed=TRUE)
    
    license <- sub( "License: ", "", description[ grep("License:", description) ], fixed=TRUE)
    spectpl <- gsub( "{{license}}", license, spectpl, fixed=TRUE)
    
    spectpl <- gsub( "{{source0}}", source0, spectpl, fixed=TRUE)
#    deps <-  cleanList( packname, "depends" )
    deps <- pkg.info$recDep
    
    if (length(deps) >0) {
        deps <- paste( "R-", deps, sep="")
    } else {
        deps <- c()
    }

#    suggests <- cleanList( packname, "suggests")
    suggests <- pkg.info$Suggests
    if (length(suggests) > 0) {
        suggests <- paste( "R-", suggests, sep="")
    } else {
        suggests <- c()
    }
    
    descstart <- grep( "Description:", description, fixed=TRUE)
    descend <- descstart + grep( "^[A-Z][a-z]+:", description[ (descstart+1):length(description)])[1] -1
    description.str <- trimws( paste( description[ descstart:descend ] ), which="both")
    description.str[1] <- gsub("Description: ", "", description.str[1])
### TODO still needs formating! textwrap for R?
    
    needs.compilation <- pkg.info$NeedsCompilation
    
    cat( "# Automatically generated by CRAN2OBS\n", file=specfile)
    
    for (line in spectpl) {
        if ( grepl( "{{depends}}", line, fixed=TRUE)) {
            if ( length( deps) > 0) {
                for ( item in deps) cat( "Requires:\t", item, "\n", sep="", file=specfile, append=TRUE)
            } else { next }
        } else if ( grepl( "{{builddepends}}", line, fixed=TRUE)) {
            if  ( length( deps) > 0)  {
                for ( item in deps) cat( "BuildRequires:\t", item, "\n", sep="", file=specfile, append=TRUE)
            } else { next }
        } else if ( grepl( "{{suggests}}", line, fixed=TRUE) ) {
            if ( length( suggests) > 0)  {
                for ( item in suggests) cat( "Recommends:\t", item, "\n",  sep="", file=specfile, append=TRUE)
            } else {next}
        } else if ( grepl( "{{needscompilation}}", line, fixed=TRUE) ) {
            if ( needs.compilation == "yes") cat( "BuildRequires:   gcc gcc-c++ gcc-fortran\n", file=specfile, append=TRUE)
        } else if ( grepl( "{{description}}", line, fixed=TRUE) ) {
            for ( item in description.str) cat( item, "\n", file=specfile, append=TRUE)
        } else {
            cat( line, "\n", file=specfile, append=TRUE)
        }
    }
    return(list(status="done", value=specfile))
}

#' buildforfiles performs a build without %files section
#' and tries to contstruct all files from the build errors.
#' 
#' @param pkg Name of a package in a R repo like CRAN
#' @param pac directory for OBS
#' @param specfile Location of created specfile
#' @param rpmbuildroot The directory where rpmbuild should do its work
#' @param localOBSdir The top level directory of a checkout of the project
#' you want to create your package in. 
#' @param remoteproj Name of the OBS project
#' @export
buildforfiles <- function(pkg, pac, specfile, localOBS=getOption("c2o.localOBSdir"),
                          remoteproj=getOption("c2o.auto"), download.cache=getOption("c2o.download.cache"),
                          binary.cache=getOption("c2o.binary.cache"),
                          ap = if (! is.null (getOption("c2o.status"))) getOption("c2o.status"),
                          log=getOption("c2o.logfile")){

    speclines <- readLines( specfile)
    result <- dropFileSection( speclines)
    
    if ( result$status != "done") {
        cat("dropping file section failed")
        cat("dropping file section failed", file=log, append=TRUE)
        return (list(status="failed", problem=result$problem))
    }
    writeLines( result$speclines, specfile)
    
    result <- testbuild( pkg, pac, specfile, ap=ap)
    
    ## we dont need to check for success, because %files section is empty and therefore fail by design
    
    if (! result$value == "unpackaged files"){
        cat( "buildforfiles: Failed to build files section for", pkg, " \n", sep="")
        cat( "buildforfiles: Failed to build files section for", pkg, " \n", sep="", file=log, append=TRUE)
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
#' @param remoteproj Name of the OBS project
#' @return list of status, value, buildlog
#' @export
testbuild <- function(pkg, pac, specfile,
                      log=getOption("c2o.logfile"),
                      localOBS=getOption("c2o.localOBSdir"),
                      remoteprj=getOption("c2o.auto"),
                      download.cache=getOption("c2o.download.cache"),
                      binary.cache=getOption("c2o.binary.cache"),
                      ap = if (! is.null (getOption("c2o.status"))) getOption("c2o.status")){

    cmd <- paste("\""," cd", pac,
                 "; osc build --prefer-pkgs=", binary.cache, " --keep-pkgs=", binary.cache,
                 " --local-package --ccache", specfile, "\"" )
    suppressWarnings(
        buildlog <- system2("bash", args=c("-c", cmd), stdout=TRUE, stderr=TRUE)
    )

    buildlog <- trimws( gsub( "^\\[.*\\]", "", buildlog), which="left")
    logger( buildlog, log)
                         
    
    if ( any ( grep( "ERROR: dependency", buildlog, fixed=TRUE)) ) { ### missing some dependency, no chance
        cat( "The following missing dependencies must be available to build R-", pkg, "\n", sep="")
        cat( "The following missing dependencies must be available to build R-", pkg, "\n", sep="", file=logfile, append=TRUE)
        for (line in buildlog[ grep( "ERROR: dependency", buildlog, fixed=TRUE)] ) {
            cat( line,"\n")
            cat( line,"\n", file=log, append=TRUE)
            cat("testbuild failed: Missing dependency\n", file=log, append=TRUE)
        }
        return(list(status="failed", value="Missing dependencies in R CMD INSTALL", buildlog=buildlog))
    }
    
    if ( any ( grep( "nothing provides", buildlog, fixed=TRUE)) ) { ### missing R-rpms, no chance
        cat( "The following missing dependencies must be built first to build R-", pkg, "\n", sep="")
        cat( "The following missing dependencies must be built first to build R-", pkg, "\n", sep="", file=log, append=TRUE)
        for (line in buildlog[ grep( "nothing provides", buildlog, fixed=TRUE)] ) {
            cat( line,"\n")
            cat( line,"\n", file=log, append=TRUE)
            cat("testbuild Failed: Missing R-packages\n", file=log, append=TRUE)
        }
        return(list(status="failed", value="missing R-packages", buildlog=buildlog))
    }
    
    if ( length( grep( "Failed build dependencies", buildlog, fixed=TRUE)) >0 ) { ### missing dependencies, no chance
        cat( "The following missing dependencies must be installed to build R-", pkg, "\n", sep="")
        cat( "The following missing dependencies must be installed to build R-", pkg, "\n", sep="", file=logfile, append=TRUE)
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
        return(list(status="failed", problem="unpackaged files", buildlog=buildlog)) 
    }
    
    if (length( grep( "Wrote:", buildlog, fixed=TRUE)) == 2) {
        cat("Success: ", pkg, " rpm package created\n")
        cat("Success: ", pkg, " rpm package created\n", file=logfile, append=TRUE)
        print(buildlog[grep("Wrote:", buildlog, fixed=TRUE)])
        for (line in  buildlog[grep("Wrote:", buildlog, fixed=TRUE)]){
            cat(line, "\n", file=logfile, append=TRUE)
        }
        return(list(status="done", value=NA, buildlog=buildlog))
    }
    ## if we end here some unknown condition has prevented a successful build
    cat( "Failed to build R-", pkg, " unknown problem\n", sep="")
    cat( "Failed to build R-", pkg, " unknown problem\n", sep="", file=log, append=TRUE)
    return(list(status="failed", problem="unknown problem", buildlog=buildlog))    
}

#' updateOBSpkg takes the name of an R package with newer
#' version CRAN and tries to update with minimal changes to
#' the existing spec.
#'
#' @param pkg Name of a package in a R repo like CRAN
#' @param rpmbuildroot The directory where rpmbuild should do its work
#' @param localOBSdir The top level directory of a checkout of the project
#' you want to create your package in. 
#' @param remoteproj Name of the OBS project
#' @param ap A dataframe containing info like from available.packages or cleanDeps.
#' 
#' @return The path to the generated specfile.
#' @export
updateOBSpkg <-  function(pkg, localOBSdir=getOption("c2o.localOBSdir"), remoteproj=getOption("c2o.auto"),
                          download.cache=getOption("c2o.download.cache"), cran=getOption("c2o.cran"),
                          ap = data.frame(available.packages(repos=getOption("c2o.cran")))) {
    
    log <- "updateOBSpkg.log"
    cat("Building ", pkg, "\n", file=log, append=TRUE)

    if (! pkg %in% ap[,"Package"]) {
        cat("Seems ", pkg, " does not exist on CRAN\n")
        cat("Seems ", pkg, " does not exist on CRAN\n", file=log, append=TRUE)
        return( paste( "Failed: Package", pkg, " not on CRAN"))
    }
    
    obsversion <- statusOBS( pkg, remoteproj)
    ## TODO should be included in c2o.status
    
    if ( is.na( obsversion)) {
        cat("Failed: ", pkg, " does not exist in OBS. updateOBSpkg only does updates!", file=log, append=TRUE)
        return( "Failed: updateOBSpkg only does updates!")
    }

    pkgdir <-  file.path( localOBSdir, remoteproj, paste0( "R-", pkg))

    if ( dir.exists( pkgdir )){
        cmd <- paste( "\"", "cd", file.path( localOBSdir, remoteproj), " ; osc up ", paste0( "R-", pkg), "\"")
        result <- system2( "bash", args = c("-c", cmd), stdout=TRUE, stderr=TRUE)
        if( ! is.null(attributes(result))) {
            cat(result)
            cat(pkg, " could not update local OBS pkg\n")
            cat(pkg, " could not update local OBS pkg\n", file=log, append=TRUE)
            return(paste("Failed: Package", pkg, " could not update OBS co"))
        }

    } else { ## create dir to hold package for OBS
        cmd <- paste("\"", "cd", file.path( localOBSdir, remoteproj), " ; osc co ",paste0( "R-", pkg)  , "\"")
        result <- system2( "bash",  args = c("-c", cmd), stdout=TRUE, stderr=TRUE)
        if( ! is.null( attributes( result))) {
            cat( result)
            cat( pkg, " could not checkout from OBS\n")
            cat( pkg, " could not checkout from OBS\n", file=log, append=TRUE)
            return( paste( "Failed: Package", pkg, " could not checkout from OBS"))
        }
    }
    ## current pac is checked out
    ## download new sources
    
    version <- ap[ ap$Package == pkg, "Version"]
    source0 <-  paste0( pkg, "_", version, ".tar.gz")
    specfile <- paste0( pkgdir, "/R-", pkg, ".spec")
    version <- gsub( "-", ".", version)

    if (! file.exists( file.path( download.cache, source0))) {
        if ( download.file( file.path( cran ,"src/contrib", source0),
                            file.path( download.cache, source0)) != 0){
            cat("Seems ", pkg, " no source on CRAN\n")
            cat("Seems ", pkg, " no source on CRAN\n", file=errorlog, append=TRUE)
            return(paste("Failed: Package", pkg, " download failed from CRAN"))
        }
    }

    if ( !file.copy( file.path( download.cache, source0), file.path( pkgdir, source0), overwrite=TRUE )){
        cat("Seems ", pkg, " setup of pac failed\n")
        cat("Seems ", pkg, " setup of pac failed\n", file=errorlog, append=TRUE)
        return(paste("Failed: Package", pkg, " setup of pac failed"))
    }

    file.remove( file.path( pkgdir, paste0( pkg, "_", obsversion, ".tar.gz"))) 
    ## new sources are in place in local OBS

    speclines <- readLines( specfile)
    cat( "First try, just replace the version\n")
    speclines[ grep( "Version:", speclines)] <- paste0("Version:        ", version)
    speclines[ grep( "Source:",  speclines)] <- paste0("Source:         ", source0)
    writeLines(speclines, specfile)

    ## time to build
    buildresult <- testbuild( pkg, pkgdir, specfile, ap=ap)

    if (buildresult$status == "success") {
        return(ap[ap$Package == pkg, "Version"])
    }

    ## if we land here the very simplest strategy of just replacing version number
    ## and sources didn't work. But no missing dependencies or compile errors either.

    buildresult <- buildforfiles( pkg, pkgdir, specfile, ap=ap)
    
    ## we dont need to check for success, because %files section empty and therefore fail by design
    
    ## and try again!
    buildresult <- testbuild( pkg, pkgdir, specfile, ap=ap)
    ##cat("rebuild with new  %files section completed\n")
    
    if ( buildresult$status == "success") {
        return( ap[ ap$Package == pkg, "Version"])
    }

    ## Last try: rebuild spec file from scratch
    result <- createEmptySpec( pkg, pkgdir, download.cache=download.cache, ap=ap)
    if (! grepl( ".spec", result)){ ##failure
        cat( "Failed: ", pkg, " no specfile created. Error was ", specfile, "\n")
        return( "Failed: could not create specfile")
    }
    specfile <- result
    speclines <- readLines( con=specfile)
### first build
    buildresult <- buildforfiles( pkg, pkgdir, specfile, ap=ap)
    buildresult <- testbuild( pkg, pkgdir, specfile, ap=ap)

    if ( buildresult$status == "success") {
        return( ap[ap$Package == pkg, "Version"] )
    }
    
    
    # out of luck, manual intervention needed
    cat( "Failed to update ", pkg, " build\n", sep="")
    return( "Failed: Unknown updating error")
}

#' uploadpac pushes a successfully built pac to remoteprj
#' 
#' @param pkg CRAN package name
#' @param version to upload
#' @param buildtype "initial build" or "update"
#' @param localOBS base directory for osc checkouts
#' @param remoteprj the project for which packages are built
#' @param cran CRAN mirror for download
#' @param status dataframe holding the current sync status between
#' CRAN and remoteprj
#' @return list "status" and "value"
uploadpac <- function(pkg, version, buildtype
                       localOBS  = getOption("c2o.localOBSdir"),
                       remoteprj = getOption("c2o.auto"),
                       log       = getOption("c2o.logfile")){

    pac <- file.path( localOBS, remoteprj, paste0( "R-", pkg))

    cmd <- paste( "\"", "cd", pac , " && osc addremove \"")
    result <- system2( "bash", args = c("-c", cmd), stdout=TRUE, stderr=TRUE)

    if( ! is.null(attributes(result))) {
        logger( paste0( pkg, " could not addremove"), log)
        return( list( status="fail", value="could not addremove"))
    }

    if (buildtype = "update") {
        msg <- " - automatically updated to version "
    } else {
        msg <- " - initial checkin of version "
    }
    
    
    cmd <- paste( "\"", "cd", pac , " && osc vc -m '", msg,  version, "' \"")
    result <- system2( "bash", args = c("-c", cmd), stdout=TRUE, stderr=TRUE)

    if( ! is.null(attributes(result))) {
        logger( paste0( pkg, " could not vc"), log)
        return( list( status="fail", value="could not vc"))
    }

    cmd <- paste( "\"", "cd", pac , " && osc ci -m '", msg, version,"' \"")
    result <- system2( "bash", args = c("-c", cmd), stdout=TRUE, stderr=TRUE)

    if( ! is.null(attributes(result))) {
        logger( paste0( pkg, " could not ci"), log)
        return( list( status="fail", value="could not ci"))
    }
    
    return( list( status="done", value=NA))
}

#' cleanuppac removes a pac directory and removes the
#' pkg from the local build service database.
#' 
#' @param pkg CRAN package name
#' @param localOBS base directory for osc checkouts
#' @param remoteprj the project for which packages are built
#' @param cran CRAN mirror for download
#' @param status dataframe holding the current sync status between
#' CRAN and remoteprj
#' @return list "status" and "value"
cleanuppac <- function(pkg,
                       localOBS  = getOption("c2o.localOBSdir"),
                       remoteprj = getOption("c2o.auto"),
                       log       = getOption("c2o.logfile")){

    cmd <- paste( "\"", "cd", file.path(localOBS, remoteprj) , " && osc delete --force ", paste0( "R-", pkg), "\"")
    result <- system2( "bash", args = c("-c", cmd), stdout=TRUE, stderr=TRUE)

    if( ! is.null(attributes(result))) {
        logger(paste0( pkg, " could clean up local pac"), log)
        return( list( status="fail", value="could not clean up local pac"))
    }
    return( list( status="done", value=NA))
}

#' setuppac creates the OBS project for pkg on the local machine
#' or checks out the copy from remoteprj.
#'
#' @param pkg CRAN package name
#' @param localOBS base directory for osc checkouts
#' @param remoteprj the project for which packages are built
#' @param cran CRAN mirror for download
#' @param status dataframe holding the current sync status between
#' CRAN and remoteprj
#' @return list( status, value) where 'status' is "done" or "fail"
#' and 'value' is the created directory or a string with an error message 
#'
#' @export
setuppac <- function(pkg,
                        localOBS  = getOption("c2o.localOBSdir"),
                        remoteprj = getOption("c2o.auto"),
                        cran      = getOption("c2o.cran"),
                        status    = getOption("c2o.status"),
                        log       = getOption("c2o.logfile")){
    cat("** Setting up OBSdir for package ", pkg, "\n", file=log, append=TRUE)

    if (! pkg %in% status[, "Package"]) {
        cat("Seems ", pkg, " not found on CRAN\n")
        cat("Seems ", pkg, " not found on CRAN\n", file=log, append=TRUE)
        return(list(status="fail", value="not found"))
    }
    
    pac <- file.path( localOBS, remoteprj, paste0( "R-", pkg))

    inOBSVersion <-  status[ which( status$Package == pkg) , "OBSVersion"]
    
    if (! is.na( inOBSVErsion )) { # update
        if ( dir.exists( pac )){ # just update
            cmd <- paste( "\"", "cd", pac, " && osc up \"")
            result <- system2( "bash", args = c("-c", cmd), stdout=TRUE, stderr=TRUE)
            
            if( ! is.null(attributes(result))) {
                cat(result, "\n")
                cat(pkg, " could not update local OBS pkg\n")
                cat(pkg, " could not update local OBS pkg\n", file=log, append=TRUE)
                return( list( status="fail", value="could not update"))
            }
        } else { ## checkout
            cmd <- paste( "\"", "cd", file.path( localOBSdir, remoteproj), " && osc co ", paste0( "R-", pkg), "\"")
            result <- system2( "bash",  args = c("-c", cmd), stdout=TRUE, stderr=TRUE)
            if( !is.null(attributes(result))) {
                cat(result)
                cat(pkg, " could not checkout from OBS\n")
                cat(pkg, " could not checkout from OBS\n", file=log, append=TRUE)
                return( list( status="fail", value="could not checkout"))
            }
        }
    } else { # create
        if ( dir.exists( pac )){ # cleanup neccessary
            logger( paste0("cleanup of existing local ", pac))
            
            cmd <- paste("\"", "cd", file.path(localOBS, remoteprj) , "&& osc delete --force ", paste0( "R-", packname)  , "\"")
            result <- system2(  "bash",  args = c("-c", cmd), stdout=TRUE, stderr=TRUE)
            
            if( ! is.null(attributes(result))) {
                cat(result, "\n")
                logger(paste0(pkg, " could not setup packname"))
                return(list(status="fail", value="could not remove existing pac"))
            }
        }
        
        ## create dir to hold package for OBS
        cmd <- paste("\"", "cd", file.path( localOBSdir, remoteproj), " ; osc mkpac ",paste0( "R-", packname)  , "\"")
        result <- system2(  "bash",  args = c("-c", cmd), stdout=TRUE, stderr=TRUE)
        if( ! is.null(attributes(result))) {
            cat(result)
            cat(packname, " could not create pac for ", pkg, "\n")
            cat(packname, " could not create pac for ", pkg, "\n", file=log, append=TRUE)
            return( list( status="fail", value="could not setup pac"))
        }
    }

    ## pac checked out or created, get the sources

    version <- gsub( "-", ".", status[ which( status$Package == pkg), "Version"])
    source0 <- paste0( pkg, "_", version, ".tar.gz")

    if (! file.exists( file.path( download.cache, source0))) {
        if ( download.file( file.path( cran ,"src/contrib", source0),
                           file.path( download.cache, source0)) != 0){
            logger(paste0(pkg, ": no sources found on CRAN"))
            return( list( status="fail", value="no sources found"))
        }
    }
    
    if ( !file.copy( file.path( download.cache, source0), file.path( pkgdir, source0), overwrite=TRUE )){
        cat(pkg, ": copy of sources failed\n")
        cat(pkg, ": copy of sources failed\n", file=log, append=TRUE)
        return( list( status="fail", value="copy of soources failed"))
    }
    
    if (! is.na( inOBSVersion)){ # it is an update, there must be an old source file, just remove it
        if ( !file.remove( file.path( pac, paste0( pkg, "_", obsversion, ".tar.gz")))){
            logger(paste0(pkg, ": could not rm old sources"))
            return( list( status="fail", value="coud not rm old sources"))
        }
    }
    return( list( status="done", value=pac))
}

#' sync2pac takes the name of an R package and creates or updates 
#' package directory to be used in OBS. 
#' The external tool osc is used to build
#' the OpenSUSE package. The resulting spec file will reside in a
#' package dir under a local checkout of a remote proj.
#'
#' @param pkg Name of a package in a R repo like CRAN
#' @param rpmbuildroot The directory where rpmbuild should do its work
#' @param localOBS The top level directory of a checkout of the project
#' you want to create your package in. 
#' @param remoteprj Name of the OBS project
#' @param cran CRAN mirror to use
#' @param ap A dataframe containing the sync status of cran and remoteprj
#' 
#' @return list of 'status', "done" or "fail" and 'problem' set to NA
#' or character string
#' @export

sync2pac <- function( pkg,
                      localOBS  = getOption( "c2o.localOBSdir"),
                      remoteprj = getOption( "c2o.auto"),
                      cran      = getOption( "c2o.cran"),
                      ap        = if (! is.null (getOption( "c2o.status"))) getOption( "c2o.status"),
                      download.cache = getOption( "c2o.download.cache"),
                      binary.cache = getOption( "c2o.binary.cache"),
                      log       = getOption( "c2o.logfile")) {
    
    cat("Syncing ", pkg, " to OBS\n")
    cat("Syncing ", pkg, " to OBS\n", file=log, append=TRUE)

    if ( ! pkg %in% ap[ , "Package"] ) {
        cat( "Seems ", pkg, " not in status file\n")
        cat( "Seems ", pkg, " not in status file\n", file=log, append=TRUE)
        return( list( status="fail", problem=paste( "Package not in status file")))
    }
    
    pkg.info <- ap[ ap$Package == pkg, ]

    if ( is.na( pkg.info$Version) ) {
        logger(paste0("Seems ", pkg, " not in CRAN"), log)
        return( list( status="fail", problem=paste( "Package not in CRAN")))
    }

    if ( ! is.na( pkg.info$OBSVersion)) {
        if ( pkg.info$OBSVersion != pkg.info$Version) {
            logger( paste0( "Update ", pkg), log)
            buildtype = "update"
        } else {
            logger(paste0( pkg, " already uptodate"), log)
            return( list( status="done", value=pkg.info$Version))
        }
    } else {
        buildtype = "initial build"
    }

    ## All recDep of pkg must have an OBSVersion in status, otherwise any try is void
    ## due to binary.cache we can use packages still not publishes in OBS
    if (pkg.info$depLen > 0) {
        deps <- unlist(strsplit(pkg.info$recDep, " "))
        depsinOBS <- intersect( deps, status$Package)
        if ( pkg.info$depLen != length(depsinOBS)) { ## missing dependencies, no chance
            missing <- setdiff( deps, depsinOBS )
            msg <- "missing R package dependencies"
            logger( paste0(msg, " to build " , pkg), log)
            logger( paste0( paste( "R-", missing, sep=""), collapse=" "), log)
            return( list( status = "fail", value=msg))
        } 
    }
    
    result <- setuppac( pkg, localOBS=localOBS, remoteprj=remoteprj, cran=cran, status=db, log=log)

    if ( result$status == "fail") {
        cat( "Setting up OBS dir failed for pkg ", pkg, "\n")
        cat( "Setting up OBS dir failed for pkg ", pkg, "\n", file=log, append=TRUE)
        return( list( status="fail", problem=result$value))
    }

    pac <- result$value
    result <- createEmptySpec(pkg, pac, download.cache=download.cache, ap)

    if (result$status == "fail") {
        cat("Creating empty spec failed for pkg ", pkg, "\n")
        cat("Creating empty spec failed for pkg ", pkg, "\n", file=log, append=TRUE)
        return( list( status="fail", problem=paste( "creating empty spec failed")))
    }

    specfile <- result$value
### speclines <- readLines(con=specfile)
### first build
    result <- buildforfiles( pkg, pac, specfile, localOBS=localOBS, remoteprj=remoteprj, download.cache=download.cache, binary.cache=binary.cache, ap=ap, log=log)

    if (! result$status == "done") {
        cat( "Failed to construct files section for ", pkg, " \n", sep="")
        cat( "Failed to construct files section for ", pkg, " \n", sep="", file = log, append =TRUE)
        return( list( status="fail", value="failed to construct files section"))
    }

    ## here the %file section is fully populated

    specfile <- result$value
    
    ## second build!
    result <- testbuild( pkg, pac, specfile, ap=ap, log=log)

    if ( result$status == "fail") {
        logger( paste0( "Failed to automatically build ", pkg), log)
        syncresult <- list( status="fail", value="unresolvable (automatically) error")
    } else {
        logger( paste0( pkg, " automatically built"), log)
        syncresult <- list( status="done", value=pkg.info$Version)
    }

### package successfully built!
### upload
### cleanup
    result <- uploadpac( pkg, pkg.info$Version, type=buildtype, localOBS=localOBS, remoteprj=remoteprj, cran=cran, status=db, log=log)
    if (! result$status == "done") {
        logger( paste0( "Failed to upload ", pkg , " to ", remoteprj), log)
        return( list( status="fail", value="failed to construct files section"))
    }
    logger( paste0( pkg, " uploaded"), log)

    result <- cleanuppac( pkg, localOBS=localOBS, remoteprj=remoteprj, cran=cran, status=db, log=log)
    if (! result$status == "done") {
        logger( paste0( "Failed final cleanup for ", pac), log)
        return( list( status="fail", value="failed to construct files section"))
    }
    logger( paste0( pac, "cleaned up"), log)

    return( syncresult)
}

#' updateStatus incorporates new information about tried builds in
#' the status dataframe.
#' @param status the dataframe containing status information about remoteprj
#' @param pkg CRAN package name which has new information
#' @param version latest version tried
#' @param success if or not the build was successful
#'
#' @return updated status dataframe
#' 
#' @export
updateStatus <- function( status=getOption(c2o.status), pkg, version, success=TRUE, log=getOption("c2o.logfile")){
    if (! pkg %in% status[, "Package"]) {
        msg <- paste0("Seems ", pkg, " has no status")
        logger(msg, log)
        return(list(status="fail", value=msg))
    }
    i <- which ( status$Package == pkg )
    if (success) {
        status[i , "OBSVersion"] <- version
    }
    status[i , "triedVer"] <- version
    return(status)
}


### TODO Check, if the rpm -q --provides package command only show expected provdides
#### see R-gdata incident



#' createOBSpac takes the name of an R package and creates a 
#' package directory to be used in OBS.
#' The external tool osc is used to build
#' the OpenSUSE package. The resulting spec file will reside in a
#' package dir under a local checkout of a remote proj.
#' The %check section is deliberately empty. We rely
#' on the checks on CRAN.
#'
#' @param packname Name of a package in a R repo like CRAN
#' @param rpmbuildroot The directory where rpmbuild should do its work
#' @param localOBSdir The top level directory of a checkout of the project
#' you want to create your package in. 
#' @param remoteproj Name of the OBS project
#' @param ap A dataframe containing info like from available.packages or cleanDeps.
#' 
#' @return The path to the generated specfile.
#' @export

createOBSpac <- function(pkg,
                         localOBS  = getOption("c2o.localOBSdir"),
                         remoteprj = getOption("c2o.auto"),
                         cran      = getOption("c2o.cran"),
                         db        = getOption("c2o.status"),
                         log       = getOption("c2o.logfile")) {
    
    cat("Building ", packname, "\n", file=log, append=TRUE)

    if (! packname %in% ap[,"Package"]) {

        cat("Seems ", packname, " does not exist on CRAN\n")
        cat("Seems ", packname, " does not exist on CRAN\n", file=log, append=TRUE)
        return(paste("Failed: Package", packname, " not on CRAN"))
    }
    
    obsstatus <- statusOBS(packname, remoteproj)
    if (! is.na(obsstatus) ) {
        cat("Failed: ", packname, " exists in OBS. createOBSpac does no updates!", file=log, append=TRUE)
        return("Failed: createOBSpac does no OBS updates!")
    }

    pacdir <-  file.path( localOBSdir, remoteproj, paste0( "R-", packname))

    if ( dir.exists( pacdir )){
        cat("Failed: ", packname, " exists in local OBS. createOBSpac does no updates!", file=log, append=TRUE)
        return("Failed: createOBSpac does no local updates!")
    } else { ## create dir to hold package for OBS
        cmd <- paste("\"", "cd", file.path( localOBSdir, remoteproj), " ; osc mkpac ",paste0( "R-", packname)  , "\"")
        result <- system2(  "bash",  args = c("-c", cmd), stdout=TRUE, stderr=TRUE)
        if( ! is.null(attributes(result))) {
            cat(result)
            cat(packname, " could not setup packname\n")
            cat(packname, " could not setup packname\n", file=log, append=TRUE)
            return(paste("Failed: Package", packname, " could not setup pacdir"))
        }
    }
    
    result <- createEmptySpec(packname, pacdir, download.cache="~/rpmbuild/SOURCES" , ap)
    if (! grepl(".spec", result)){ ##failure
        cat("Failed: ", packname, " no specfile created. Error was ", specfile, "\n", file=log, append=TRUE)
        return("Failed: could not create specfile")
    }
    specfile <- result
    speclines <- readLines(con=specfile)
### first build
    buildresult <- testbuild( packname, pacdir, specfile, ap=ap)
    
    if (buildresult$problem %in% c("bad exit status", "missing dependencies", "unknown problem", "missing R-packages")){
        cat( "Failed to build ", packname, " \n", sep="")
        return("Failed: Unrecoverable error.")
    }

### At this point something was build. But as the specfile.tpl has an empty file section.
### that part must be constructed from error logs. Therfore we know the error structure
### and where to find the unpackaged files list.

    filelist <- extractFilesFromLog(buildresult$buildlog, packname)
    version <- gsub("-",".",ap[ap$Package == packname, "Version"])
    gsub(version, "%{version}",filelist) # if there are files with version in name, this will be caught, see abcrlda
    
    speclines <- c(speclines , filelist)
    writeLines(speclines, specfile)
### here the %file section is fully populated

### second build!
    buildresult <- testbuild( packname, pacdir, specfile, ap=ap)

    if (buildresult$status == "success") {
        return(ap[ap$Package == packname, "Version"])
    }

    if (buildresult$problem %in% c("bad exit status", "missing dependencies", "unknown problem", "missing R-packages")){
        cat( "Failed to build ", packname, " \n", sep="")
        return("Failed: Unrecoverable error.")
    }
    
    # out of luck, manual intervention needed
    cat( "Failed to update ", packname, " in second osc build\n", sep="")
    return("Failed: Unknown updating error")

### At this point osc successfully generated a package
### TODO Check, if the rpm -q --provides package command only show expected provdides
#### see R-gdata incident
}

#' dropFileSection removes the filelist from a character array
#' representing a spec file. Keep the first line of the file list.
#'
#' @param speclines character vector with lines holding a specfile
#'
#' @return list of two components status and speclines
#' in status is "success" oder "fail", speclines either with empty file
#' section resp. unaltered.
#'
#' @export

dropFileSection <- function(speclines){
    filesLine <- grep("%files",speclines,fixed=TRUE)
    if (length(filesLine) != 1) {
        return(list(status="fail", speclines, problem="No or more than one %files section! Don't know what to do."))
    } else {
        speclines[filesLine+1]="%dir %{rlibdir}/%{packname}"
    }
    return(list(status="done", speclines=speclines[1:(filesLine+1)]))
}

#' extractFilesFromLog takes the lines of an osc build log and
#' extrats the list of unpackaged files. Times must be stripped
#' already.
#'
#' @param buildlog Character vector containing the lines of an osc
#' build log
#'
#' @return Character vector containing entries for the files section,
#'
#' @export

extractFilesFromLog <- function(buildlog, packname, systemRlib="/usr/lib64/R/library/"){
    buildlog <- buildlog[ ( grep( "RPM build errors", buildlog, fixed=TRUE)+2):( length(buildlog)-5) ]
    buildlog <- gsub( paste0( systemRlib, packname, "/"), "", buildlog)

    filelist <- c()
    dirlist <- NULL
    for (line in buildlog){
        if ( grepl( "/", line, fixed=TRUE) ) { ## a file in a subdirectory, extract the unique directories
            nextdir <- unlist( strsplit( line, "/") )[1]
            if (! nextdir %in% dirlist){
                dirlist <- c( dirlist, nextdir )
                if (grepl( "html", line, fixed=TRUE) | grepl( "help", line, fixed=TRUE)) {
                    filelist <- c( filelist, paste0("%doc %{rlibdir}/%{packname}/", nextdir))
                } else {
                    filelist <- c( filelist, paste0("%{rlibdir}/%{packname}/", nextdir))
                }
            }
        } else { ## a file
            if ( grepl( "LICEN", line, fixed=TRUE)) {
                filelist <- c( filelist, paste0( "%license %{rlibdir}/%{packname}/", line))
            } else if (grepl( "DESCRIPTION", line, fixed=TRUE)) {
                filelist <- c( filelist, paste0( "%doc %{rlibdir}/%{packname}/", line))
            } else if (grepl( "NEWS", line, fixed=TRUE)) {
                filelist <- c( filelist, paste0( "%doc %{rlibdir}/%{packname}/", line))
            } else {
                filelist <- c( filelist, paste0( "%{rlibdir}/%{packname}/", line))
            }
        }
    }
    return(filelist)
}

#' desc2spec takes the name of an R package and generates a spec file
#' to be used in OBS. The external tool rpmbuild is used to build
#' the OpenSUSE package. The resulting spec file will reside where
#' rpmbuild puts it. The %check section is deliberately empty. We rely
#' on the checks on CRAN.
#'
#' @param packname A package name of a package in a R repo like CRAN
#' @param rpmbuildroot The directory where rpmbuild should do its work
#' 
#' @return The path to the generated specfile.
#' @export
desc2spec <- function(packname, rpmbuildroot="~/rpmbuild/",
                      ap = data.frame(available.packages(repos="https://cloud.r-project.org"))) {
    errorlog <- "desc2spec.err"
    cat("Building ", packname, "\n", file=errorlog, append=TRUE)

    specfile <- createEmptySpec(packname, rpmbuildroot, ap)

### call rpmbuild

    suppressWarnings(
        rpmlog <- system2("rpmbuild", 
                          args   = c("-ba", specfile),
                          env    = "LANG=en;",
                          stdout = TRUE,
                          stderr = TRUE)
    ## the first build will fail by construction.
    ## the output of rpmbuild allows to build the %file section
    ## in the spec
    )

    ## but we have to look for other errors preventing successful second run
    
    if ( length( grep( "Failed build dependencies", rpmlog, fixed=TRUE)) >0 ) { ### missing dependencies, no chance
        cat( "The following missing dependencies must be installed to build R-", packname, "\n", sep="")
        cat( "The following missing dependencies must be installed to build R-", packname, "\n", sep="", file=errorlog, append=TRUE)
        for (line in rpmlog) if ( grepl( "needed", line, fixed=TRUE)) {
                                 cat( line,"\n")
                                 cat( line,"\n", file=errorlog, append=TRUE)
                                 cat("Failed: Missing dependencies first run rpmbuild\n", file=errorlog, append=TRUE)
                             }
        return("Failed: Missing dependencies first run rpmbuild")
    }

    if (length( grep("Bad exit status", rpmlog, fixed=TRUE)) > 0 ) {
        cat( "Bad exit status from build R-", packname, "\n", sep="")
        cat( "Bad exit status from build R-", packname, "\n", sep="", file=errorlog, append=TRUE)
        return("Failed: Bad exit status")
    }

### At this point something was build. But as the specfile.tpl has an empty file section.
### that part must be constructed from error logs
    
    # Double sub-setting needed because "Installed (but unpackaged) file(s) found" isn't
    # necessarily right under "RPM build errors"...
    rpmlog <- rpmlog[ (grep( "RPM build errors", rpmlog, fixed=TRUE)+1):length(rpmlog)]
    rpmlog <- trimws( rpmlog[ (grep( "Installed (but unpackaged) file(s) found", rpmlog, fixed=TRUE)+1):length(rpmlog)], which="left")
    rpmlog <- gsub(paste("/usr/lib64/R/library/",packname,"/",sep=""), "", rpmlog)
    
    dirlist <- NULL
    for (line in rpmlog){
        if ( grepl( "/", line, fixed=TRUE) ) { ## a file in a subdirectory, extract the unique directories
            nextdir <- unlist( strsplit( line, "/") )[1]
            if (! nextdir %in% dirlist){
                dirlist <- c( dirlist, nextdir )
                if (grepl( "html", line, fixed=TRUE) | grepl( "help", line, fixed=TRUE)) {
                    cat( "%doc %{rlibdir}/%{packname}/", nextdir, "\n", sep="", file=specfile, append=TRUE)
                } else {
                    cat( "%{rlibdir}/%{packname}/", nextdir, "\n", sep="", file=specfile, append=TRUE)
                }
            }
        } else { ## a file
            if ( grepl( "LICENSE", line, fixed=TRUE)) {
                cat( "%license %{rlibdir}/%{packname}/", line, "\n", sep="", file=specfile, append=TRUE )
            } else if (grepl( "DESCRIPTION", line, fixed=TRUE)) {
                cat( "%doc %{rlibdir}/%{packname}/", line, "\n", sep="", file=specfile, append=TRUE )
            } else if (grepl( "NEWS", line, fixed=TRUE)) {
                cat( "%doc %{rlibdir}/%{packname}/", line, "\n", sep="", file=specfile, append=TRUE )
            } else {
                cat( "%{rlibdir}/%{packname}/", line, "\n", sep="", file=specfile, append=TRUE)
            }
        }
    }

### second build!
    rpmlog <- system2("rpmbuild", 
                      args   = c("-ba", specfile),
                      env    = "LANG=en;",
                      stdout = TRUE,
                      stderr = TRUE)
    
    if (length( grep( "Wrote:", rpmlog, fixed=TRUE)) == 2) {
        cat("Successfully created rpm package\n")
        cat("Successfully created rpm package\n", file=errorlog, append=TRUE)
        print(rpmlog[grep("Wrote:", rpmlog, fixed=TRUE)])
        for (line in  rpmlog[grep("Wrote:", rpmlog, fixed=TRUE)]){
            cat(line, "\n", file=errorlog, append=TRUE)
        }
        cat("Succeded ", as.character(Sys.time()), "\n", file=errorlog, append=TRUE)
    }
    else {
        cat( "Failed to build ", packname, " in second run rpmbuild\n", sep="")
        return("Failed: Unknown error second run rpmbuild")
    }
    return(c(packname, version, specfile, rpmname))
}
