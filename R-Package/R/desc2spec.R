
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
    result <- createEmptySpec( pkg, pkgdir, download.cache=download.cache, status=ap)
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
