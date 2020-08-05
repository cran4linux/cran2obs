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
uploadpac <- function(pkg, version, buildtype,
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

    if (buildtype == "update") {
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
                     download.cache = getOption("c2o.download.cache"),
                     status    = getOption("c2o.status"),
                     log       = getOption("c2o.logfile")){
    logger(paste0("** Setting up OBSdir for package ", pkg))

    if (! pkg %in% status[, "Package"]) {
        logger(paste0("Seems ", pkg, " not found on CRAN"))
        return(list(status="fail", value="not found"))
    }
    
    pac <- file.path( localOBS, remoteprj, paste0( "R-", pkg))

    inOBSVersion <-  status[ which( status$Package == pkg) , "OBSVersion"]
    
    if (! is.na( inOBSVersion )) { # update
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
            cmd <- paste( "\"", "cd", file.path( localOBS, remoteprj), " && osc co ", paste0( "R-", pkg), "\"")
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
            
            cmd <- paste("\"", "cd", file.path(localOBS, remoteprj) , "&& osc delete --force ", paste0( "R-", pkg)  , "\"")
            result <- system2(  "bash",  args = c("-c", cmd), stdout=TRUE, stderr=TRUE)
            
            if( ! is.null(attributes(result))) {
                cat(result, "\n")
                logger(paste0(pkg, " could not setup"))
                return(list(status="fail", value="could not remove existing pac"))
            }
        }
        
        ## create dir to hold package for OBS
        cmd <- paste("\"", "cd", file.path( localOBS, remoteprj), " ; osc mkpac ",paste0( "R-", pkg)  , "\"")
        result <- system2(  "bash",  args = c("-c", cmd), stdout=TRUE, stderr=TRUE)
        if( ! is.null(attributes(result))) {
            cat(result)
            cat(pkg, " could not create pac for ", pkg, "\n")
            cat(pkg, " could not create pac for ", pkg, "\n", file=log, append=TRUE)
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
    
    if ( !file.copy( file.path( download.cache, source0), file.path( pac, source0), overwrite=TRUE )){
        cat(pkg, ": copy of sources failed\n")
        cat(pkg, ": copy of sources failed\n", file=log, append=TRUE)
        return( list( status="fail", value="copy of soources failed"))
    }
    
    if (! is.na( inOBSVersion)){ # it is an update, there must be an old source file, just remove it
        if ( !file.remove( file.path( pac, paste0( pkg, "_", inOBSVersion, ".tar.gz")))){
            logger(paste0(pkg, ": could not rm old sources"))
            return( list( status="fail", value="coud not rm old sources"))
        }
    }
    return( list( status="done", value=pac))
}

#' pkg2pac takes the name of an R package and creates or updates 
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

pkg2pac <- function( pkg,
                      localOBS  = getOption( "c2o.localOBSdir"),
                      remoteprj = getOption( "c2o.auto"),
                      cran      = getOption( "c2o.cran"),
                      ap        = getOption( "c2o.status"),
                      download.cache = getOption( "c2o.download.cache"),
                      binary.cache = getOption( "c2o.binary.cache"),
                      log       = getOption( "c2o.logfile")) {
    
    logger(paste0("** Syncing ", pkg, " to OBS"))

    if ( ! pkg %in% ap[ , "Package"] ) {
        cat( "Seems ", pkg, " not in status file\n")
        cat( "Seems ", pkg, " not in status file\n", file=log, append=TRUE)
        return( list( status="fail", problem=paste( "Package not in status file")))
    }
    
    pkg.info <- ap[ ap$Package == pkg, ]
    logger("pkg info", log)
    logger(paste(pkg.info),log)
    
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
        logger( paste0( "initial build of ", pkg), log)
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
    
    result <- setuppac( pkg, localOBS=localOBS, remoteprj=remoteprj, cran=cran, status=status,
                       download.cache=download.cache, log=log)

    if ( result$status == "fail") {
        logger(paste0( "Setting up OBS dir failed for pkg ", pkg))
        return( list( status="fail", problem=result$value))
    }

    pac <- result$value
    result <- createEmptySpec(pkg, pac, download.cache=download.cache, ap)

    if (result$status == "fail") {
        logger(paste0("Creating empty spec failed for pkg ", pkg))
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
    result <- uploadpac( pkg, pkg.info$Version, buildtype=buildtype, localOBS=localOBS, remoteprj=remoteprj, log=log)
    if (! result$status == "done") {
        logger( paste0( "Failed to upload ", pkg , " to ", remoteprj), log)
        return( list( status="fail", value="failed to construct files section"))
    }
    logger( paste0( pkg, " uploaded"), log)

    result <- cleanuppac( pkg, localOBS=localOBS, remoteprj=remoteprj, log=log)
    if (! result$status == "done") {
        logger( paste0( "Failed final cleanup for ", pac), log)
        return( list( status="fail", value="failed to cleanup after upload"))
    }
    logger( paste0( pac, "cleaned up"), log)

    return( syncresult)
}
