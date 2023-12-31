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
#' @export
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

#' resetpac removes a pac completely and removes the
#' line from the status.
#' pkg will be handled as new in next status construction.
#' 
#' @param pkg CRAN package name
#' @param localOBS base directory for osc checkouts
#' @param remoteprj the project for which packages are built
#' @param cran CRAN mirror for download
#' @param statusfile file holding the current sync status between
#' CRAN and remoteprj
#' @return list "status" and "value"
#' @export
resetpac <- function(pkg,
                     localOBS  = getOption("c2o.localOBSdir"),
                     remoteprj = getOption("c2o.auto"),
                     statusfile = getOption("c2o.statusfile"),
                     log       = getOption("c2o.logfile")){

    cmd <- paste( "\"", "cd", file.path(localOBS, remoteprj) , " && osc delete --force ", paste0( "R-", pkg), "\"")
    result <- system2( "bash", args = c("-c", cmd), stdout=TRUE, stderr=TRUE)

    if( ! is.null(attributes(result))) {
        logger(paste0( pkg, " could not clean up local pac"), log)
        return( list( status="fail", value="could not clean up local pac"))
    }

    cmd <- paste("\"", "rm -rf", file.path(localOBS, remoteprj, paste0( "R-", pkg)), "\"")
    result <- system2( "bash", args = c("-c", cmd), stdout=TRUE, stderr=TRUE)

    if( ! is.null(attributes(result))) {
        logger(paste0( pkg, " could not remove local pacdir"), log)
        return( list( status="fail", value="could not remove local pacdir"))
    }
    cmd <- paste("\"", "osc rdelete ", remoteprj, " ", paste0("R-", pkg), " -m 'delete pkg' \"")
    result <- system2( "bash", args = c("-c", cmd), stdout=TRUE, stderr=TRUE)
    if( ! is.null(attributes(result))) {
        logger(paste0( pkg, " could not remove remote pac"), log)
        return( list( status="fail", value="could not clean up local pac"))
    }
    
    status <- read.table(statusfile, sep=";", header=TRUE, colClasses="character")
    status <- status[-which(status$Package == pkg),]
    write.table(status, file=statusfile, row.names=FALSE, sep=";")
    
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
        logger(paste0( pkg, " could not clean up local pac"), log)
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
#' @return list( status, buildtype, value) where 'status' is "done" or "fail",
#' buildtype is "first" or "update", 'value' is the created directory
#' or a string with an error message 
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

#    inOBSVersion <-  status[ which( status$Package == pkg) , "OBSVersion"]

    ## if there is pac dir, remove it anyways
    if ( dir.exists( pac )){
        cmd <- paste("\"", "cd", file.path(localOBS, remoteprj) , "&& rm -rf  ", paste0( "R-", pkg)  , "\"")
        result <- system2(  "bash",  args = c("-c", cmd), stdout=TRUE, stderr=TRUE)
            
        if( ! is.null(attributes(result))) {
            cat(result, "\n")
            logger(paste0(pkg, " could not rm dir"))
            return(list(status="fail", value="could not remove dir"))
        }
    }

    ## try to checkout pac
    cmd <- paste( "\"", "cd", file.path( localOBS, remoteprj), " && osc co ", paste0( "R-", pkg), "\"")
    result <- system2( "bash",  args = c("-c", cmd), stdout=TRUE, stderr=TRUE)
    if( !is.null(attributes(result)) ) {
        if (! any( grepl( "Package not found", result))) {
            logger(result)
            logger(paste0( pkg, " could not checkout from OBS"))
            return( list( status="fail", value="could not check out"))
        } else { ## package not found, ergo new
            buildtype <- "first"
        }
    } else { ## co was possible
        buildtype <- "update"
    }

    if (buildtype == "first") { ## i.e. no version in OBS
        ## create dir to hold package for OBS
        cmd <- paste("\"", "cd", file.path( localOBS, remoteprj), " ; osc mkpac ", paste0( "R-", pkg)  , "\"")
        suppressWarnings(
            result <- system2(  "bash",  args = c("-c", cmd), stdout=TRUE, stderr=TRUE)
        )
        if( ! is.null(attributes(result))) {
            logger(paste0( " Problem creating pac for ", pkg, " Error: ", result))
            if ( any( grepl( "already under version control", result))) {
                ## try to free pkg from version control
                cmd <- paste("\"", "cd", file.path( localOBS, remoteprj), " ; osc delete --force ",paste0( "R-", pkg)  , "\"")
                result <- system2(  "bash",  args = c("-c", cmd), stdout=TRUE, stderr=TRUE)
                if( ! is.null(attributes(result))) {
                    logger(paste0( " Could not release ", pkg, " from osc control. Error: ", result))
                    return(list(status="fail", value="could not setup pac"))
                }
            } ## released
        
            ## retry mkpac
            cmd <- paste("\"", "cd", file.path( localOBS, remoteprj), " ; osc mkpac ",paste0( "R-", pkg)  , "\"")
            result <- system2(  "bash",  args = c("-c", cmd), stdout=TRUE, stderr=TRUE)
            if( ! is.null(attributes(result))) { ## fail
                logger(paste0( " Could not create pac for ", pkg, ". Error: ", result))
                return(list(status="fail", value="could not setup pac")) 
            }
        }
    }

    ## pac checked out or created, get the sources

    source0 <- paste0( pkg, "_", status[ which( status$Package == pkg), "Version"]   , ".tar.gz")
    ## this is the source file needed for this build
    
    if (buildtype == "update") { ## rm old sources
        oldsources <- list.files( pac, "*.tar.gz")
        oldsources <- oldsources[ oldsources != source0  ]
        ## CRAN packages have one source file in tgz format
        if (length(oldsources) > 0) { ## there are old sources to remove
            ## if (length(oldsource) == 1) { 
            ##     if ( !file.remove( file.path( pac, paste0( pkg, "_", inOBSVersion, ".tar.gz")))){
            ##         logger(paste0(pkg, ": could not rm old sources"))
            ##         return( list( status="fail", value="could not rm old sources"))
            ##     }
            ## } else {
            ##     logger(paste0("more than one source file in pkg ", pkg))
            ##     return( list( status="fail", value="more than one source package"))
            ## }
            for (file in oldsources) {
                if ( !file.remove( file.path( pac, file))){
                    logger(paste0(pkg, ": could not rm old sources"))
                    return( list( status="fail", value="could not rm old sources"))
                }
                else {
                    logger( paste0( pkg,": removed old source file ", file.path( pac, file)))
                }
            }
        }
    }

    if (! file.exists( file.path( pac, source0))) { ## if update because of dependencies, sources may be present already
        if (! file.exists( file.path( download.cache, source0))) { ## may in cache
            if ( class( try( download.file( file.path( cran ,"src/contrib", source0),
                                           file.path( download.cache, source0)))) == "try-error"){
                logger( paste0(pkg, ": sources not found on CRAN"))
                return( list( status="fail", value="no sources found"))
            }
        } 
        if ( !file.copy( file.path( download.cache, source0), file.path( pac, source0), overwrite=TRUE )){
            logger(paste0(pkg, ": copy of sources failed"))
            return( list( status="fail", value="copy of sources failed"))
        }
    }
    
    return( list( status="done", buildtype=buildtype, value=pac))
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
#' @param statusfile a file holding current syncstate
#' @param build.clean default FALSE, wheter obs should be called with --clean
#' 
#' @return list of 'status', "done" or "fail", 'buildtype', 'value' set to NA
#' or character string and 'hasDevel'
#' @export

pkg2pac <- function( pkg,
                    localOBS      = getOption( "c2o.localOBSdir"),
                    remoteprj     = getOption( "c2o.auto"),
                    cran          = getOption( "c2o.cran"),
                    statusfile    = getOption( "c2o.statusfile"),
                    download.cache= getOption( "c2o.download.cache"),
                    binary.cache  = getOption( "c2o.binary.cache"),
                    log           = getOption( "c2o.logfile"),
                    additional.osc.flags = "") {
    
    logger(paste0("* Syncing ", pkg, " to OBS"))
    status <- read.table(statusfile, sep=";", header=TRUE, colClasses="character")

    if ( ! pkg %in% status[ , "Package"] ) {
        logger(paste0( "Seems ", pkg, " not in status file"))
        return( list( status="fail", problem=paste( "Package not in status file")))
    }
    
    pkg.info <- status[ status$Package == pkg, ]
    logger(paste0("Pkg ", pkg, " depLen: ", pkg.info$depLen))
    logger(paste(pkg.info),log)
    
    if ( is.na( pkg.info$Version) ) {
        logger(paste0("Seems ", pkg, " not in CRAN"), log)
        return( list( status="fail", problem=paste( "Package not in CRAN")))
    }

    if ( ! is.na( pkg.info$OBSVersion)) {
        if ( pkg.info$OBSVersion != obsVersion(pkg.info$Version)) {
            logger( paste0( "Update ", pkg), log)
            buildtype = "update"
        } else {
            logger(paste0( pkg, " already uptodate"), log)
            return( list( status="fail", value="already uptodate"))
        }
    } else {
        logger( paste0( "initial build of ", pkg), log)
        buildtype = "initial build"
    }

    ## All recDep of pkg must have an OBSVersion in status, otherwise any try is void
    ## due to binary.cache we can use packages still not publishes in OBS
    if (pkg.info$depLen > 0) {
        deps <- unlist(strsplit(pkg.info$recDep, " "))
        depsinOBS <- intersect( deps, status$Package[ ! is.na(status$OBSVersion) ])
        missing <- setdiff( deps, depsinOBS )
        if ( length(missing) > 0) { ## missing dependencies, no chance
            msg <- "** missing R package dependencies"
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
    buildtype <- result$buildtype
    pac <- result$value
    
    result <- createEmptySpec(pkg, pac=pac, download.cache=download.cache, statusfile=statusfile)

    if (result$status == "fail") {
        logger(paste0("Creating empty spec failed for pkg ", pkg))
        return( list( status="fail", value="creating empty spec failed"))
    }

    specfile <- result$value

### first build
    logger("** build for filelist")
    result <- buildforfiles( pkg, pac, specfile, localOBS=localOBS, remoteprj=remoteprj, download.cache=download.cache, binary.cache=binary.cache, ap=status, log=log, additional.osc.flags = additional.osc.flags)

    if (! result$status == "done") {
        logger( paste0( "Failed initial build, probably missing system lib for ", pkg))
        return( list( status="fail", value=result$value))
    }

    ## here the %file section is fully populated
    specfile <- result$value
    
    ## second build!
    logger("** build with file list")
    result <- testbuild( pkg, pac, specfile, ap=status, log=log, additional.osc.flags = " --no-init " )

    if (result$status == "done") { ## pkg successfully built!
        logger( paste0( pkg, " automatically built"), log)
        syncresult <- list( status="done", buildtype=buildtype, value=obsVersion(pkg.info$Version), hasDevel=FALSE)
        return( syncresult)
    }

    ## there was a build error. Let's see, if it is about splitting in pkg and pkg-devel
    incase.hasDevel <- FALSE ## it may be a -devel package needs rebuilding for some other cause
                             ## to remember the correct value
    
    if (result$value == "split devel") { ## may be the split helps
        develfiles <- extractDevelFilesFromLog(result$buildlog, pkg)
        mainfiles <- extractFilesFromSimpleSpec(specfile)
        mainfiles <- setdiff(mainfiles, develfiles)
        result <- expandSpecForDevel(specfile, mainfiles, develfiles)
        specfile <- result$value
        incase.hasDevel <- TRUE
        logger("** build with -devel package")
        result <- testbuild(pkg, pac, specfile, ap=status, log=log, additional.osc.flags = " --no-init ")
        if (result$status == "done"){
            logger( paste0( pkg, " automatically built with -devel"), log)
            syncresult <- list( status="done", buildtype=buildtype, value=obsVersion(pkg.info$Version), hasDevel=incase.hasDevel)
            return( syncresult)
        }
    }

    if (result$value == "lto-no-text-in-archive") { ## may be adding to Makevars helps
        result <- addMakevarsToSpec(specfile, "mkdir ~/.R/ && echo \"PKG_CFLAGS += -ffat-lto-objects\" > ~/.R/Makevars\n echo \"PKG_CPPFLAGS += -ffat-lto-objects\" >> ~/.R/Makevars")
        specfile <- result$value
        logger("** build with -lto-fat-objects")
        result <- testbuild(pkg, pac, specfile, ap=status, log=log, additional.osc.flags = " --no-init " )
        if (result$status == "done"){
            logger( paste0( pkg, " automatically built with -lto-fat-objects"), log)
            syncresult <- list( status="done", buildtype=buildtype, value=obsVersion(pkg.info$Version), hasDevel=incase.hasDevel)
            return( syncresult)
        }
    }

    if ( result$value == "executable-docs") { ## some docs have the executable bit set
        exefiles <- sapply( strsplit( result$buildlog[ grep("E: executable-docs", result$buildlog)], " "), function(x) x[6])
        result <- rmExeFromDoc( specfile, exefiles)
        specfile <- result$value
        logger("** removed some executable bits")
        result <- testbuild(pkg, pac, specfile, ap=status, log=log,  additional.osc.flags = " --no-init " )
        if (result$status == "done"){
            logger( paste0( pkg, " built after fixing some executable bits in doc"), log)
            syncresult <- list( status="done", buildtype=buildtype, value=obsVersion(pkg.info$Version), hasDevel=incase.hasDevel)
            return( syncresult)
        }
    }

    if ( result$value == "missing-ldconfig") { ## must add %post and %postun sections
        result <- addPost( specfile)
        specfile <- result$value
        logger("** added %post and %postun for included libraries")
        result <- testbuild(pkg, pac, specfile, ap=status, log=log,  additional.osc.flags = " --no-init " )
        if (result$status == "done"){
            logger( paste0( pkg, " built after adding %post and %postun"), log)
            syncresult <- list( status="done", buildtype=buildtype, value=obsVersion(pkg.info$Version), hasDevel=incase.hasDevel)
            return( syncresult)
        }
    }

    ## if we end here, something went badly wrong.
   
    logger( paste0( "Failed to automatically build ", pkg), log)
    syncresult <- list( status="fail", value=result$value)
}
