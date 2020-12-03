#' cran2repo tries to sync as much of a CRAN mirror
#' to an OBS repo as possible. Creating und Updating of packages
#' are done in one go in sequence along with increasing recursice
#' deplength. Usually the parameters are set using the options of
#' CRAN2OBS.
#' @param cran CRAN mirror to use
#' @param localOBS root directory to build remoteprj in locally
#' @param repo OBS repo to use
#' @param binary.cache directory where newly built pacs are found
#' @param download.cache directory where source files from cran cached
#' sync status
#' @param log logfile to use
#'
#' @return dataframe containing new syncstatus
#'
#' @export
cran2repo <- function(cran=getOption("c2o.cran"),
                      localOBS = getOption("c2o.localOBSdir"),
                      remoteprj=getOption("c2o.auto"),
                      statusfile = getOption("c2o.statusfile"),
                      download.cache = getOption("c2o.download.cache"),
                      binary.cache = getOption("c2o.binary.cache"),
                      log = getOption("c2o.logfile")){

    sync.start <- Sys.time()
    logger( paste0("Sync started at ", Sys.time() ))
    status <- read.table(statusfile, header=TRUE, sep=";", colClasses="character")

    actions <- defineActions(status)
    
    all.deplength <- sort(unique(as.numeric(status$depLen)))
    total.to.build <- length(actions$update) + length(actions$totry)
    start.time <- Sys.time()
    build.counter <- 0
    
    for (level in all.deplength){
        pkgs <- which(status$depLen == level)
        for (pkg in status$Package[pkgs]) {
            buildtype <- NA
            if (pkg %in% actions$update) {
                buildtype <- "update"
            } else {
                if (pkg %in% actions$totry) {
                    buildtype <- "initial build"
                }
            }
            if (! is.na(buildtype))
            {
                build.counter <- build.counter + 1
                pkg.build.start <- Sys.time()
                logger(paste0("* Working on ", pkg, "( ", build.counter, "/", total.to.build,")" ))
                logger(paste0("  Begin:     ", pkg.build.start))
            } else {
                next
            }
            num <- which(status$Package == pkg)
            result <- pkg2pac(pkg, localOBS=localOBS, remoteprj=remoteprj, statusfile=statusfile,
                              download.cache=download.cache, binary.cache=binary.cache, log=log)

            if (result$status == "done") {
                upresult <- uploadpac( pkg, obsVersion(status$Version[num]), buildtype=result$buildtype, localOBS=localOBS, remoteprj=remoteprj, log=log)
                if (! upresult$status == "done") {
                    logger( paste0( "Failed to upload ", pkg , " to ", remoteprj), log)
                    logger( paste0( "with error ", upresult$value))
                    ## return( list( status="fail", value="failed to upload built package"))
                } else {
                    logger( paste0( pkg, " successfully uploaded"), log)
                    logger( paste0( "** Sync finished for pkg ", pkg))
                }
            } else {
                logger(paste0("** Sync failed for ", pkg))
            }
            status <- updateStatusOfpkg ( status, pkg, result, statusfile=statusfile, log=log) 
            pkg.build.end <- Sys.time()
            logger( paste0("  Work on ",pkg, " ended at ", pkg.build.end, " after ",
                           floor(as.numeric(difftime(pkg.build.end, pkg.build.start, units="sec"))*100)/100, "s" ))
            avg.time <- as.numeric(difftime(pkg.build.end, sync.start,units="secs"))/build.counter
            logger( paste0("  Average time per package: ", floor(avg.time*100)/100, "s" ))
            logger( paste0("  Expected remaining time:  ", floor((total.to.build-build.counter)*avg.time*100)/100, "s" ))
        }
    }
    sync.end <- Sys.time()
    logger( paste0("  Sync finished at ", sync.end, "after ",
                   floor(as.numeric(difftime(sync.end, sync.start, units="auto"))*100)/100, "s" ))
    
    return(status)
}
                                        
#' pkg2repo does the same as cran2repo, but for a singl pkg
#' @param cran CRAN mirror to use
#' @param localOBS root directory to build remoteprj in locally
#' @param repo OBS repo to use
#' @param binary.cache directory where newly built pacs are found
#' @param download.cache directory where source files from cran cached
#' sync status
#' @param log logfile to use
#'
#' @return dataframe containing new syncstatus
#'
#' @export
pkg2repo <- function(pkg,
                     cran=getOption("c2o.cran"),
                     localOBS = getOption("c2o.localOBSdir"),
                     remoteprj=getOption("c2o.auto"),
                     statusfile = getOption("c2o.statusfile"),
                     download.cache = getOption("c2o.download.cache"),
                     binary.cache = getOption("c2o.binary.cache"),
                     log = getOption("c2o.logfile")){

    status <- read.table(statusfile, header=TRUE, sep=";", colClasses="character")
    actions <- defineActions(status)

    logger(paste0("* Working on ", pkg ))
    num <- which(status$Package == pkg)

    if ( (pkg %in% actions$totry) | (pkg %in% actions$update)){
        result <- pkg2pac(pkg, localOBS=localOBS, remoteprj=remoteprj, statusfile=statusfile,
                          download.cache=download.cache, binary.cache=binary.cache, log=log)
        if (result$status == "done") {
            upresult <- uploadpac( pkg, obsVersion(status$Version[num]), buildtype=result$buildtype, localOBS=localOBS, remoteprj=remoteprj, log=log)
            if (! upresult$status == "done") {
                logger( paste0( "Failed to upload ", pkg , " to ", remoteprj), log)
                logger( paste0( "with error ", upresult$value))
                ## return( list( status="fail", value="failed to upload built package"))
            } else {
                logger( paste0( pkg, " successfully uploaded"), log)
                logger(paste0("** Sync finished for ", pkg))
            }

        } else {
            logger(paste0("** Sync failed for ", pkg))
        }
        status <- updateStatusOfpkg ( status, pkg, result, statusfile=statusfile, log=log) 
    } else {
        logger( paste0("Nothing to be done for pkg ", pkg))
    }
    return(status)
}
