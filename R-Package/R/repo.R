#' cran2repo tries to sync as much of a CRAN mirror
#' to an OBS repo as possible. Creating und Updating of packages
#' are done in one go in sequence along with increasing recursice
#' deplength. Usually the parameters are set using the options of
#' CRAN2OBS.
#' @param cran CRAN mirror to use
#' @param localOBS root directory to build remoteprj in locally
#' @param repo OBS repo to use
#' @param status dataframe containing status information about
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
                      status= getOption( "c2o.status"),
                      log = getOption("c2o.logfile")){
    all.deplength <- sort(unique(status$depLen))
    for (level in all.deplength){
        pkgs <- which(status$depLen == level)
        for (pkg in status$Package[pkgs]) {
            logger(paste0("* Working on ", pkg ))
            result <- pkg2pac(pkg, localOBS=localOBS, remoteprj=remoteprj, ap=status,
                              download.cache=download.cache, binary.cache=binary.cache, log=log)
            if (result$status == "done") {
                logger(paste0("** Sync finished for ", pkg))
                uploadpac(pkg, status$Version[pkg], "initial build")
            } else {
                logger(paste0("** Sync failed for ", pkg))
            }
            status <- updateStatusOfpkg ( status, pkg, result, file=statusfile, log=log) 
        }
    }
    return(status)
}
