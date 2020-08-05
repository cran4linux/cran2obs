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
cran2repo <- function(cran=getOption("c2o.cran"),
                      localOBS = getOption("c2o.localOBSdir"),
                      remoteprj=getOption("c2o.auto"),
                      status= getOption( "c2o.status"),
                      log = getOption("c2o.logfile")){
}
