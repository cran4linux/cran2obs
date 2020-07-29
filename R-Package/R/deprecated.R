#' CranOBSstatus enriches cleanDeps for all packages in CRAN with their version numbers
#' in OBS
#' @param quiet If set to FALSE some progress info is given, default = TRUE.
#' @param cran If not NULL a matrix like returned from cleanDeps() must be given.
#' If NULL cleanDeps() is called.
#' @param obs If not NULL a matrix like returned from available.packages.OBS() must be given. If NULL
#' that function is called.
#' @param oldstatus If not NULL the name of a csv file containing the result of the latest run of this
#' function. It is used to cut down the time accessing OBS to find which packages are uptodate already.
#' @return A dataframe like cleanDeps with additional infos for OBS packages
#' 
#' @export
CranOBSstatus <- function(quiet=TRUE, cran=getOption("c2o.cran"), obs=getOption("c2o.auto"), oldstatus=NULL){
    if (is.null(cran)) cran <- cleanDeps(cran)
    if (is.null(obs))  obs <- available.packages.OBS(obsproject=obs, quiet=quiet)
    status <- merge( cran, obs, by="row.names" , all.x=TRUE )
    status$Row.names <- NULL
    for (col in 1:dim(status)[2]) status[,col] <- as.character(status[,col])  
    return(status)
}


#' createOBSstatus creates a table for all packages in OBS with version numbers
#' of CRAN and OBS
#' return OBSstatus A dataframe containing esp. version information from CRAN for all OBS packages
#' @param quiet If set to FALSE some progress info is given, default = TRUE.
#' @param cran If not NULL a matrix like returned from cleanDeps() must be given. If NULL cleanDeps()
#' is called.
#' @param obs If not NULL a matrix like returned from available.packages.OBS() must be given. If NULL
#' that function is called.
#' @export
createOBSstatus <- function(quiet=TRUE, cran=NULL, obs=NULL){
    if (is.null(cran)) cran <- cleanDeps(getOption("c2o.cran"))
    if (is.null(obs))  obs <- available.packages.OBS(obsproject=getOption("c2o.auto"), quiet=quiet)
    status <- merge( obs, cran, by="row.names" , all.x=TRUE )
    status$Row.names <- NULL
    for (col in 1:dim(status)[2]) status[,col] <- as.character(status[,col])  
    return(status)
}
    

#' This function generates the complete set of dependencies, give a
#' vector of package names.
#' This is useful i.e. to build rstudio, to have a simple way
#' to generate the complete list of recursive dependencies.
#' @param pkglist A vector of package names
#' @param ap A data structure like returned from cleanDeps, must
#' contain a column "recDep" containing recursive dependencies for
#' a given package. If NULL will be generated.
#' @export
depUnion <- function(pkglist, ap=NULL) {
    if (is.null(ap)) ap <- cleanDeps()
    if (sum( pkglist %in% ap[,"Package"] ) < length(pkglist )) {
        stop("Information on dependencies missing for some packages")
    }
    allDeps <- unique(
        unlist(
            strsplit(
                paste(ap[pkglist, "recDep"], collapse=" "), " " )))
}
