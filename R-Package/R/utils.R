#' readStatus reads a statusfile and return the datafram
#' @param statusfile filename of statusfile
#' @return status datafram
#' @export
readStatus <- function(statusfile = getOption("c2o.statusfile")){
    invisible(read.table(statusfile, sep=";", header=TRUE, colClasses="character"))
}

#' writeStatus writes status dataframe to statusfile
#' @param status dataframe with status info
#' @param statusfile file to save info into
#' @export
writeStatus <- function(status, 
                        statusfile = getOption("c2o.statusfile")){
    write.table(status, file=statusfile, sep=";", row.names=FALSE)
}

#' resetPkg clears OBSVersion, triedVersion and hasDevel
#' @param pkg package name
#' @param status dataframe holding status
#' @return updated dataframe holding status.
#' @export
resetPkg <- function(pkg, status){
    i <- which(status$Package == pkg)
    status[i, "OBSVersion"] <- status[i, "triedVersion"] <- status[i, "hasDevel"] <- NA
    invisible(status)
}

#' cleanBuildlog removes useless parts of the output
#' - timestamps get removed
#' - whitespace gets removed
#' - setup info from osc gets removed
#' @param buildlog array of lines resulting from call to osc
#' @return cleaned up version of input
cleanBuildlog <- function(buildlog){
    buildlog <- trimws( gsub( "^\\[.*\\]", "", iconv(buildlog, "UTF-8", "UTF-8", sub="")), which="left")
    startoscsetup <- grep("Scanning", buildlog)
    endoscsetup <- grep("mktexlsr: Done.", buildlog)
    if ( ( length( startoscsetup) == 1) &
         ( length( endoscsetup) == 1)) {
        buildlog[-(startoscsetup:endoscsetup)]
    }
}

#' showPkg return the line of status file for pkg
#' @param pkg pkgname
#' @param status datafram conataining sync state
#'
#' @export
showPkg <- function(pkg, status=readStatus(getOption("c2o.statusfile"))){
    status[ which(status$Package == pkg), ]
}

#' logger writes a character string in the console and append it to file
#' If msg is more than one line, only written to file
#' @param msg character string
#' @param file to write to
#' @return no return value
logger <- function( msg, file=getOption("c2o.logfile")) {
    cat( msg[1], "\n")
    for ( line in msg) {
        cat( line, "\n", file=file, append=TRUE)
    }
}

#' obsVersion turns source version in OBS Version
#' (just for readability) 
#' @param version character string representing CRAN version
#'
#' @return normalized OBS Version
obsVersion <- function(version){ gsub("-", ".", version) }

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
