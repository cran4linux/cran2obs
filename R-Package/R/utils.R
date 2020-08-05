#' logger writes a character string in the console and append it to file
#' If msg is more than one line, only written to file
#' @param msg character string
#' @param file to write to
#' @return no return value
logger <- function( msg, file=getOption("c2o.logfile")) {
    if (length(msg == 1))  cat( msg, "\n")
    for ( line in msg) {
        cat( line, "\n", file=file, append=TRUE)
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
