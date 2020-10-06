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


#' logger writes a character string in the console and append it to file
#' If msg is more than one line, only written to file
#' @param msg character string
#' @param file to write to
#' @return no return value
logger <- function( msg, file=getOption("c2o.logfile")) {
    if (length(msg) == 1)  cat( msg, "\n")
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
