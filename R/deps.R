#' This function returns a structure like available.package()
#' but including recursive dependencies (Depends, Imports, LinkingTo).
#' Because recommended packages may be assumed to be installed
#' in a OpenSUSE installation with R, these are removed together with
#' base packages from the dependencies.
#' @param
#' @return A matrix containing available packages with a new column
#' 'recDep' containing all recursive dependencies
#' @export
cleanDeps <- function(){
    ap <- available.packages()
    ## Option to use predefined CRAN-alike repository should be added.
    dep <- tools::package_dependencies(ap[,"Package"], db =ap,
                                       which = c("Depends", "Imports", "LinkingTo"),
                                       recursive=TRUE, reverse=FALSE )
    ## This takes a few seconds to a minute on a fairly fast computer

    rmpkgs <- installed.packages()
    rmpkgs <- rmpkgs[ which( (rmpkgs[, "Priority"] == "base") |
                             (rmpkgs[,"Priority"] == "recommended")), "Package"]
    ## finde base and recommended already installed

    ap <- ap[, c("Package", "Version", "License", "License_is_FOSS", "License_restricts_use", "OS_type", "NeedsCompilation")]
    
    depn <- lapply( dep, function(x) x <- x[ ! x %in% rmpkgs  ]  )
    ## remove rmpkgs from dependency list
    
    ap <- cbind(ap, recDep=as.vector(unlist(lapply(depn, paste0, collapse=" "))))
    ## new column with recursive dependencies
}

#' This function generates an analogous matrix to available.packages()
#' but for all the packages in OBS
#' @param  If set to FALSE some progression info is given, default = TRUE.
#' @return Matrix of all R packages avaialble in OBS devel:languages:R:released
#' @export
available.packages.OBS <- function(quiet=TRUE){
    ## the names first
    obspkgs <- system("osc ls devel:languages:R:released", intern=TRUE)
    ## contains some additional stuff only related to compiling some packages
    obspkgs <- obspkgs[grep("R-", obspkgs)]
    ## all packages start with "R-" by convention, see page in build service
    obspkgs <- obspkgs[-which(obspkgs=="R-base")]
    ## R-base not an R package
    obspkgs <- obspkgs[-which(obspkgs == "R-base-java")]
    ## R-base-java is not a CRAN package.
    
    cranpkgnames <- gsub("R-", "", obspkgs)
    obsinfo <- sapply(cranpkgnames, getOBSVersion, quiet=quiet)

    obspkgs <- cbind(obspkgs, obsinfo["OBSFile",], obsinfo["OBSVersion",])
}


#' This function takes the name of an R package (from CRAN) and receives the corresponding
#' information off OBS
#' This function is not exactly cheap run-time wise. Around 0.5s per package.
#' @param pkg A character string containing the name of a R package as found in CRAN
#' @param quiet If set to FALSE some progression info is given, default = TRUE.
#' @return A list with components file containing the source file name and
#' version containing the version.
#' @export
getOBSVersion <- function ( pkg, quiet=TRUE ) {
    if (! quiet) cat("Checking ", pkg, "\n")
    lst <- system( paste("osc ls devel:languages:R:released R-", pkg, sep="", collapse="") , intern=TRUE )
    srcfile <- lst[grep(paste("^",pkg,sep="", collapse=""), lst)]
    srcversion <- strsplit(gsub(".tar.gz", "", srcfile), "_")[[1]][2]
    list(OBSFile=srcfile, OBSVersion=srcversion)
}

#' This functions shows a table for all packages in OBS with version numbers
#' of CRAN and OBS
#' return OBSstatus A dataframe containing esp. version information from CRAN for all OBS packages
#' @param quiet If set to FALSE some progression info is given, default = TRUE.
#' @export
showOBSstatus <- function(quiet=TRUE){
    cranpkgs <- cleanDeps()
    obspkgs <- available.packages.OBS(quiet=quiet)
    merge( obspkgs, cranpkgs, by="row.names" , all.x=TRUE )
}
    
