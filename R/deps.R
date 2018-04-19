#' This function returns a structure like available.package()
#' but including recursive dependencies (Depends, Imports, LinkingTo).
#' Because recommended packages may be assumed to be installed
#' in a OpenSUSE installation with R, these are removed together with
#' base packages from the dependencies.
#' 
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
                             (rmpkgs[, "Priority"] == "recommended")), "Package"]
    ## base and recommended always already installed

    ap <- ap[, c("Package", "Version", "License", "License_is_FOSS", "License_restricts_use", "OS_type", "NeedsCompilation")]
    
    depn <- lapply( dep, function(x) x <- x[ ! x %in% rmpkgs  ]  )
    ## remove rmpkgs from dependency list
    
    ap <- cbind(ap, recDep=as.vector(unlist(lapply(depn, paste0, collapse=" "))))
    ## new column with recursive dependencies
}

#' This function generates an analogous matrix to available.packages()
#' but for all the packages in OBS
#' @param obsproject Project in OBS where packages are taken from.
#' @param quiet If set to FALSE some progression info is given, default = TRUE.
#' @return Matrix of all R packages avaialble in OBS devel:languages:R:released
#' @export
available.packages.OBS <- function(obsproject="devel:languages:R:released", quiet=TRUE){
    ## the names first
    cmd <- paste("osc ls", obsproject, sep=" ", collapse="")
    obspkgs <- system(cmd, intern=TRUE)
    ## contains some additional stuff only related to compiling some packages
    obspkgs <- obspkgs[grep("R-", obspkgs)]
    ## all packages start with "R-" by convention, see page in build service
    if (obsproject == "devel:languages:R:released") {
        ## other OBS Project shouldn't contain non-R
        obspkgs <- obspkgs[-which(obspkgs=="R-base")]
        ## R-base not an R package
        obspkgs <- obspkgs[-which(obspkgs == "R-base-java")]
        ## R-base-java is not a CRAN package.
    }
    ##obspkgs <- obspkgs[1:10] 
    ##debugging
    
    cranpkgnames <- gsub("R-", "", obspkgs)
                                        #    obsinfo <- sapply(cranpkgnames, getOBSVersion, USE.NAMES=FALSE, obsproject=obsproject)
    obsinfo <- sapply(cranpkgnames, getOBSVersion, obsproject=obsproject)

    obspkgs <- cbind(OBSpkg=obspkgs, File=obsinfo[1,], OBSVersion=obsinfo[2,])
}


#' This function takes the name of an R package (from CRAN) and receives the corresponding
#' information off OBS
#' This function is not exactly cheap run-time wise. Around 0.5s per package.
#' @param pkg A character string containing the name of a R package as found in CRAN
#' @param obsproject Project in OBS where packages are taken from.
#' @param quiet If set to FALSE some progression info is given, default = TRUE.
#' @return A list with components file containing the source file name and
#' version containing the version.
#' @export
getOBSVersion <- function ( pkg, obsproject="devel:languages:R:released", quiet=TRUE ) {
    if (! quiet) cat("Checking ", pkg, "\n")
    cmd <- paste("osc ls ", obsproject," R-", pkg, sep="", collapse="")
    lst <- system( cmd , intern=TRUE )
    srcfile <- lst[grep(paste("^",pkg,sep="", collapse=""), lst)][1]
    ## the last [1] is needed if a package is linked to factory. In lst the files
    ## can appear multiple times then.
    srcversion <- strsplit(gsub(".tar.gz", "", srcfile), "_")[[1]][2]
    c(srcfile, srcversion)
}

#' This functions shows a table for all packages in OBS with version numbers
#' of CRAN and OBS
#' return OBSstatus A dataframe containing esp. version information from CRAN for all OBS packages
#' @param quiet If set to FALSE some progresso info is given, default = TRUE.
#' @param cran If not NULL a matrix like returned from cleanDeps() must be given. If NULL cleanDeps()
#' is called.
#' @param obs If not NULL a matrix like returned from available.packages.OBS() must be given. If NULL
#' that function is called.
#' @export
showOBSstatus <- function(quiet=TRUE, cran=NULL, obs=NULL){
    if (is.null(cran)) cran <- cleanDeps()
    if (is.null(obs))  obs <- available.packages.OBS(obsproject="devel:languages:R:released", quiet=quiet)
    status <- merge( obs, cran, by="row.names" , all.x=TRUE )
    status$Row.names <- NULL
    status
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

#' This function creates an OBS package from an R package
#' @param obsproject Project in OBS where packages are taken from.
#' @param obscodir Directory where the local copy of the OBS package shall be created
#' @export
createNewOBSPackage <- function( obsproject, obscodir="~/OBS/", pkg ){

    opkg <- paste0("R-", pkg)
    
    cmd <- paste0("cd ", obscodir, obsproject, " && osc mkpac " , opkg)
    cmdresult <- system(cmd, intern=TRUE)

#    cmd <- paste0("osc mkpac ", opkg)
#    cmdresult <- system(cmd, intern=TRUE)

    cmd <- paste0("R2rpm --verbose --debug --no-check --no-suggest -p ",pkg)
    cmdresult <- system(cmd, intern=TRUE)

    cmd <- paste0("cp ~/rpmbuild/SOURCES/", pkg,"*tar.gz ~/rpmbuild/SPECS/", opkg,".spec ", obscodir, obsproject, "/", opkg)
    cmdresult <- system(cmd, intern=TRUE)

    cmd <- paste0("cd ", obscodir, obsproject,"/",opkg," && osc addremove")
    cmdresult <- system(cmd, intern=TRUE)

    cmd <- paste0("cd ", obscodir, obsproject,"/",opkg," && osc ci -m 'new package ", opkg,"'")
    cmdresult <- system(cmd, intern=TRUE)
}

#deplength <- sapply( allDep[, "recDep"], function(x) length(strsplit(x," ")[[1]])) 
#for (pno in which(deplength==0)[1:50]) {
#    createNewOBSPackage( "home:dsteuer:CRANtest", pkg=allDep[pno, "Package"])
#    Sys.sleep(10)
#    }
