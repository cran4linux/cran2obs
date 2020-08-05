#' statusOBS checks if a package already exists either as remote pac on build.opensuse.org
#'
#' @param pkg CRAN R package name
#' @param remoteproj Existing project to build packages for in OBS (build.opensuse.org)
#'
#' @return Version number of package in OBS or NA
#' @export

statusOBS <- function(pkg, remoteproj=getOption("c2o.auto")){
    suppressWarnings(
        osclist <- system2("osc", args=c("ls", paste0(remoteproj,"/R-",pkg)), stdout=TRUE, stderr=TRUE)
    )
    if ( is.null( attributes( osclist))) {
        return( gsub( ".tar.gz", "", gsub( paste0( pkg, "_"), "", osclist[ grep( "tar.gz", osclist)]))) # returns the version 
    } else {
        return(NA)
    }
}


#' updateStatusOfpkg incorporates new information about a tried build in
#' the status dataframe.
#' @param status the dataframe containing status information about remoteprj
#' @param pkg CRAN package name which has new information
#' @param version latest version tried
#' @param success if or not the build was successful
#'
#' @return updated status dataframe
#' 
#' @export
updateStatusOfpkg <- function( status, pkg, syncresult, always.safe=TRUE, file= getOption("c2o.statusfile"), log=getOption("c2o.logfile")){
    if (! pkg %in% status[, "Package"]) {
        msg <- paste0("Seems ", pkg, " has no status")
        logger(msg, log)
        return(list(status="fail", value=msg))
    }
    i <- which ( status$Package == pkg )
    if (syncresult$status == "done") {
        status[i , "OBSVersion"] <- syncresult$value
    }
    status[i , "triedVersion"] <- syncresult$value
    if (always.safe) write.table(status, file=file, row.names=FALSE, sep=";")
    return(status)
}


#' repoStatusCreate creates a datafram from available.packages() and
#' available.packages.OBS() to have all the neccessary information to sync
#' the given CRAN mirror to the given OBS repo
#' @param cran is a CRAN mirror
#' @param repo is an OBS repo
#' @param file is the name of the file which will store the information
#' @param overwrite is a flag if an existing status file shall be overwritten
#'
#' @return a datafram containing the columns "Package", "Version", "License",
#' "NeedsCompilation", "recDep", "Suggests", "depLen", "OBSpkg", "File", "OBSVersion",
#' "triedVersion"
#'
#' @export
repoStatusCreate <- function(cran=getOption("c2o.cran"), repo=getOption("c2o.auto"),
                                  file=getOption("c2o.statusfile"), overwrite=FALSE ) {
    if (file.exists(file) && !overwrite  ){
        stop("Status file exists and overwrite='FALSE'!") 
    }
    cranstatus <- cleanDeps(cran)
    repostatus <- available.packages.OBS(obsproject=repo)
    status <- cbind( merge( cranstatus, repostatus, by="Package" , all=TRUE ), triedVersion=NA)
    write.table(status, file=file, row.names=FALSE, sep=";")
    return(status)
}

#' repoStatusUpdate takes a status file and incorporates information of
#' new packages, not in the file both from available.packages() and
#' OBS.
#' @param cran is a CRAN mirror
#' @param repo is an OBS repo
#' @param file is the name of the file which will store the information
#' @param overwrite is a flag if an existing status file shall be overwritten
#'
#' @return a dataframe containing the columns "Package", "Version", "License",
#' "NeedsCompilation", "recDep", "Suggests", "depLen", "OBSpkg", "File", "OBSVersion",
#' "triedVersion"
#'
#' @export
repoStatusUpdate <- function(cran=getOption("c2o.cran"), repo=getOption("c2o.auto"),
                                  file=getOption("c2o.statusfile"), new.file=NA, overwrite=FALSE ) {
    if ( file.exists(file) & is.na(new.file) & !overwrite  ){
        stop("Status file exists, overwrite='FALSE' and no alternative filename given!") 
    }

    if ( !file.exists(file)){
        stop("Status file does not exist")
    }

    
    oldstatus <- read.table( file, header=TRUE, sep=";")
    if (! is.na(new.file) ) file=new.file
    
    ## first merge new info from available packages
    
    ap <- as.data.frame(available.packages(repos=cran))

    newpkgs <- setdiff( ap$Package, oldstatus$Package )
    removedpkgs <- oldstatus$Package[ which(! oldstatus$Package %in% ap$Package)] ## no longer in available.packages
    
    status <- merge( ap[, c("Package", "Version", "File", "License", "NeedsCompilation")],
                       oldstatus[, c("Package", "recDep", "Suggests", "depLen", "OBSVersion", "triedVersion" )], by="Package", all=TRUE)

    if ( length( removedpkgs > 0)) { ## we keep them in repo as long, as they build
        pkgnumbers <- which(status$Package %in% removedpkgs)
        for (pkg in pkgnumbers ) { # not found in CRAN
            if (! is.na( status$OBSVersion)) {
                status[ pkg , "Version"] <- NA
            } 
        }
        emtpypkgs <- pkgnumbers[ which( is.na(status$Version[pkgnumbers]) &  is.na(status$OBSVersion[pkgnumbers]))]
        if (length(emptypkgs) > 0) status <- status[ emptypkgs , ]
    }
    
    if ( length( newpkgs > 0)) {
        for (pkg in which(status$Package %in% newpkgs) ) {
            status[ pkg , "recDep"]   <- cleanList( status$Package[pkg], "depends", repo=cran)
            status[ pkg , "Suggests"] <- cleanList( status$Package[pkg], "suggests", repo=cran)
            status[ pkg , "depLen"]   <- length( unlist( strsplit( status[ pkg, "recDep"], " ")))
        }
    }

    ## now check for new in repo
    cmd <- paste("osc ls", repo, sep=" ", collapse="")
    obspkgs <- gsub("R-", "", system(cmd, intern=TRUE))


    for (pkg in which( is.na(status$OBSVersion))) { ## do not know about version in OBS, check if correct
        if (is.na(status$triedVersion[pkg])  ||
            ( (! is.na(status$triedVersion[pkg])) & (status$Version[pkg] != status$triedVersion[pkg]))) {
            if ( status$Package[pkg] %in% obspkgs  ) { ## seems someone somewhere else has built the package
                obstatus <- getOBSVersion( status$Package[pkg], repo)
#                status$File[pkg] <- obsstatus[1]
                status$OBSVersion[pkg] <- obsstatus[2]
                status$triedVersion <- obsstatus[2]
            }
        }
    }
    
    write.table(status, file=file, row.names=FALSE, sep=";")
    return(status)
}


