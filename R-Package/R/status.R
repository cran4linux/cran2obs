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
#' @param status datframe holding status information about remoteprj
#' @param pkg CRAN package name which has new information
#' @param syncresult holds a list with components status, version and hasDevel
#' @param always.safe boolean that defines if statusfile should be written for
#' every package
#' @param statusfile file containing status information about ongoing sync
#' @param log logfile
#'
#' @return updated status dataframe
#' 
#' @export
updateStatusOfpkg <- function( status, pkg, syncresult, always.save=TRUE, statusfile= getOption("c2o.statusfile"), log=getOption("c2o.logfile")){
    logger("Update of pkg status")
    if (! pkg %in% status[, "Package"]) {
        msg <- paste0("Seems ", pkg, " has no status")
        logger(msg, log)
        return(list(status="fail", value=msg))
    }
    i <- which ( status$Package == pkg )
    if (syncresult$status == "done") {
        status[i , "OBSVersion"] <- status[i, "Version" ]
        status[i , "hasDevel"] <- syncresult$hasDevel
    }
    status[i , "triedVersion"] <- status[i, "Version" ]
    if (always.save) write.table(status, file=statusfile, row.names=FALSE, sep=";")
    return(status)
}


#' repoStatusCreate creates a dataframe from available.packages() and
#' available.packages.OBS() to have all the neccessary information to sync
#' the given CRAN mirror to the given OBS repo
#' @param cran is a CRAN mirror
#' @param repo is an OBS repo
#' @param file is the name of the file which will store the information
#' @param overwrite is a flag if an existing status file shall be overwritten
#'
#' @return a datafram containing the columns "Package", "Version", "License",
#' "NeedsCompilation", "recDep", "Suggests", "depLen", "OBSpkg", "File", "OBSVersion",
#'  "hasDevel", "triedVersion
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
#' @param file is the name of the file where the information is stored
#' @param new.file is the name of the file which will store the information
#' @param overwrite is a flag if an existing status file shall be overwritten
#' @param log logfile
#' 
#' @return a dataframe containing the columns "Package", "Version", "License",
#' "NeedsCompilation", "recDep", "Suggests", "depLen", "OBSpkg", "File", "OBSVersion",
#' "triedVersion"
#'
#' @export
repoStatusUpdate <- function(cran=getOption("c2o.cran"),
                             repo=getOption("c2o.auto"),
                             file=getOption("c2o.statusfile"),
                             new.file=NA,
                             overwrite=FALSE,
                             log=getOption("c2o.logfile")) {
    logger("** repoStatusUpdate")
    if ( file.exists(file) & is.na(new.file) & !overwrite  ){
        logger("Status file exists, overwrite='FALSE' and no alternative filename given!")
        stop("Status file exists, overwrite='FALSE' and no alternative filename given!") 
    }

    if ( !file.exists(file)){
        logger("Status file does not exist")
        stop("Status file does not exist")
    }
    
    oldstatus <- read.table( file, header=TRUE, sep=";", colClasses="character")
    if (! is.na(new.file) ) file=new.file
    
    ## first merge new info from available packages
    
    ap <- as.data.frame(available.packages(repos=cran))

    newpkgs <- setdiff( ap$Package, oldstatus$Package )
    logger(paste0("New packages: ", newpkgs))
    
    removedpkgs <- oldstatus$Package[ which(! oldstatus$Package %in% ap$Package)] ## no longer in available.packages
    logger(paste0("Removed packages: ", removedpkgs))

    if ( length( removedpkgs > 0)) { ## we keep them in repo as long, as they build
        ## but if not in OBS, remove every mention of package.
        pkgnumbers <- which( (oldstatus$Package %in% removedpkgs) & is.na(oldstatus$OBSVersion) )
        oldstatus <- oldstatus[-pkgnumbers,]
    }
    
    status <- merge( ap[, c("Package", "Version", "License", "NeedsCompilation")],
                    oldstatus[, c("Package", "recDep", "Suggests", "depLen", "OBSVersion", "hasDevel", "triedVersion" )], by="Package", all=TRUE)
    
    retiredpkgs <- oldstatus$Package[ which(! oldstatus$Package %in% ap$Package)] ## no longer in available.packages, but in OBS
    logger(paste0("Retired packages: ", retiredpkgs))
    
    if ( length( retiredpkgs > 0)) { ## we keep them in repo as long, as they build
        pkgnumbers <- which( status$Package %in% retiredpkgs)
        for (pkg in pkgnumbers ) { # not found in CRAN
            status[ pkg , "Version"] <- NA
        }
    }
    
    if ( length( newpkgs > 0)) {
        for (pkg in which(status$Package %in% newpkgs) ) {
            logger(paste0("Dependencies for pkg ", status$Package[pkg]))
            status[ pkg , "recDep"]   <- cleanList( status$Package[pkg], "depends", repo=cran)
            status[ pkg , "Suggests"] <- cleanList( status$Package[pkg], "suggests", repo=cran)
            status[ pkg , "depLen"]   <- length( unlist( strsplit( status[ pkg, "recDep"], " ")))
        }
    }

    ## now check for new in repo
    cmd <- paste("osc ls", repo, sep=" ", collapse="")
    obspkgs <- gsub("R-", "", system(cmd, intern=TRUE))


    for (pkg in which( is.na(status$OBSVersion) & (status$Package %in% obspkgs)  )){
        logger(paste0("Get OBS info for package ",status$Package[pkg] ))
        obsinfo <- getOBSVersion( status$Package[pkg], repo)
        status$OBSVersion[pkg] <- as.character(unlist(obsinfo[1,]))
        status$triedVersion[pkg] <- as.character(unlist(obsinfo[1,]))
        status$hasDevel[pkg] <- unlist(obsinfo[2,])
    }
    
    write.table(status, file=file, row.names=FALSE, sep=";")
    logger("new status file written")
    return(status)
}


