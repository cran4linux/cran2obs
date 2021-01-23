#' defineActions returns a list of a partition in 5 sets of all
#' packages with status information
#' @param status dataframe read from datafile or constructed
#'
#' @return list with compoents "retired", "uptodate", "update" and
#' "totry" , "tried" holding the names of respective packages
#'
#' @export
defineActions <- function(status = getOption("c2o.status")){
    totry <- status$Package[ which( ( !is.na( status$Version) & is.na( status$OBSVersion)) &
                                    (( is.na( status$triedVersion) |
                                       (obsVersion( status$Version) != status$triedVersion))))]
    logger( paste0( "pkgs to try ", length(totry)))
    logger( paste0( "pkgs to try ", totry))

    uptodate <- status$Package[ which(obsVersion( status$Version) == status$OBSVersion) ]
    ##logger( paste0( "pkgs uptodate ", uptodate))
    
    tried <- status$Package[ which( obsVersion(status$Version) == status$triedVersion  )  ]
    ##logger( paste0( "pkgs unsuccessful ", tried))
    
    update <- status$Package[ which( !is.na( status$OBSVersion) & ( obsVersion( status$Version) != status$OBSVersion) & ( obsVersion(status$Version) != status$triedVersion)  )]
    logger( paste0( "pkgs to update ", length(update)))
    logger( paste0( "pkgs to update ", update))

    retired <- status$Package[ is.na(status$Version) ] 
    logger( paste0( "pkgs which retired ", length(retired)))
    logger( paste0( "pkgs which retired ", retired))
    
    return(list(retired=retired, uptodate=uptodate, update=update, tried=tried,
                totry=totry))
}

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

#' resetStatusOfpkg removes entries for OBSVersion, hasDevel and
#' triedVersion
#' @param pkg a packagename
#' @param status a dataframe containing current status. If NA statusfile
#' will be read
#' @param statusfile file containing status information about ongoing sync
#' @param log logfile
#'
#' @return dataframe holding updated status
#'
#' @export
resetStatusOfpkg <- function( pkg, status=NA, statusfile= getOption("c2o.statusfile"), log=getOption("c2o.logfile")){
    if (is.na(status)) status <- readStatus(statusfile)
    num <- which(status$Package == pkg)
    status$OBSVersion[num] <- status$hasDevel[num] <- status$triedVersion[num] <- NA
    writeStatus(status, statusfile)
    invisible(status)
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
    logger("  Update of pkg status")
    if (! pkg %in% status[, "Package"]) {
        msg <- paste0("Seems ", pkg, " has no status")
        logger(msg, log)
        return(list(status="fail", value=msg))
    }
    i <- which ( status$Package == pkg )
    if (syncresult$status == "done") {
        status[i , "OBSVersion"] <- obsVersion(status[i, "Version" ])
        status[i , "hasDevel"] <- syncresult$hasDevel
    } else {
        status[i , "triedVersion"] <- obsVersion(status[i, "Version" ])
        if (!is.na(status[ i, "OBSVersion"]) & (status[i, "OBSVersion"] == "0")) status[i, "OBSVersion"] <- NA
    }
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
#' "hasDevel", "triedVersion"
#'
#' @export
repoStatusUpdate <- function(cran=getOption("c2o.cran"),
                             repo=getOption("c2o.auto"),
                             file=getOption("c2o.statusfile"),
                             new.file=NA,
                             overwrite=FALSE,
                             log=getOption("c2o.logfile")) {
    logger("* repoStatusUpdate")
    if ( file.exists(file) & is.na(new.file) & !overwrite  ){
        logger("Status file exists, overwrite='FALSE' and no alternative filename given!")
        stop("Status file exists, overwrite='FALSE' and no alternative filename given!") 
    }
    
    if ( !file.exists(file)){
        logger("Status file does not exist")
        stop("Status file does not exist")
    }

    cranstatus <- cleanDeps(cran)
    oldstatus <- read.table( file, header=TRUE, sep=";", colClasses="character")
    if (! is.na(new.file) ) file=new.file
    
    newpkgs <- setdiff( cranstatus$Package, oldstatus$Package )
    
    removedpkgs <- setdiff( oldstatus$Package, cranstatus$Package) ## no longer in available.packages
    logger(paste0("** Removed packages: ", removedpkgs))
    
    if ( length( removedpkgs > 0)) {
        pkgnumbers <- which( (oldstatus$Package %in% removedpkgs) & !is.na(oldstatus$OBSVersion) )
        if (length(pkgnumbers) > 0) {  ## we keep them in repo as long, as they build
            oldstatus[ pkgnumbers, "Version" ] <- NA
            retiredpkgs <- oldstatus$Package[pkgnumbers]
        }
        pkgnumbers <- which( (oldstatus$Package %in% removedpkgs) & is.na(oldstatus$OBSVersion) )
        if (length(pkgnumbers) > 0){   ## but if not in OBS, remove every mention of package.
            oldstatus <- oldstatus[-pkgnumbers,]
        }
        
    }

    status <- merge( cranstatus[, c("Package", "Version", "License", "NeedsCompilation", "recDep", "Suggests", "depLen")],
                    oldstatus[, c("Package", "OBSVersion", "hasDevel", "triedVersion" )],
                    by="Package", all=TRUE)
    logger(paste0("** Number of retired packages: ", length( retiredpkgs)))
    logger(paste0("   Retired packages: ", retiredpkgs))
    logger(paste0("** Number of new packages: ", length( newpkgs)))
    logger(paste0("   New packages: ", newpkgs))

    updatedpkgs <- status$Package[ which( !is.na( status$OBSVersion) & ( obsVersion( status$Version) != status$OBSVersion) )]
    logger( paste0("** Number of *updated* packages: ", length(updatedpkgs)))
    logger( paste0("   Updated packages: ", updatedpkgs))

    ## For reverse dependencies of updated packages
    ## if the updated pgk has a different set of packages as dependencies
    ## all reversedependencies must be updated as well, i.e. need new spec files
    ## this is achieved via setting these packages to OBSVersion "0" and treat them as updates, too.

    upandnewdep <- c()
    for (pkg in updatedpkgs) {
###        cat( pkg , " ",  which(oldstatus$Package == pkg), " ", which( status$Package == pkg),"\n")
###        cat( pkg , " ",  oldstatus$recDep[ which(oldstatus$Package == pkg)], " ", status$recDep[ which( status$Package == pkg)],"\n")
        if ( is.na(oldstatus$recDep[which(oldstatus$Package == pkg)]) | (oldstatus$recDep[ which(oldstatus$Package == pkg)] != status$recDep[ which( status$Package == pkg)])) {
            upandnewdep <- c(upandnewdep, pkg)
        }
    }   

    revdepup <-c()
    for (pkg in upandnewdep) { revdepup <- c( revdepup, status$Package[ which( grepl( pkg, status$recDep ))]) }
    revdepup <- unique(revdepup)        
    
    for (pkg in revdepup) {
        i <- which(status$Package == pkg)
        if (! is.na( status[ i, "OBSVersion"])) status[i, "OBSVersion"] <- "0"
        status[i, "triedVersion"] <- status[i, "hasDevel"] <- NA 
    }
    logger( paste0("** Number of Packages to be rebuilt: ", length(revdepup)))
    logger( paste0("   Packages to rebuild because of dependencies: ", revdepup))
        
    write.table(status, file=file, row.names=FALSE, sep=";")
    logger("new status file written")
    return(status)
}



