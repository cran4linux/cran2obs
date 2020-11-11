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
    logger( paste0( "pkgs to try ", totry))

    uptodate <- status$Package[ which(obsVersion( status$Version) == status$OBSVersion) ]
    ##logger( paste0( "pkgs uptodate ", uptodate))
    
    tried <- status$Package[ which(is.na(status$OBSVersion) & ( obsVersion(status$Version) != status$triedVersion  ))  ]
    ##logger( paste0( "pkgs unsuccessful ", tried))
    
    update <- status$Package[ which( !is.na( status$OBSVersion) & ( obsVersion( status$Version) != status$OBSVersion) )]
    logger( paste0( "pkgs to update ", update))

    retired <- status$Package[ is.na(status$Version) ] 
    logger( paste0( "pkgs which retired ", retired))
    
    return(list(retired=retired, uptodate=uptodate, update=update, tried= tried, totry=totry))
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
    logger("Update of pkg status")
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

    logger(paste0("** Retired packages: ", retiredpkgs))
    logger(paste0("** New packages: ", newpkgs))

    updatedpkgs <- status$Package[ which( !is.na( status$OBSVersion) & ( obsVersion( status$Version) != status$OBSVersion) )]
    logger(paste0("** Updated packages: ", updatedpkgs))

    write.table(status, file=file, row.names=FALSE, sep=";")
    logger("new status file written")
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
## repoStatusUpdate <- function(cran=getOption("c2o.cran"),
##                              repo=getOption("c2o.auto"),
##                              file=getOption("c2o.statusfile"),
##                              new.file=NA,
##                              overwrite=FALSE,
##                              log=getOption("c2o.logfile")) {
##     logger("* repoStatusUpdate")
##     if ( file.exists(file) & is.na(new.file) & !overwrite  ){
##         logger("Status file exists, overwrite='FALSE' and no alternative filename given!")
##         stop("Status file exists, overwrite='FALSE' and no alternative filename given!") 
##     }
    
##     if ( !file.exists(file)){
##         logger("Status file does not exist")
##         stop("Status file does not exist")
##     }
    
##     oldstatus <- read.table( file, header=TRUE, sep=";", colClasses="character")
##     if (! is.na(new.file) ) file=new.file
    
##     ## first merge new info from available packages
    
##     ap <- as.data.frame(available.packages(repos=cran))
    
##     newpkgs <- setdiff( ap$Package, oldstatus$Package )
##     ##logger(paste0("New packages: ", newpkgs))
    
##     removedpkgs <- oldstatus$Package[ which(! oldstatus$Package %in% ap$Package)] ## no longer in available.packages
##     ##logger(paste0("Removed packages: ", removedpkgs))
    
##     if ( length( removedpkgs > 0)) { ## we keep them in repo as long, as they build
##         ## but if not in OBS, remove every mention of package.
##         pkgnumbers <- which( (oldstatus$Package %in% removedpkgs) & is.na(oldstatus$OBSVersion) )
##         if (length(pkgnumbers) > 0){
##             oldstatus <- oldstatus[-pkgnumbers,]
##         }
##     }
    
##     status <- merge( ap[, c("Package", "Version", "License", "NeedsCompilation")],
##                     oldstatus[, c("Package", "recDep", "Suggests", "depLen", "OBSVersion", "hasDevel", "triedVersion" )], by="Package", all=TRUE)
    

    
##     retiredpkgs <- oldstatus$Package[ which(! oldstatus$Package %in% ap$Package)] ## no longer in available.packages, but in OBS
##     logger(paste0("** Retired packages: ", retiredpkgs))
    
##     if ( length( retiredpkgs) > 0) { ## we keep them in repo as long, as they build
##         pkgnumbers <- which( status$Package %in% retiredpkgs)
##         for (pkg in pkgnumbers ) { # not found in CRAN
##             status[ pkg , "Version"] <- NA
##         }
##     }
    
##     logger(paste0("** New packages: ", newpkgs))
##     if ( length( newpkgs > 0)) {
##         for (pkg in which(status$Package %in% newpkgs) ) {
##             logger( paste0( "Dependencies for pkg ", status$Package[pkg]))
##             status[ pkg , "recDep"]   <- cleanList( status$Package[pkg], "depends", repo=cran)
##             logger( paste0( " Depends: ",  status[ pkg , "recDep"]))
##             status[ pkg , "Suggests"] <- cleanList( status$Package[pkg], "suggests", repo=cran)
##             logger( paste0(" Suggests: ",  status[ pkg , "Suggests"]))
##             status[ pkg , "depLen"]   <- length( unlist( strsplit( status[ pkg, "recDep"], " ")))
##             logger( paste0(" depLen: ",  status[ pkg , "depLen"]))
##         }
##     }

##     action <- defineActions(status)
##     logger(paste0("** Updated packages: ", action$update))
    
##     if ( length( action$update) > 0) {
##         reversecalc <- c()
##         for (pkg in which(status$Package %in% action$update) ) {
##             oldeps <- status$recDep[pkg]
##             logger( paste0( "Dependencies for pkg ", status$Package[pkg]))
            
##             status[ pkg , "recDep"]   <- cleanList( status$Package[pkg], "depends", repo=cran)
##             logger( paste0( " Depends: ",  status[ pkg , "recDep"]))
            
##             if (oldeps != status$recDep[pkg]) reversecalc <- c(reversecalc, status$Package[pkg])
            
##             status[ pkg , "Suggests"] <- cleanList( status$Package[pkg], "suggests", repo=cran)
##             logger( paste0(" Suggests: ",  status[ pkg , "Suggests"]))
            
##             status[ pkg , "depLen"]   <- length( unlist( strsplit( status[ pkg, "recDep"], " ")))
##             logger( paste0(" depLen: ",  status[ pkg , "depLen"]))
##         }
##     }

##     logger( paste0( "Packages of which reversedependencies must be calculated: ", length(reversecalc)))
##     logger( pasteo(reversecalc))

##     pkgseen <- c()
##     while( length(reversecalc) > 0){
##         toppkg <- reversecalc[1]
##         reversecalc <- reversecalc[-1]
##         for (pkg  in which( grepl( toppkg, status$recDep))) {
##             logger( paste0( "recalculate deps of ", status$Package[pkg], ", which is reversedep of ", toppkg))
##             if (! pkg %in% pkgseen) {
##                 oldeps <- status$recDep[pkg]
##                 status[ pkg , "recDep"]   <- cleanList( status$Package[pkg], "depends", repo=cran)
##                 pkgseen <- c(pkgseen, pkg)
##             } else {
##                 logger( paste0( "Dependencies for ", status$Package[pkg], "already re-caclulated. Skipping."))
##             }
##         }
##     }


##     ## now check for new and successfully built in repo
##     cmd <- paste("osc prjresults -V", repo, "-r openSUSE_Tumbleweed -a x86_64 | grep '^\\.'  ",   sep=" ", collapse="")
##     obspkgs <- gsub(".  R-", "", system(cmd, intern=TRUE))

##     for (pkg in which( is.na(status$OBSVersion) & (status$Package %in% obspkgs)  )){
##         logger(paste0("Get OBS info for package ",status$Package[pkg] ))
##         obsinfo <- getOBSVersion( status$Package[pkg], repo)
##         status$OBSVersion[pkg] <- obsinfo$version
##         status$triedVersion[pkg] <- obsinfo$version
##         status$hasDevel[pkg] <- obsinfo$hasDevel
##     }
    
##     write.table(status, file=file, row.names=FALSE, sep=";")
##     logger("new status file written")
##     return(status)
## }


