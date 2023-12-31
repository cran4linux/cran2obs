% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/status.R
\name{repoStatusUpdate}
\alias{repoStatusUpdate}
\title{repoStatusUpdate takes a status file and incorporates information of
new packages, not in the file both from available.packages() and
OBS.}
\usage{
repoStatusUpdate(
  cran = getOption("c2o.cran"),
  repo = getOption("c2o.auto"),
  file = getOption("c2o.statusfile"),
  new.file = NA,
  overwrite = FALSE,
  log = getOption("c2o.logfile")
)
}
\arguments{
\item{cran}{is a CRAN mirror}

\item{repo}{is an OBS repo}

\item{file}{is the name of the file where the information is stored}

\item{new.file}{is the name of the file which will store the information}

\item{overwrite}{is a flag if an existing status file shall be overwritten}

\item{log}{logfile}
}
\value{
a dataframe containing the columns "Package", "Version", "License",
"NeedsCompilation", "recDep", "Suggests", "depLen", "OBSpkg", "File", "OBSVersion",
"hasDevel", "triedVersion"
}
\description{
repoStatusUpdate takes a status file and incorporates information of
new packages, not in the file both from available.packages() and
OBS.
}
