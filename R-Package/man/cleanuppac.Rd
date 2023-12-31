% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pac.R
\name{cleanuppac}
\alias{cleanuppac}
\title{cleanuppac removes a pac directory and removes the
pkg from the local build service database.}
\usage{
cleanuppac(
  pkg,
  localOBS = getOption("c2o.localOBSdir"),
  remoteprj = getOption("c2o.auto"),
  log = getOption("c2o.logfile")
)
}
\arguments{
\item{pkg}{CRAN package name}

\item{localOBS}{base directory for osc checkouts}

\item{remoteprj}{the project for which packages are built}

\item{cran}{CRAN mirror for download}

\item{status}{dataframe holding the current sync status between
CRAN and remoteprj}
}
\value{
list "status" and "value"
}
\description{
cleanuppac removes a pac directory and removes the
pkg from the local build service database.
}
