% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convenience.r
\name{detect_function_conflicts}
\alias{detect_function_conflicts}
\title{Detect function conflicts between packages}
\usage{
detect_function_conflicts()
}
\value{
A list of character vectors of conflicting functions (invisibly)

Mostly useful for the side-effect of printing conflicting functions.

This function draws heavily from \code{\link[tidyverse:tidyverse_conflicts]{tidyverse::tidyverse_conflicts()}}, except
it finds any conflicts (not just with tidyverse packages), and it's got no
dependencies beyond base R (>= 3.0ish)

Note that this function detects conflicts between \emph{attached} packages, not
packages and user-defined functions, or packages that haven't been attached.
}
\description{
Detect function conflicts between packages
}
