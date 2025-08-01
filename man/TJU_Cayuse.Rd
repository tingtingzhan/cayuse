% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TJU_Cayuse.R
\name{TJU_Cayuse}
\alias{TJU_Cayuse}
\alias{viewAward}
\alias{viewProposal}
\title{Award & Effort from Cayuse}
\usage{
viewAward(path = "~/Downloads", fiscal.year = year(Sys.Date()))

viewProposal(path = "~/Downloads", fiscal.year = year(Sys.Date()))
}
\arguments{
\item{path}{\link[base]{character} scalar, directory of downloaded award \code{.csv} file.
Default is the download directory \code{'~/Downloads'}}

\item{fiscal.year}{\link[base]{integer} scalar}
}
\value{
..
}
\description{
Print out grant and effort from Cayuse.
}
\details{
\itemize{
\item {go to \verb{https://jefferson.cayuse424.com/sp/index.cfm}}
\item {My Proposals -> Submitted Proposals.
Lower-right corner of screen, 'Export to CSV'.
Downloaded file has name pattern \code{'^proposals_.*\\\\.csv'}}
\item {My Awards -> Awards (\emph{not} 'Active Projects').
Lower-right corner of screen, 'Export to CSV'.
Downloaded file has name pattern \code{'^Awards_.*\\\\.csv'}}
\item {My Awards -> Awards.  Click into each project, under 'People' tab to find my
'Sponsored Effort'}
}

Function \link{viewAward} aggregates grant over different period
(e.g. from Axx-xx-001, Axx-xx-002, Axx-xx-003 to Axx-xx).
Then we need to manually added in our 'Sponsored Effort' in the returned \code{.csv} file.
}
\examples{
if (FALSE) {
viewAward()
viewProposal() # 2025 Review: finished Row `17`, Hallgeir Rui, then system dies
}

}
