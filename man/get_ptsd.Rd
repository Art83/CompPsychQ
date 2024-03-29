% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_ptsd.R
\name{get_ptsd}
\alias{get_ptsd}
\title{get_sds function}
\usage{
get_ptsd(dataset, completers = T)
}
\arguments{
\item{dataset}{original dataset "PC-PTSD-5" from the bundle}

\item{completers}{boolean parameter, if True filters out participants that are not labeled as completers}
}
\value{
dataframe with 3 columns:
        PIN, ptsd_sum, ptsd_cat
}
\description{
get_sds function
}
