% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_PHQ.R
\name{get_phq}
\alias{get_phq}
\title{get_phq function}
\usage{
get_phq(dataset, subscales = F, completers = T)
}
\arguments{
\item{dataset}{original dataset "PHQ-9" from the bundle}

\item{subscales}{boolean parameter, if True includes to the returned dataframe moves subscales}

\item{completers}{boolean parameter, if True filters out participants that are not labeled as completers}
}
\value{
either dataframe with 5 columns:
        PIN, response, phq_sum, phq_cat, phq_sev or dataframe with 7 columns: PIN, response, phq_sum, phq_cat, phq_sev, phq_sym_nsom, phq_symp_som
}
\description{
get_phq function
}
