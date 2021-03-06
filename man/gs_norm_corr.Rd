% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim.R
\name{gs_norm_corr}
\alias{gs_norm_corr}
\title{Wrapper for faux rnorm_pre}
\usage{
gs_norm_corr(
  .data,
  colname,
  corr_col,
  mu = 0,
  sd = 1,
  r = 0,
  empirical = FALSE
)
}
\arguments{
\item{.data}{(required) the source data}

\item{corr_col}{the  correlated column name}

\item{mu}{desired mean of returned vector}

\item{sd}{desired SD of returned vector}

\item{r}{desired correlation between existing and returned observations}

\item{empirical}{logical. If true, mu, sd and r specify the empirical not population mean, sd and covariance}
}
\description{
Wrapper for faux rnorm_pre
}
