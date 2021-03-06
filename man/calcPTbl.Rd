% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcBootEst.R
\name{calcPTbl}
\alias{calcPTbl}
\title{Calculate p value for a table}
\usage{
calcPTbl(bootData, origData, method, altSide, nullValue)
}
\arguments{
\item{bootData}{dataframe. Must have character column split, defining
groups, and bootStat/bootTStat, giving \code{B} bootstrap sample estimates for each
group in \code{split} of statistic for which confidence interval is required.}

\item{origData}{dataframe. Must have character column split, defining
groups, and origStat/origTStat, giving original sample estimate of
statistic for which p-value is required.}

\item{method}{character. The method specified by the argument \code{pMethod}
in the \code{ggbootUV} function.}

\item{altSide}{"both", "low" or "high". Specifies side(s) of null hypothesis alternative
hypothesis lies.}

\item{calcStat}{function. Function that calculates the statistic for
which a confidence interval is required.}
}
\value{
A dataframe with columns split and p.
}
\description{
Calculate p value for each subgroup in a table
}
