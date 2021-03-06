% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sortPW.R
\name{findSortedTbl}
\alias{findSortedTbl}
\title{Sort x- and y-columns by size of response across facets.}
\usage{
findSortedTbl(data, plotTbl, calcStat)
}
\arguments{
\item{data}{dataframe. Has columns group, resp and facet. The column 'group' specifies individual group,
'resp' response and 'facet' overall facet.}

\item{plotTbl}{dataframe. Has columns .id, x, y and origStat. May have other columns. The column '.id' specifies overall facet,
'x' and 'y' the specific groups compared in a row, and 'origStat' how much larger the variable level in 'y' is than the variable
level in 'x' (at least for \code{ggbootPW}; for other functions it could be the other way around the code would run fine).}
}
\value{
Dataframe with columns along x- and y-axes changed appropriately.
data = data.frame( facet = c( rep( 0, 6 ), rep( 1, 8 ) ),
group = c( 1, 1, 2, 2, 3, 3, 1, 1, 3, 3, 2, 2, 4, 4 ),
resp = c( 0.1, 0.3, 5, 2, -2, 1, 5, 2, 3, 4, 5, 6, 7, 3 ) )
plotTbl = data.frame( .id = c( rep( 0, 3 ), rep( 1, 6 ) ),
x = c( '1', '1', '2', '1', '1', '1', '2', '2', '3' ),
y = c( '2', '3', '3', '2', '3', '4', '3', '4', '4' ),
origStat = c( 0, 5, 3, 2, 5, 4, 2, 10, 10 ) )
findSortedTbl( data, plotTbl)
}
\description{
This sorts within facet.
}
