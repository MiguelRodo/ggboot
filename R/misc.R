#' Subtract second half of a vector from the first.
#'
#' x
sub_second_half = function( numVec ){ n2 = length( numVec )/2; numVec[(n2+1):(2*n2)]-numVec[1:n2] }

#' Sort a table in some way.
#'
#' x
sortTblFunc = function( dataTbl, level ){
  dataRowList = split( dataTbl, 1:nrow( dataTbl ) )
  ldply( dataRowList, function( splitTbl, currLevel = level ){
    if( splitTbl$x == currLevel ) return( splitTbl )
    if( splitTbl$x != currLevel ){
      splitTbl$y = splitTbl$x
      splitTbl$x = currLevel
      splitTbl
    }
  }
  )  %>%
    select( -.id)
}

#' Split by vaccine within a specified splitting vector.
#'
#' x
fullSplitFunc = function( currTbl, splitVec1){
  llply( llply( split( currTbl, splitVec1 ), function(x) split( x, x$vaccine ) ) )
}

#' Calculate the 2.5% quantile.
#'
#' x
llbFunc = function(x){
  quantile( x, p = 0.025, na.rm = TRUE )
}

#' Calculate the 97.5% quantile.
#'
#' x
uubFunc = function(x){
  quantile( x, p = 0.975, na.rm = TRUE )
}

#' correct using bh procedure
#'
#' x
#' @export
correctBH = function( pVec, fdr, nSide ){

  sortPVec = sort( pVec )
  ind = 0; i = 0; m = length(sortPVec); k = 0; q = fdr/nSide

  while( i <= m - 1 ){
    i = i + 1
    if( sortPVec[i] <= (i/m * q) ) k = i
  }

  if( k > 0 ){
    if( is.null( names( sortPVec ) ) ) return( which( pVec %in% sortPVec[1:k] ) )
    return( names(sortPVec)[1:k] )
  }

  NULL
}

#' Calculate unique combinations
#'
#' x
calcUniqueCombn  = function( charVec, diff = TRUE ){
  allPwCombnVec = combn( unique( charVec ), 2 )
  uniquePwCombnVec = NULL

  for( i in 1:ncol( allPwCombnVec ) ){

    if( and( diff, allPwCombnVec[1,i] == allPwCombnVec[2,i]) ) next

    currCombn = str_c( allPwCombnVec[,i], collapse = "_" )
    currCond = !or( currCombn %in% uniquePwCombnVec, rev( currCombn ) %in% uniquePwCombnVec )
    if( currCond ) uniquePwCombnVec = append( uniquePwCombnVec, currCombn )
  }


  uniquePwCombnVec
}

#' Calculate maximum Bayes factor.
#'
#' upper bound bayes factor
#' @export
calcBF = function(p) -1/(exp(1) * p * log(p))

calcMinMax = function(x) c( "min" = min(x), "max" = max(x) )

#' Calculate min and max
#'
#' Wrapper function around min and max that returns as a vector the minimum and maximum of a vector.
#'
#' @param x A numeric vector.
#' @return A named numeric vector of the minimum and maximum value.
#' @examples
#' calcMinMax(rnorm(20))
#' calcMinMax(1:10)
#' @export

calcMinMax = function(x) c( "min" = min(x), "max" = max(x) )

#' Calculate number of unique vector elements.
#'
#' Convenience function around \code{length()} and \code{unique()}
#' that calculates the number of unique elements in a vector.
#'
#' @param x vector.
#' @export
calcLU = function(x) length( unique( x ) )

