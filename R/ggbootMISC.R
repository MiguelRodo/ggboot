#' Set ggbootMV graphical parameters.
#'
#' Set the graphical parameters for ggbootMV.
#'
#' @param dispPlot,checkPlot logical. If \code{TRUE}, then
#' default values are over-ridden with appropriate values
#' for a display or checking plot, respectively. If both are \code{TRUE},
#' then dispPlot is used.
#' @param env environment. Environment to which to create the bindings.
assignGraphPar = function( dispPlot, checkPlot, env ){

  if( dispPlot){
    assign( "axes", TRUE, env )
    assign( "points", TRUE, env )
    assign( "pointAlpha", 1, env )
    assign( "ellipse", TRUE, env )
    assign( "ellAlpha", 1, env )
    assign( "ellSize", 2, env )
    assign( "quant", FALSE, env )
    assign( "density", TRUE, env )
    assign( "densAlpha", 0.3, env )
    assign( "densSize", 1, env )
    assign( "addOrigPos", FALSE, env)
    return( NULL )
  }

  if( checkPlot ){
    assign( "axes", FALSE, env )
    assign( "points", TRUE, env )
    assign( "pointAlpha", 1, env )
    assign( "ellipse", TRUE, env )
    assign( "ellAlpha", 0.8, env )
    assign( "ellSize", 1, env )
    assign( "quant", TRUE, env )
    assign( "quantAlpha", 0.8, env )
    assign( "quantSize", 1, env )
    assign( "density", TRUE, env )
    assign( "densAlpha", 0.2, env )
    assign( "densSize", 1, env )
    assign( "addOrigPos", FALSE, env)
  }
  NULL
}

#' Return the selected location function.
#'
#' @param locType character. Specifies measure of location to use. Options are
#' mean, cmwed (column-wise median), oja, weisz (geometric median) and
#' gmed (a fast version for the geometric median).
getLocFunc = function( locType ){
  if( locType == "mean" ) return( function(x) c( mean( x[[1]] ), mean( x[[2]] ) ) )
  if( locType == "cwmed" ) return( function(x) c( median( x[[1]] ), median( x[[2]] ) ) )
  if( locType == "oja" ) return( OjaNP::ojaMedian )
  if( locType == "gmed" ) return( Gmedian::Gmedian )
  if( locType == "weisz" ) return( function(x) Gmedian::Weiszfeld(x)$median )
  stop( "locType not recognised.")
}

#' Return the column indices with given variables names.
#'
#' @param selAxisLab character vector. Names of columns in \code{data} to print.
#' @param data dataframe.
getSelAxisInd = function( selAxisLab, data ){
  if( is.null( selAxisLab ) ) return( 1:ncol( data ) )
  which( tbl_vars( data ) %in% selAxisLab )
}
