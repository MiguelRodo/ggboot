#' Assign selected column indices to
#'
#' \code{assignSelAxisIndVec} assigns the selected columns in
#' \code{.data} from \code{.selAxisLab} to a specified environment.
#'
#' @param selAxisLab integer vector. Column indices of axes
#' to plot on biplot.
#' @param data numeric dataframe. Dataframe from which to take
#' the labels, if required.
#' @param env environment. Environment to which to save the results.
#'
#' @return Nothing. Assigns values to names in \code{env} directly.
assignSelAxisIndVec = function( selAxisLab, data, env ){
    # If no axes were pre-specified to be selected,
    # then return the integer vector from
    # to the number of columns in .data.
    if( is.null( selAxisLab ) ){
      assign( "selAxisIndVec", 1:ncol( data ), env )
    } else{
      assign( "selAxisIndVec", which( tbl_vars( data ) %in% selAxisLab ), env )
    }
}

#' Get selected column indices.
#'
#' \code{getSelAxisInd} returns the indices of the columns
#' in \code{data} whose names are in \code{selAxisLab}.
#' @param selAxisLab character vector. Names of columns
#' whose axes must be shown on biplot. If NULL, then
#' all axes must be plotted, and an integer vector
#' from 1 to \code{ncol(data)} is returned.
#' @param data numeric dataframe. Dataframe from which to draw
#' the labels.
#' @return Integer vector of columns in \code{data} to plot.
getSelAxisInd = function( selAxisLab, data ){

  if( is.null( selAxisLab ) ) return( 1:ncol( data ) )

  which( tbl_vars( data ) %in% selAxisLab )
}

#' Assign values to graphical parameters.
#'
#' \code{assignGraphPar} assigns values to the names
#' used for variables that determine what is plotted.
#'
#' @param displot,checkPlot logical. If \code{TRUE}, then
#' the values for certain graphical parameters given by default
#' or in the function call are overridden. See code of
#' \code{assignGraphPar} for details.
#' @param env environment. Environment to assign values to names in.
#'
#' @return Nothing. Assigns values to names in \code{env} directly.
assignGraphPar = function( dispPlot, checkPlot, env ){

  if( dispPlot ){
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

#' Get the measure of location function.
#'
#' Based on \code{locType}, returns the measure of location desired.
#'
#' @param locType character. Selects the location type to choose. Options
#' are: mean (column-wise median), gmed (geometric median), wei (fast approximate geometric median), oja and cwmed (column-wise median). Default is mean.
getLocFunc = function( locType = "mean", .trim = trim ){
  if( locType == "mean" ) return( function(x) c( mean( x[[1]] ), mean( x[[2]] ) ) )
  if( locType == "cwmed" ) return( function(x) c( median( x[[1]] ), median( x[[2]] ) ) )
  if( locType == "oja" ) return( ojaMedian )
  if( locType == "gmed" ) return( Gmedian )
  if( locType == "wei" ) return( weiszFunc )
  stop( "Unrecognised locType selected." )
}

#' Return proportion of variation explained.
#'
#' Convenience function to return the proportion of variation explained
#' for the desired principal components, rounded to two decimal places.
#'
#' @param vec numeric vector. Contains component-wise proportion of
#' variation.
#' @param comp integer vector. Specifies the principal components
#' whose proportion of variation must be returned. Defaults to 1:2.
#'
#' @return Numeric vector.
calcRoundPropVar = function( vec, comp = 1:2 ){
  laply( comp, function(x) round( vec[ x ], 2 ) * 100 )
}

