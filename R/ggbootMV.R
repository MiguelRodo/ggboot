
#' mv med plot info func
#'
#' x

#' PCA biplot with bootstrap confidence areas.
#'
#' Plot a PCA biplot with bootstrap confidence ares.
#'

#' @inheritParams ggbootUV
#' @inheritParams getSelAxisInd
#' @inheritParams getLocFunc
#' @inheritParams calcRoundPropVar
#' @param data dataframe. In wide format. Only numeric columns will be used.
#' @param group vector. Vector indicating group membership for each observation in \code{data}.
#' Will be coerced to a character vector.
#' @param boot logical. If TRUE, then bootstrap is re-performed. If FALSE, then
#' bootstrap values are taken from object with name \code{name} in environment \code{bootList}.
#' @param name character. Name of object in \code{bootList} to add to or take to. Must not be
#' NULL if \code{save=TRUE}.
#' @param save logical. If \code{TRUE} and \code{boot=TRUE}, then bootList is
#' @param  env environment. Environment to save bootstrapped values to.
#' @param path character. Path of to save bootstrapped values to, or load bootstrapped values from.
#' @param quant logical. If \code{TRUE}, then univariate 95 percent percentile bootstrap confidence
#' intervals are plotted.
#' @param textAdjVal numeric. Value that controls the degree to which the axis labels are shifted from
#' the endpoints of the axes. Higher values move the axis further right and up.
#' @param axesLenRatio numeric. If not NULL, then the ratio of the length of the y-axis divided by the
#' x-axis is made equal to \code{axesLenRatio}, where length is defined as the difference between the maximum and the
#' minimum value plotted along that axis. This is done by stretching the shorter axis around its mean. Useful
#' for making the axes of equal length.
#' @param qualCutoff numberic. The minimum sum of the axis predictivity across the principal components selected (by \code{comp}) required for the axis of a variable to be plotted. Only works if \code{arrow=FALSE} (this will change in future).
#' @param arrow logical. If TRUE, then arrows instead of lines are plotted for the axes. The arrows point from
#' the origin to the largest observed value for its variable on the biplot axis.
#' @param arrowSize numeric. Size of the arrows for \code{ggplot2} to use.
#' @param fontScaleFactor numeric. Ratio by which to multiply the default font size.
#' @export

ggbootMV = function( data, group, B, seed = NULL, comp = 1:2,
  locType = "mean",  scale = FALSE,
  checkPlot = FALSE, dispPlot = FALSE,
  labelVec = NULL, legendTitle = "Group",
  colVec = c( 'darkorchid1', 'springgreen2', 'maroon1', 'dodgerblue', 'red', 'yellow3', 'cyan2', "orange2" ),
  addOrigPos = TRUE, pcaAxesLabSize = 2.5, origPosLabSize = 2,
  axes = TRUE, points = TRUE, pointAlpha = 1, pointSize = 1,
  ellipse = TRUE, ellAlpha = 1, ellSize = 1,
  quant = FALSE, quantAlpha = 1, quantSize = 1,
  density = FALSE, densAlpha = 1, densSize = 1,
  boot = TRUE, save = FALSE, name = NULL, env = NULL, path = NULL,
  textAdjVal = 0.2, axesLenRatio = NULL,
  qualCutoff = 0, selAxisLab = NULL, arrow = FALSE, arrowSize = 2,
  fontScaleFactor = 1, trim = 0.2 ){

  #
  force(trim)

  # data edit
  group = as.character( group )
  data %<>% select_if( is.numeric )
  comp = sort( comp )

  if( is.null( labelVec ) ) labelVec = setNames( unique( group ),
    unique( group ) )

  # get indices of columns selected by selAxixLab
  selAxisIndVec = getSelAxisInd( selAxisLab, data )

  # assign graphical parameters
  assignGraphPar( dispPlot, checkPlot, environment() )

  # location function
  calcLoc = getLocFunc( locType )

  # pca
  biplotList = calcBiplot( dataTibble = data, scale = scale, ellipseGroupVec = group,
    pcCompNum1 = comp[1], pcCompNum2 = comp[2], textAdjVal = textAdjVal )

  attach( biplotList )

  # proportion of variation
  propVarVec = calcRoundPropVar( compPropVarVec, comp )

  # ellipses
  mvObj = calcMV( data = data, group = group, B = B, seed = seed,
    comp = comp, scale = scale, calcLoc = calcLoc, boot = boot, save = save,
    name = name, env = env, path = path, quant = quant, trim = trim )

  attach( mvObj )

  xlimVec = calcMinMax( c( textXVec, pcaScoreAndInfoTbl$V1 ) )
  ylimVec = calcMinMax( c( textYVec, pcaScoreAndInfoTbl$V2 ) )

  if( !is.null( axesLenRatio ) ) {
    ratio = diff(range(ylimVec)) / diff(range(xlimVec))
    if( ratio < axesLenRatio ) ylimVec = (ylimVec - mean(ylimVec) ) * (axesLenRatio/ratio) + mean(ylimVec)
    if( ratio > axesLenRatio ) xlimVec = (xlimVec - mean(xlimVec) ) * (ratio/axesLenRatio) + mean(xlimVec)
  }


  ### PLOT

  # scaffolding
  p = ggplot( pcaScoreAndInfoTbl ) +
    theme_cowplot( font_size = 14 * fontScaleFactor ) +
    labs( x = str_c( 'PC', comp[1], " - ", propVarVec[1], "%" ),
      y = str_c( 'PC', comp[2], " - ", propVarVec[2], "%" ) )

  if( axes ){

    axisLocTbl = tibble( varName = tbl_vars( data ), x = textXVec, y = textYVec )

    if( qualCutoff > 0 ){

      qualTbl = calcAxisPred( data, centre, scale )

      axisSelTbl = qualTbl %>%
        filter( comp %in% comp ) %>%
        group_by( varName ) %>%
        summarise( compQual = sum( compQual ) )

      qualIndVec = Vectorize(ifelse)( axisSelTbl[["compQual"]] > qualCutoff, "0", "1" ) %>%
        rep( rep(2, length(axisSelTbl[["compQual"]])))

      qualLabVec = c( "0" = paste( ">", qualCutoff), "1" = paste( "<", qualCutoff) )

      p = p +
        geom_line(data = axesCoordTbl %>%
            filter( segmentPos != 'max',
              varName %in% tbl_vars( data )[selAxisIndVec]) %>%
            mutate( qualInd = qualIndVec ),
          aes( x = x, y = y, group = varName, linetype = qualInd ), col = 'gray10'  ) +
        geom_line( data = axesCoordTbl %>%
            filter( segmentPos != 'min',
              varName %in% tbl_vars( data )[selAxisIndVec]) %>%
            mutate( qualInd = qualIndVec ),
          aes( x = x, y = y, group = varName, linetype = qualInd ), col = 'gray10' ) +
        scale_linetype_discrete( name = "Axis Predictivity",
          labels = qualLabVec )

    } else{

      if( !arrow ){
        p = p +
          geom_line( data = axesCoordTbl %>% filter( segmentPos != 'max',
            varName %in% tbl_vars( data )[selAxisIndVec]),
            aes( x = x, y = y, group = varName ), col = 'gray10'  ) +
          geom_line( data = axesCoordTbl %>% filter( segmentPos != 'min',
            varName %in% tbl_vars( data )[selAxisIndVec] ),
            aes( x = x, y = y, group = varName ), col = 'gray10' )
      } else{
        arrowDataTbl = axesCoordTbl %>% filter( segmentPos != 'min',
          varName %in% tbl_vars( data )[selAxisIndVec] )
        botTbl = arrowDataTbl %>% filter( segmentPos == "median" ) %>% mutate( x = 0, y = 0)
        topTbl = arrowDataTbl %>% filter( segmentPos == "max" ) %>%
          rename( xend = x, yend = y )

        arrowPlotTbl = full_join( botTbl, topTbl, by= "varName" )

        p = p +
          geom_segment( data = arrowPlotTbl, col = 'black', alpha = 0.8,
            aes( x = x, y = y, xend = xend, yend = yend, group = varName ),
            arrow = arrow( length = unit( 0.03, "npc") ), size = arrowSize )

      }

    }

  }

  # add points
  if( points ){

    p = p +
      geom_point( aes( x = V1, y = V2, col = group ),
        size = pointSize, alpha = pointAlpha ) +
      scale_colour_manual( name = legendTitle,
        values = colVec,
        labels = labelVec )
  }

  # add density curves
  if( density ) {
    # base densities
    p = p + geom_density2d( data = bootTbl,
      aes( x = V1, y = V2, col = group ),
      alpha = densAlpha, size = densSize )
  }

  # add ellipses
  if( ellipse ){

    # base ellipses
    p = p +
      geom_path( data = ellipseTbl,
        aes( x = x, y = y, colour = group ),
        size = ellSize, alpha = ellAlpha )

  }

  # add quantile lines
  if( quant ) {
    p = p +
      geom_line( aes( x = varV1, y = V2, colour = lineGroup ),
        quantPlotTbl, linetype = 1,
        size = quantSize, alpha = quantAlpha ) +
      geom_line( aes( x = V1, y = varV2, colour = lineGroup ),
        qquantPlotTbl, linetype = 1,
        size = quantSize, alpha = quantAlpha )
  }

  # final editing

  if( addOrigPos ) {

    p = p +
      geom_text( data = origLocTbl, aes( x = V1, y = V2,
        label = labelVec[origLocTbl$group] ),
        size = origPosLabSize * fontScaleFactor )
  }

  p = p +
    coord_fixed( xlim = xlimVec, ylim = ylimVec )

  # add names of axes
  if( axes ){
    p = p +
      annotate( 'text', x = textXVec[selAxisIndVec], y = textYVec[selAxisIndVec],
        label = tbl_vars( data )[selAxisIndVec], size = pcaAxesLabSize * fontScaleFactor,
        col = 'black', alpha = 0.8 )
  }

  detach(biplotList)
  detach(mvObj)

  p
}
