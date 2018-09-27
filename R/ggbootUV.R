#' Univariate bootstrap plot
#'
#' Plots a sample statistic, such as the mean or median, and associated bootstrapped 95\% confidence intervals for specified subsets of observations. Facilitates hypothesis testing by allowing a) application of the Benjamini-Hochberg procedure to control the alse discovery rate and b) specification of a minimum effect size.
#'

#' @param data dataframe. Has one row per observation and the column names specified below.
#' @param resp unquoted expression. Response variable name.
#' @param xAxis unquoted expression. X-axis variable name.
#' @param col unquoted expression. Colour variable name.
#' @param facet unquoted expression. Facet variable name.
#' @param diff unquoted expression. Difference variable name. Must be a binary variable, preferrably ('0','1') as then it is clear that the difference is the amount by which the observations labelled '1' are greater than the observations labelled '0'.
#' @param calcStat function olr character. Bootstrap statistic function. Functions other than mean or median used at own risk.
#' @param B integer. Number of bootstrap samples per subgroup.
#' @param alpha numeric. 1-\code{alpha} is the coverage of the confidence intervals.
#' @param nSide 1 or 2. Number of sides of the hypothesis tests.
#' @param altSde 'high', 'low' or 'both'. Character specifying the alternative hypothesis relative to the null hypothesis. Note that you must still specify nSide, even if altSide is specified.
#' @param fdr numeric. False discovery rate.
#' @param minEff numeric. Minimum effect size. Default is 0.
#' @param hLineVec numeric vector. Heights of dashed horizontal lines to plot. Useful for indicating minimum effect size.
#' @param nullValue numeric. Null hypothesis value for statistic.
#' @param xLab,yLab characeter. X- and y-axis titles. Optional.
#' @param xAxisLabVec named character vector. Labels for x-axis variable. Optional.
#' @param rotXText logical. If TRUE, then the x-axis text is rotated 90 degrees clockwise.
#' @param remXAxisMarks logical. If TRUE, then the x-axis ticks and text are removed.
#' @param pointSize numeric. Size of plot points
#' @param errBarSize numeric. Size of error bar
#' @param errBarSizeRatio numeric. How many times larger the size is of the statistically significant error bars than the
#' non-statistically significant bars are.
#' @param colLabName character. Name for colour legend. Optional.
#' @param colourVec character vector. Colours for colour scale. Optional. Label with factor levels to match levels to colours manually.
#' @param colLabVec named character vector. Labels for colour variable. Optional.
#' @param facetlabeVec named character vector. Labels for facet fariable. Optional.
#' @param facetScale 'free', 'free_x' or 'free_y'. Facet scale.
#' @param nCol numeric. Number of columns to use.
#' @param plotTblName character. If not NULL, then the dataframe used to plot is saved to global environment with name \code{plotTblName}.
#' @param eqErrBarWidth logical. If \code{TRUE}, then the error bars are made the same width in each facet. Otherwise the error bars automatically
#' span the entire facet, however many x-axis entries there are.
#' @param errBarAlpha numeric. Error bar alpha
#' @param errBarAlphaRatio numeric. How many times greater the alpha is of the statistically significant error bars than the
#' non-statistically significant bars are.
#' @param sigIndType character vector. Must contain at least one of \code{"lineType"}, \code{"alpha"} and \code{"size"}. The
#' statistically significant and non-statistically significant error bars will then differ by this aesthetic.
#' @param fontScaleFactor numeric. Factor by which to scale the text size from the default.
#' @param lineScaleFactor numeric. Factor by which to scale the line size from the default.
#' @param hLineFactor numeric. Factor by which to scale the horizontal line size from the default.
#' @param pMethod "perc" or "percT". Method to use for calculating p-values. Both uses the bootstrap percentile method, but "perc" does this on the mean or median, whilst "percT" does this on the t-statistic based on the mean or median.
#' @param ciMethod "perc" or "bca". method to use to calculate confidence intervals. Both use bootstrap samples, but "perc" uses the percentile method and "bca" the bias-corrected and adjusted method.
#' @return a ggplot2 object
#' @export

ggbootUV = function( data, resp, xAxis, col = NULL, facet = NULL, nullBFactor = 10,
  diff = NULL, calcStat = "mean", B = 10, seB = 200,
  pMethod = "percT", fdr = 0.01, altSide, nSide = 1,
  ciMethod = "bca",  alpha = 0.05, minEff = 0, hLineVec = NULL, nullValue = 0,
  xLab = NULL, yLab = 'Response', xAxisLabVec = NULL, rotXText = FALSE,
  remXAxisMarks = FALSE, pointSize = 2, errBarSize = 1, errBarSizeRatio = 2,
  errBarAlpha = 0.7, errBarAlphaRatio = 2, eqErrBarWidth = TRUE, errBarLineType = "dotted",
  colLabName = NULL, colourVec = c( 'darkorchid1', 'springgreen2', 'maroon1', 'dodgerblue', 'red', 'yellow3', 'cyan2', "orange2" ), colLabVec = NULL,
  facetLabVec = NULL, facetScale = 'free', nCol = 2,
  plotTblName = NULL,
  fontScaleFactor = 1, lineScaleFactor = 1, hLineFactor = 1,
    bootT = TRUE, trim = 0.2,  bootSECorr = FALSE ){

  ### PRELIMINARIES ###
  on.exit( if( exists( "cl1" ) ) parallel::stopCluster( cl1 ) )

  # variables

  ## variable names
  xAxisVar = substitute( xAxis )
  colVar = substitute( col )
  facet = substitute( facet )
  diff = substitute( diff )
  resp = substitute( resp )

  ## vectors of grouping variables
  xAxisVec = data[[ deparse( xAxisVar ) ]]
  colVec = data[[ deparse( colVar ) ]]
  facetVec = data[[ deparse( facet ) ]]
  diffVec = data[[ deparse( diff ) ]]
  respVec = data[[ deparse( resp ) ]]

  ## grouping variables name vector
  vNameVec = c( "V1" )
  if( !is.null( colVar ) ) vNameVec = c( vNameVec, "V2" )
  if( !is.null( facet ) ) vNameVec = c( vNameVec, "V3" )

  ## vector to split into groups based on
  splitVec = str_c( xAxisVec, colVec, facetVec, diffVec, sep = "." )
  noDiffSplitVec = str_c( xAxisVec, colVec, facetVec, sep = "." )

  # x axis label
  if( is.null( xLab ) ) xLab = deparse( xAxisVar )

  sdFunc = sd

  if( identical( calcStat, "median" ) ){
    calcStat = function(x) matrixStats::colMedians( as.matrix( x, ncol = 1 ) )
    calcSE = function(x) { boot::boot( x, R = seB,
      function(x,w) matrix( x[w], ncol = 1 ) %>% matrixStats::colMedians() )$t %>% sdFunc() }
  } else if( identical( calcStat, "mean" ) ){
    calcStat = function( x, .trim = trim ) mean( x, .trim )
    calcSE = function(x) { boot::boot( x, R = seB,
      function(x,w) mean( x[w], trim ) )$t %>% sdFunc() }
  } else{
    stop( "calcStat must be either 'mean' or 'median'")
  }

  if( pMethod == "perc" ) {
    calcStatVec = calcStat
    calcBootStatVec = function( x, w ) calcStat( x[w] )
  }
  if( pMethod == "percT" ){
    calcStatVec = function( x ) c( calcStat( x ), calcSE( x ) )
    calcBootStatVec = function( x, w ) c( calcStat( x[w] ), calcSE( x[w] ) )
  } # only calculate calcStat to save time when not using t-stat for hyp testing





  ### BOOTSTRAP STATISTICS ###
  cl1 = parallel::makeCluster(parallel::detectCores()-1)
  doParallel::registerDoParallel(cl1)
  if( is.null( diff ) ){ # if no difference taken

    ## table of statistics on original sample
    origStatTbl = plyr::ldply( split( respVec, splitVec ),
                               calcStatVec, .parallel = TRUE ) %>%
      rename( split = .id, origStat = V1 )

    origStatVec = setNames( origStatTbl$origStat, origStatTbl$split )

    bcaATbl = plyr::ldply( split( respVec, splitVec ), function(x) calcBCaA( x, calcStat ) ) %>%
      rename( split = .id, bcaA = V1 )

    bcaAVec = setNames( bcaATbl$bcaA, bcaATbl$split )
    if( pMethod == "percT" ){
      origStatTbl %<>%
        rename( origSE = V2 ) %>%
        mutate( origTStat = ( origStat - nullValue ) / origSE )
      origSEVec = setNames( origStatTbl$origSE, origStatTbl$split )
    }

    # table of bootstrapped sample estimates
    bootTbl = plyr::ldply( split( respVec, splitVec ),
      function(x) boot::boot( x, calcBootStatVec, B )$t, .parallel = TRUE ) %>%
      rename( split = .id, bootStat = `1` ) %>%
      mutate( origStat = origStatVec[ split ] )

    if( pMethod == "percT" ){
      bootTbl %<>%
        rename( bootSE = `2` ) %>%
        mutate( origSE = origSEVec[ split ],
                bootTStat = ( bootStat - origStat + nullValue ) / bootSE )
    }

  } else{ # difference taken
    bcaAVec = NULL
    if( ciMethod == "bca" ) ciMethod = "bc"
    ## table of statistics on original sample
    if( pMethod == "perc" ){
      origStatTbl = plyr::ldply( split( respVec, splitVec ), calcStat )
        mutate( diff = str_sub( .id, -1 ),
                split = str_sub( .id, end = -3 ) ) %>%
        arrange( split, diff ) %>%
        group_by( split ) %>%
        summarise( origStat = V1[2] - V1[1] )

        origStatVec = setNames( origStatTbl$origStat, origStatTbl$split )

      # table of bootstrapped sample estimates

       bootTbl = plyr::ldply( split( respVec, splitVec ), # a group for each unique value in splitVec
        function(x) boot::boot( x, calcBootStatVec, B )$t, .parallel = TRUE ) %>% # sample from each group B times, calculating calcBootStatVec
        rename( split = .id, bootStat = `1` ) %>%
        mutate( diff = str_sub( split, -1 ), split = str_sub( split, end = -3 ) )

      bootTbl = plyr::ldply( split( bootTbl, bootTbl$split ), # a group for each unique value in splitVec
        function(x) x %>%
          mutate( tempID = c( 1:(nrow(x)/2),1:(nrow(x)/2)  ) ) %>%
          spread( key = diff, value = bootStat ), .parallel = TRUE ) %>% # sample from each group B times, calculating calcBootStatVec
        mutate( bootStat = `1` - `0` ) %>%
        select( -c( .id, tempID, `0`, `1` ) ) %>%
        mutate( origStat = origStatVec[ split ] )

    } else if( pMethod == "percT" ){

      origDataTbl = tibble( split = splitVec, resp = respVec ) %>%
        mutate( origSplit = split, diff = str_sub( split, -1 ),
                split = str_sub( split, end = -3 ) )

      origStatTbl = ldply( split( origDataTbl, origDataTbl$split ),
        function(x){
          resp0Vec = x %>% filter( diff == 0 ) %>% extract2( "resp" )
          resp1Vec = x %>% filter( diff == 1 ) %>% extract2( "resp" )
          se0 = calcSE( resp0Vec )
          se1 = calcSE( resp1Vec )
          diffSE = sqrt( se0^2 + se1^2 )
          statDiff = calcStat( resp1Vec ) - calcStat( resp0Vec )
          tibble( origStat = statDiff, origSE = diffSE,
                  origTStat = ( statDiff - nullValue )/diffSE )
        } ) %>%
        rename( split = .id )

      # get bootstrap sample estimates of mean and estimates of sd of mean
      bootTbl = ldply( split( origDataTbl, origDataTbl$origSplit ),
        function(x){
          boot::boot( x$resp, R = B,
            function( y, w ) c( bootStat = calcStat( y[w] ),
                                bootSE = calcSE( y[w] ) ) )$t  %>%
            as_tibble() %>%
            rename( bootStat = V1, bootSE = V2 )
        }, .parallel = TRUE ) %>%
        rename( origSplit = .id ) %>%
        mutate( split = str_sub( origSplit, end = -3 ),
                diff = str_sub( origSplit, - 1 ) )

      # calculate bootStat and bootTStat

      bootTbl = plyr::ldply( split( bootTbl %>% select( -origSplit ), bootTbl$split ), # a group for each unique value in splitVec
        function(x){
          currSplit = x$split[1]
          currOrigStat = origStatTbl$origStat[which(origStatTbl$split==currSplit)]
          currOrigSE = origStatTbl$origSE[which(origStatTbl$split==currSplit)]
          x %<>% mutate(tempID = c( 1:(nrow(x)/2),1:(nrow(x)/2)  ) ) %>% select( -split )
          statTbl = x %>%
            select( - bootSE ) %>%
            spread( key = diff, value = bootStat ) %>%
            rename( stat0 = `0`, stat1 = `1` )
          sdTbl = x %>%
            select( - bootStat ) %>%
            spread( key = diff, value = bootSE ) %>%
            rename( se0 = `0`, se1 = `1` )
          fullTbl = merge( statTbl, sdTbl, by = "tempID" )

          fullTbl %>%
            mutate( bootStat = stat1 - stat0,
                    bootSE = sqrt( se1^2 + se0^2 ),
                    bootTStat = ( bootStat - currOrigStat + nullValue ) / bootSE,
                    origStat = currOrigStat,
                    origSE = currOrigSE) %>%
            as_tibble()
        } , .parallel = TRUE ) %>% # sample from each group B times, calculating calcBootStatVec
        rename( split = .id ) # %>%
        # select( split, bootStat, bootTStat, trueBootTStat, nullBootTStat )

    } else { stop( "pMethod must be one of perc or percT" ) }

  }

  if( pMethod == "percT" & bootSECorr )
    bootTbl = ldply( as.list( unique( bootTbl$split ) ), function(i){
      currOrigStat = origStatTbl %>%
        filter( split == i ) %>%
        extract2( "origStat" )
      currOrigSE = origStatTbl %>%
        filter( split == i ) %>%
        extract2( "origSE" )
      currBootTbl = bootTbl %>%
        filter( split == i )
      gamMod = gam::gam( formula = bootSE ~ gam::s(bootStat, df = 4),
                    data = currBootTbl )
      bootFit = gamMod$fitted.values
      origSEFit = gam::predict.Gam( gamMod, tibble( bootStat = currOrigStat ) )
      adjFactor = currOrigSE / origSEFit
      bootFit = bootFit * adjFactor # adjust by a multiplicative factor
      origMinBootSE = min( currBootTbl$bootSE ) # min  raw boot SE
      bootFit = Vectorize(ifelse)( bootFit <origMinBootSE, origMinBootSE, bootFit ) # ensure positive
      currBootTbl %>% mutate( bootSE = bootSE * adjFactor, adj = adjFactor )
    }, .parallel = TRUE )

  bootTbl %<>% as_tibble()

  ### BOOTSTRAP CONFIDENCE INTERVALS AND P-VALUES ###
  ciTbl = calcCITbl( bootData = bootTbl, alpha = alpha, method = ciMethod,
                     diff = !is.null( diff ), bcaAVec = bcaAVec, createCluster = FALSE )

  pTbl = calcPTbl( bootData = bootTbl, origData = origStatTbl, method = pMethod,
                   altSide = altSide, nullValue = nullValue )

  bootStatTbl = merge( ciTbl, pTbl, by = "split" )

  ### CORRECTIONS FOR MULTIPLE TESTING ###

  # identify names of split levels for which the p-value was statistically significant
  # after controlling fdr
  if( pMethod == "perc" ) pVec = bootStatTbl$percP
  if( pMethod == "percT" ) pVec = bootStatTbl$percTP

  fdrSigNameVec = correctBH( setNames(pVec, bootStatTbl$split ), fdr = fdr, nSide = 1 )

  plotTbl = bootStatTbl %>%
    full_join( origStatTbl, by = 'split' ) %>%
    mutate( fdrSig = ( split %in% fdrSigNameVec ) * 1  ) # add in p-value sig levels

  # check which meet min effect size
  if( altSide == "high" ) plotTbl %<>% mutate( mesSig = ( ( origStat >= abs( minEff ) ) * 1 ) )
  if( altSide == "low" ) plotTbl %<>% mutate(  mesSig = ( ( origStat <= -abs( minEff ) ) * 1 ) )
  if( altSide == "both" ) plotTbl %<>% mutate(  mesSig = pmax( ( origStat <= -abs( minEff ) ) * 1,
    ( origStat >= abs( minEff ) ) * 1 ) )

  # get which are both fdr and min eff size significant
  plotTbl %<>% mutate( sig = pmin( fdrSig, mesSig ) %>% as.character() )

  # assign plotTbl to plotTblName in global environment
  if( !is.null( plotTblName ) ) assign( plotTblName, as_tibble( plotTbl ), envir = globalenv() )




  ### PLOT ###

  plotTbl %<>% separate( split, into = vNameVec )

  # width
  if( eqErrBarWidth & !is.null( facet ) ){
    widthPerTbl = plotTbl %>%
      group_by( V3 ) %>%
      summarise( count = n() ) %>%
      mutate( maxCount = max( count ),
        widthRatio = count/maxCount * 0.95 )
    widthLabVec = setNames( widthPerTbl$widthRatio, widthPerTbl$V3 )

    plotTbl %<>% mutate( width = widthLabVec[ plotTbl$V3 ] )
  } else{
    plotTbl %<>% mutate( width = 1 )
  }


  # give values for lb and ub equal to origStat if lb and ub are NAN
  plotTbl %<>%
    mutate( lb = Vectorize(ifelse)( is.nan( lb ), origStat, lb ),
      ub = Vectorize(ifelse)( is.nan( ub ), origStat, ub ) )


  # base plot
  p = ggplot( plotTbl, aes( x = V1 ) ) + # base settings
    theme_cowplot( font_size = 14 * fontScaleFactor,
      line_size = 0.5 * lineScaleFactor ) +
    labs( x = xLab, y = yLab ) + # axes labels
    geom_hline( yintercept = nullValue, linetype = 2, size = 1 * hLineFactor ) # add a horizontal line at null value

  if( rotXText ) p = p + theme( axis.text.x = element_text( angle = 90 ) ) # rotate x-axis labels
  if( !is.null( xAxisLabVec ) ) p = p + scale_x_discrete( labels = xAxisLabVec ) # give x-axis better labels
  if( remXAxisMarks ) p = p + theme( axis.text.x = element_blank(),
    axis.ticks.x = element_blank() )    # remove x axis text
  # add in extra horizontal lines
  k = ifelse( is.null( hLineVec ), 0, length( hLineVec ) )
  while ( k > 0 ){
    p = p + geom_hline( yintercept = hLineVec[k], linetype = 2, alpha = 0.4, linetype = 'dotted' )
    k = k - 1
  }

  # colour variable
  if( is.null( colVar ) ){ # if there is no colour variables

    if( !fdr == 1 ){ p = p + geom_errorbar( aes( ymin = lb, ymax = ub, width = width,
        linetype = sig ), size = errBarSize, alpha = errBarAlpha )
    p = p +
      scale_linetype_manual( labels = c( "0" = "Not significant", "1" = "Significant" ),
        name = "",
        values = c( "0" = errBarLineType, '1' = 'solid' ) )
    }

    if( fdr == 1 ) p = p + geom_errorbar( aes( ymin = lb, ymax = ub, width = width ),
      size = errBarSize, alpha = errBarAlpha )

    p = p + geom_point( aes( y = origStat ), size = pointSize ) # add in points


  } else if( !is.null( colVar ) ){ # if there are colour variables

    if( !fdr == 1 ){  p = p + geom_errorbar( aes( ymin = lb, ymax = ub, width = width, col = V2,
      linetype = sig ), size = errBarSize, alpha = errBarAlpha )
      p = p +
        scale_linetype_manual( labels = c( "0" = "Not significant", "1" = "Significant" ),
          name = "",
          values = c( "0" = errBarLineType, '1' = 'solid' )  )
    }

    if( fdr == 1 ) p = p + geom_errorbar( aes( ymin = lb, ymax = ub, col = V2, width = width ),
      size = errBarSize, position = position_dodge( 1 ), linetype = errBarLineType,
      alpha = errBarAlpha )

    p = p +
      geom_point( aes( y = origStat, col = V2 ), size = pointSize,
        position = position_dodge( 1 ) ) + # add in points
      scale_colour_manual( values = colourVec,
        labels = colLabVec,
        name = colLabName) # modify points
  }

  # facet variable
  if( !is.null( facet ) ){

    if( is.null( facetLabVec ) ){
      p = p +
        facet_wrap( ~ V3, ncol = nCol, scales = facetScale )
    }
    else{
      p = p +
        facet_wrap( ~ V3, ncol = nCol, scales = facetScale,
          labeller = labeller( V3 = facetLabVec ) )
    }

  }

  p


}
