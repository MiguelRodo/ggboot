#' Pair-wise comparisons plot
#'
#' Plot the estimates or maximum Bayes factors (MBFs) pair-wise comparisons in terms of a summary statistic between all levels of a factor. BCa bootstrapping is used to obtain the maximum Bayes factor.
#'
#' @inheritParams ggbootUV
#' @param groupLabVec named character vector. Labels for grouping vector. Optional.
#' @param B integer. Number of bootstrap repetitions. If greater than 0, the plot displays the MBF rather than the estimate of the difference. If 0, then no bootstrapping is performed and the sample values for \code{calcStat} are displayed.
#' @param group unquoted expression. Group variable name.
#' @param axisLab character. X- and y-axis titles.
#' @param rotX logical. If TRUE, x-axis titles are rotated 90 degrees.
#' @param ncol integer. Number of facet variable columns.
#' @param text logical. If TRUE, the difference estimates or MBFs are written over each grid point.
#' @param textSize numeric. Size of text.
#' @param plot logical. If TRUE, then a plot is outputted.
#' @param facetScale 'fixed' or 'free'. Input for scale argument of \code{ggboot::facet_wrap}. Forced to \code{'free'} if \code{sort=TRUE}.
#' @param scale logical. Whether to sort x- and y-axes text by size of response. Default is \code{TRUE}.
#' @export

ggbootPW = function( data, resp, group, facet = NULL, B = 1e2, seB = 1e1, calcStat = "mean", trim = 0.2,
  fdr = NULL, minEff  = 0, groupLabVec = NULL, facetLabVec = NULL, axisLab = NULL,
  ncol = 2, facetOrderVec = NULL, rotX = FALSE, method = "percT", facetScale = 'fixed',
  text = TRUE, textSize = 3, plotTblName = NULL, plot = TRUE,
  fillLabVec = NULL,
  fillColVec = NULL,
  sort = TRUE ){

  # data
  tempTbl = data.frame( group = data[[ deparse( substitute( group ) ) ]],
                        resp = data[[ deparse( substitute( resp ) ) ]] )

  if( !is.null( substitute( facet ) ) ) tempTbl[[ "facet" ]] = data[[ deparse( substitute( facet ) ) ]]

  data = tempTbl %>%
    as_tibble() %>%
    mutate_if( is.factor, as.character )

  #
  if( is.null( fillLabVec ) ){
    if( !sort ){
      fillLabVec = c( "0" = "Not significant", "0.1" = "Left vaccine significantly larger",
                      "1.1" = "Bottom vaccine significantly larger" )
    } else{
      fillLabVec = c( "0" = " Not significant ", "1.1" = " Significant " )
    }
  }
  if( is.null( fillColVec ) ){
    if( !sort ){
      fillColVec =  c( "0" = "gray94", "0.1" = "orange", "1.1" = "royalblue1" )
    } else{
      fillColVec =  c( "0" = "gray94", "1.1" = "orange" )
    }
  }

  if( !sort ){
    fdrLegTitle = "Statistical\nSignificance"
  } else{
    fdrLegTitle = ""
  }



  ### BOOTSTRAP STATISTICS

  if( is.null( substitute( facet ) ) ) plotTbl = calcPW( data = data, calcStat = calcStat, trim = trim, B = B, seB = seB, method = method, fdr = fdr )
  if( !is.null( substitute( facet ) ) ) plotTbl = ldply( split( data, data$facet ),
    function(x) calcPW( data = x, calcStat = calcStat, trim = trim, B = B, seB = seB, method = method, fdr = fdr ) )


  if( is.null( groupLabVec ) ) groupLabVec = setNames( unique( data$group ), unique( data$group ))
  if( is.null( axisLab ) ) axisLab = "Group"


  if( sort ){

    if( identical( calcStat, "mean" ) & trim != 0 ){
      selTrim = trim
      calcStat = pryr::partial( mean, trim = selTrim )
    } else{
      calcStat = get( calcStat )
    }

    origGroupLabVec = groupLabVec
    groupLabVec = sortGroupLabVec( data, groupLabVec, calcStat = calcStat )
    facetScale = 'free'


    plotTbl = findSortedTbl( data, plotTbl, calcStat = calcStat )
  }

  ### CORRECTIONS FOR MULTIPLE TESTING

  if( !is.null( fdr ) ){
    if( !is.null( substitute( facet ) ) ) plotTbl %<>% mutate( split =  str_c( .id, split ) )

    sigNameVec = correctBH( setNames( plotTbl$p, plotTbl$split ), fdr, 2 )
    plotTbl %<>% mutate( fdrSig = ( split %in% sigNameVec ) * 1,
      mesSig =  pmax( ( origStat <= -abs( minEff ) ) * 1,
        ( origStat >= abs( minEff ) ) * 1 ),
      sig = pmin( fdrSig, mesSig ) %>% as.character(),
      xLarger = Vectorize(ifelse)( origStat >= 0, "1", "0" ),
      xLarger.Sig = Vectorize( ifelse )( sig == 0,
        "0",
        str_c( xLarger, ".", sig ) ) )

    ### MBF CALC
    if( text ) plotTbl %<>%
      mutate( mbf = Vectorize(calcBF)( p ),
        isNanMbf = is.nan( mbf ),
        mbf = Vectorize(ifelse)( isNanMbf, calcBF(1/B), mbf ) ) %>%
      select( -isNanMbf )
  }


  ### OUTPUT TBL
  if( !is.null( plotTblName ) ){

    if( sort ){
      invOrigGroupLabVec = setNames( names( origGroupLabVec ), origGroupLabVec )
      retPlotTbl = plotTbl %>%
        mutate( x = invOrigGroupLabVec[ groupLabVec[x]],
                y = invOrigGroupLabVec[ groupLabVec[y]] ) # convert x and y to original
    } else{
      retPlotTbl = plotTbl
    }

    assign( plotTblName, as_tibble( retPlotTbl ), envir = globalenv() )
  }
  if( !plot ) return() # if you just want the plotTbl






  ### PLOT





  p = ggplot( plotTbl, aes( x, y ) ) +
    labs( x = axisLab, y = axisLab ) +
    scale_x_discrete( labels = groupLabVec ) +
    scale_y_discrete( labels = groupLabVec )

  if( rotX ) p = p + theme( axis.text.x = element_text( angle = 90 ) )

  # just estimates
  if( is.null( fdr ) ){
    p = p +
      geom_tile( aes( fill = origStat ), col = "black" ) +
      scale_fill_gradient2( low = "royalblue1",
        high = "orange",
        name = "Decrease\nin Row" )

    if( text ) p = p +
        geom_text( aes( label = as.character( round( origStat, 2 ) ) ), size = textSize )

    # if to be faceted
    if( is.null( substitute( facet ) ) ) return( p )

    if( is.null( facetLabVec ) ) facetLabVec = setNames( data$facet, data$facet )
    p = p + facet_wrap( ~ .id, labeller = labeller( .id = facetLabVec ),
      ncol = ncol, scale = facetScale  )


    return( p )
  }

  p = p +
    geom_tile( aes( fill = xLarger.Sig ), col = "black" ) +
    scale_fill_manual( values = fillColVec,
      name = fdrLegTitle,
      labels = fillLabVec )

  if( text ) p = p +
    geom_text( aes( label = as.character( round( mbf, 0 ) ) ), size = textSize )

  # facet wrap
  if( is.null( facetLabVec ) ) facetLabVec = setNames( unique( data$facet ), unique( data$facet ) )
  if( !is.null( substitute( facet ) ) )  p = p +
    facet_wrap( ~ .id, labeller = labeller( .id = facetLabVec ),
                ncol = ncol, scale = facetScale  )

  p
}
