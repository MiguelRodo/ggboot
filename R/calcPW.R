#' Compute pair-wise comparisons estimates and bootstrap statistics
#'
#' Calculates the mean or median sample estimates of all
#' pair-wise differences within a factor. Can be set
#' compute the maximum Bayes factors (MBFs) and indicate
#' which of the comparisons are statistically
#' significant after controlling the false discovery rate, if the number of bootstrap repetition (\code{B}) is set to greater than 0.
#'
#' @param data dataframe. Contains numeric variable named 'resp' for responses and character variable named 'group' containing factor levels.
#' @param B integer. Number of bootstrap samples per subgroup. If equal to zero, then the MBF is not calculated.
#' @inheritParams ggbootUV
#' @return A tibble of the comparison groups and estimates of differences in \code{calcStat}, as well as the confidence intervals and MBFs if bootstrapping was performed.
#' @export
calcPW = function( data, calcStat, trim = 0.2, B, seB, method = "percT", fdr = FALSE ){

  # FLOW
  #
  # Parallel I
  # - Purpose:
  # -- Closes cl1 if it exists when the function closes.
  #
  # Functions
  # - Purpose:
  # -- Creates functions to calculate sample statistic and sample statistic standard
  # -- deviation, both for the original sample and for use in boot::boot.
  # - Code:
  # -- calcStat
  # --- calculates mean or median
  # -- calcSE
  # --- Calculates standard deviation of calcStat
  # -- calcBootStatVec
  # --- If method is "perc", then only calculates calcStat
  # --- for each bootstrap sample to save time.
  # --- If method is "percT", then it calculates both
  # --- calcStat and calcSE for each bootstrap sample.
  #
  # Combinations
  # - Purpose:
  # -- Calculates all unique pair-wise combinations.
  #
  # Raw original statistics
  # - Purpose
  # -- Calculates calcStat and, if method == "percT",
  # -- calcSE, for each group. This is needed for calculating
  # -- the original statistics for
  # -- ( stat1 - stat0 ) (for method == "perc" ) and
  # -- ( stat1 - stat0 ) / sqrt( se1^2 + se2^2)
  #
  # Original statistics
  # - Purpose
  # -- Calculates original statistics, as described above,
  # -- for each pair-wise combination, as found in
  # -- the 'Combinations' section.
  # - Code:
  # -- Calculates only origStat if method == "perc",
  # -- and both origStat and origSE if method == "percT".
  #
  # Sample differences only
  # - Purpose:
  # -- Returns the original statistics as calculated above,
  # -- if no statistical significance testing is to be performed.
  # - Code:
  # -- This is set to happen if fdr is NULL.
  #
  # Parallel II
  # - Purpose
  # -- Once it's confirmed that bootstrapping is required to
  # -- calculate p-values, since not only sample differences
  # -- are being returned, a cluster is initiated.
  #
  # Raw bootstrap statistics
  # - Purpose
  # -- Similary to 'Raw original statistics',
  # -- calcStat and, if method == "percT", calcSE,
  # -- are calculated for each bootstrap sample
  # -- from each individual group.
  #
  # Statistics
  #
  on.exit( if( exists( "cl1" ) ) parallel::stopCluster( cl1 ) )

  # Functions
  if( identical( calcStat, "median" ) ){
    calcStat = function(x) matrixStats::colMedians( as.matrix( x, ncol = 1 ) )
    calcSE = function(x) { boot::boot( x, R = seB,
                                       function(x,w) matrix( x[w], ncol = 1 ) %>% matrixStats::colMedians() )$t %>% sd() }
  } else if( identical( calcStat, "mean" ) ){
    calcStat = function( x ) mean( x, trim )
    calcSE = function(x) { boot::boot( x, R = seB,
                                       function(x,w) mean( x[w], trim ) )$t %>% sd() }
  } else{
    stop( "calcStat must be either 'mean' or 'median'")
  }

  if( method == "percT" ) calcBootStatVec = function( x, w ) c( calcStat( x[w] ),
                                          calcSE( x[w] ) )
  if( method == "perc" ) calcBootStatVec = function( x, w ) calcStat( x[w] )

  # Combinations
  combnVec = calcUniqueCombn( data$group )


  # Raw original statistics
  if( method == "perc" ) rawOrigStatTbl = data %>%
    group_by( group ) %>%
    summarise(  rawOrigStat = calcStat( resp ) )

  if( method == "percT" ) rawOrigStatTbl = data %>%
    group_by( group ) %>%
    summarise( rawOrigStat = calcStat( resp ),
               rawOrigSE = calcSE( resp ) )

  # Original statistics
  if( method == "perc" ) origStatTbl = tibble( split = combnVec ) %>%
    mutate( split1 = split ) %>%
    separate( split1, into = c( "y", "x" ) ) %>%
    group_by( x, y, split ) %>%
    summarise( origStat = rawOrigStatTbl$rawOrigStat[ rawOrigStatTbl$group == x ] -
                 rawOrigStatTbl$rawOrigStat[ rawOrigStatTbl$group == y ] ) %>%
    ungroup()

  if( method == "percT" ) origStatTbl = tibble( split = combnVec ) %>%
    mutate( split1 = split ) %>%
    separate( split1, into = c( "y", "x" ) ) %>%
    group_by( x, y, split ) %>%
    summarise( origStat = rawOrigStatTbl$rawOrigStat[ rawOrigStatTbl$group == x ] -
                 rawOrigStatTbl$rawOrigStat[ rawOrigStatTbl$group == y ],
               origSE = ( rawOrigStatTbl$rawOrigSE[ rawOrigStatTbl$group == x ]^2 +
                            rawOrigStatTbl$rawOrigSE[ rawOrigStatTbl$group == y ]^2 )^0.5 ) %>%
    mutate( origTStat = origStat / origSE ) %>% ### should be ( origStat - nullValue ) / origSE
    ungroup()

  # Sample differences only
  if( is.null( fdr ) ) return( origStatTbl )

  # Parallel II
  cl1 = parallel::makeCluster(parallel::detectCores()-1)
  doParallel::registerDoParallel(cl1)

  # Raw bootstrap statistics
  rawBootStatTbl = ldply( split( data, data$group ),
                          function(x) boot::boot( x$resp, calcBootStatVec, B )$t,
                          .parallel = TRUE ) %>%
    rename( group = .id, rawBootStat = `1` )

  if( method == "percT" ) rawBootStatTbl %<>% rename( rawBootSE = `2` )

  # Statistics
  statTbl = ldply( split( origStatTbl, origStatTbl$split ), function( tbl ){

    xRawBootStatVec = rawBootStatTbl %>% filter( group == tbl$x ) %>% `[[`( "rawBootStat" )
    yRawBootStatVec = rawBootStatTbl %>% filter( group == tbl$y ) %>% `[[`( "rawBootStat" )
    bootStatVec = xRawBootStatVec - yRawBootStatVec
    origStat = origStatTbl %>% filter( x == tbl$x & y == tbl$y ) %>% `[[`( "origStat" )
    z0 = calcBCaZ0( bootStatVec, origStat )
    if( method == "perc" ){
      alphaHigh = sum( ( bootStatVec - origStat ) >= origStat  ) / length( bootStatVec )
      alphaLow = sum( ( bootStatVec - origStat ) <= origStat ) / length( bootStatVec )
    }

    if( method == "percT" ){
      xRawBootSEVec = rawBootStatTbl %>% filter( group == tbl$x ) %>% `[[`( "rawBootSE" )
      yRawBootSEVec = rawBootStatTbl %>% filter( group == tbl$y ) %>% `[[`( "rawBootSE" )
      bootStatSE = sqrt( xRawBootSEVec^2 + yRawBootSEVec^2 )
      origSE = origStatTbl %>% filter( x == tbl$x & y == tbl$y ) %>% `[[`( "origSE" )
      origTStat = origStatTbl %>% filter( x == tbl$x & y == tbl$y ) %>% `[[`( "origTStat" )
      bootTStatVec = ( bootStatVec - origStat ) / bootStatSE
      z0T = calcBCaZ0( bootStatVec / bootStatSE, origStat / origSE )
      alphaHigh = sum( bootTStatVec >= origTStat  ) / length( bootTStatVec )
      alphaLow = sum( bootTStatVec <= origTStat ) / length( bootTStatVec )
    }

    pVal = 2 * min( alphaHigh, alphaLow )

    splitStatTbl = tibble( x = tbl$x, y = tbl$y, split = tbl$split, p = pVal, origStat = origStat,
                           z0 = z0 )
    if( method == "percT" ) splitStatTbl %<>% mutate( z0T = z0T )
    if( "facet" %in% colnames(data) ) splitStatTbl %<>% mutate( facet = data$facet[1] )
    splitStatTbl
  }, .parallel = TRUE )

  statTbl %>%
    mutate( .id = facet ) %>%
    select( -facet )
}
