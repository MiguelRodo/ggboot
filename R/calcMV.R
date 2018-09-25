#' Calculate information required for plotting PCA biplot with confidence areas.
#'
#' @param data dataframe. Only the numeric columns will be used.
#' @param group vector. Vector indicating group membership for each observation in \code{data}.
#' Will be coerced to a character vector.
#' @inheritParams ggbootMV
#' @inheritParams ggbootUV

calcMV = function( data, group, B, seed, comp = 1:2,
  scale, calcLoc, boot = TRUE, save = FALSE, name = NULL,
  env = NULL, path = NULL, quant, trim ){

  # checks
  if( save & is.null( name ) ) stop( "If save=TRUE, then name argument cannot be null." )
  if( !boot & is.null( name ) ) stop( "If boot=FALSE, then name argument cannot be null." )
  if( !boot & is.null( env ) ) stop( "If boot=FALSE, then env argument cannot be null." )

  # data edit
  group = as.character( group )
  data %<>% select_if( is.numeric )
  comp = sort( comp )

  # pca
  pcaTbl = calcPCA( data, scale = scale )[[2]][,comp] %>% as_tibble()

  # obtain bootstrap values
  if( boot ){

    # boot
    if( !is.null( seed ) ) set.seed( seed )
    bootTbl = calcMVBoot( pcaTbl, group, B, calcLoc )

    # hard cache
    if( !is.null( name ) ){
      env$bootList[[ name ]] = bootTbl
      if( save ) save( bootList, envir = env, file = path )
    }
  } else{
    bootTbl = env$bootList[[ name ]]
  }

  # median/mean
  ## original data
  origLocTbl = ldply( split( data, group ), function( x ) calcLoc( x ) ) %>%
    dplyr::rename( group = .id ) %>%
    as_tibble()

  ## bootstrap data
  bootLocTbl = ldply( split( bootTbl, bootTbl$group ), function( x ) calcLoc( x[,2:3] ) ) %>%
    dplyr::rename( group = .id ) %>%
    as_tibble()

  # quantiles
  quantTbl = bootTbl %>%
    group_by( group ) %>%
    summarise( lbV1 = llbFunc(V1), ubV1 = uubFunc(V1),
      lbV2 = llbFunc(V2), ubV2 = uubFunc(V2) )

  quantPlotTbl = tibble( varV1 = c( quantTbl$lbV1, quantTbl$ubV1 ),
    V2 = rep( origLocTbl$V2, 2 ),
    V1 = rep( origLocTbl$V1, 2 ),
    varV2 = c( quantTbl$lbV2, quantTbl$ubV2 ),
    lineGroup = as.character( rep( quantTbl$group, 2 ) ) )

  # ellipses
  ellipseTbl = calcGroupEllipse( bootTbl, p = 0.95 )

  # output
  list( "bootTbl" = bootTbl, "origLocTbl" = origLocTbl, "bootLocTbl" = bootLocTbl,
    "quantPlotTbl" = quantPlotTbl, "ellipseTbl" = ellipseTbl )

}
