#' Bootstrap location measures for multiple groups.
#'
#' @param data dataframe. Numeric dataframe in wide format.
#' @param group character vector. Vector indicating group membership
#' for each observation in \code{data}.
#' @param calcLoc function. Returns multivariate measure of location.
#' @inheritParams ggbootUV

calcMVBoot = function( data, group, B, calcLoc ){

  # table of estimated mv locs across groups
  ldply( split( data, group ), function( x ){
    calcGroupMVBoot( x, B, calcLoc )
  } ) %>%
    dplyr::rename( group = .id ) %>%
    as_tibble()
}

#' Bootstrap location measures for a single group.
#'
#' @inheritParams calcMVBoot
#' @inheritParams ggbootUV

calcGroupMVBoot = function( data, B, calcLoc ){

  ldply( 1:B, function(i) calcBootSampleGroupMVBoot( data, calcLoc ) )

}

#' Bootstrap location measure.
#'
#' @inheritParams calcMVBoot
calcBootSampleGroupMVBoot = function( data, calcLoc ){
  data %>%
    sample_frac( replace = TRUE ) %>%
    calcLoc()
}
