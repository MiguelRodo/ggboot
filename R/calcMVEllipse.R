#' mv med ellipse calc func
#'
#' x
mvEllCalcFunc = function( currTbl, locTbl ){
  # split into groups
  bootList = split( currTbl %>% select( V1, V2 ), currTbl$group )
  # locVec
  locList = llply( split( locTbl, locTbl$group ), function(x) c( x$V1, x$V2 ) )
  # apply ellipse calc func to each group
  ldply( seq_along( bootList ), function(i) calcEllipse( bootList[[i]], type = "mvMed", group = names( bootList )[i], p = 0.95, medVec =  locList[[ i ]] ) ) %>% as_tibble()
}


calcMVEllipse = function( bootTbl, locTbl ){

}
