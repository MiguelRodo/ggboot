#' Find and correct row of a specific categorical variable combination.
#'
#' Used for the \code{ggbootPW} function, where it may be desired
#' that the variables are put in a specific order along the x and y-axis.
#' Finds the row in a dataframe corresponding to a specific 'x' and 'y' combination,
#' but where the elements that should be in 'x' may be in 'y', and vice versa. It
#' returns that row, with the elements swopped around if need be. If the elements are swopped,
#' the column labelled 'origStat' is multiplied by -1.
#'
#' @param xVar,yVar numeric/character. Elements to set as 'x' and 'y' variables, respectively.
#' @param plotTbl dataframe. Dataframe with columns x, y and origStat. May have other columns.
#' The combination \code{xVar} and \code{yVar} must only occur once in \code{plotTbl}, regardless of order.
#' @return Row of dataframe as a dataframe.
#' @examples findXYRow( 1, 2, data.frame(x = c( 3, 1, 1 ),
#' y = c( 2, 2, 1 ),
#' origStat = c( 0.5, 0.3, -0.2 ) ) )

findXYRow = function( yVar, xVar, plotTbl ){
  origRow = plotTbl %>%
    filter( x == xVar | y == xVar ) %>%
    filter( x == yVar | y == yVar )

  if( origRow$x != xVar ) origRow[["origStat"]] = -origRow[["origStat"]]

  origRow %>% mutate( x = xVar, y = yVar )
}

#' Set a categorical variable as x-variable, with all remaining y-variables.
#'
#' Returns the rows of plotTbl with the selected group as the x-variable, and
#' all the y-variables that are "smaller" than this variable, as defined by \code{sortVec}.
#'
#' @param i numeric. \code{sortVec[1:(length(sortVec)-i)]} are searched for as y-variables, and
#' \code{rev( sortVec )[i]} is set as the x-variable.
#' @inheritParams findXYRow
#' @param sortVec character vector. Order in which groups are to display along x- and y-axes. The
#' first group along the x-axis is the last element of \code{sortVec}.
#' @return Dataframe with the columns along x and y-axes changed appropriately, for the specific variables
#' as set by \code{i} and \code{sortVec}.
#' @examples findXTbl( 1, 1:2, data.frame(x = c( 3, 1, 1 ),
#' y = c( 2, 2, 1 ),
#' origStat = c( 0.5, 0.3, -0.2 ) ) )
findXTbl = function( i, sortVec, plotTbl ){
  xVar = rev( sortVec )[i]
  yVarList = sortVec[1:(length(sortVec)-i)] %>% as.list()
  ldply( yVarList, findXYRow, xVar = xVar, plotTbl = plotTbl )
}

#' Sort x- and y-columns by size of response for an individual facet.
#'
#' This assumed that all the data is from an individual facet.
#'
#' @param data dataframe. Has columns group, resp and facet. The column 'group' specifies individual group,
#' 'resp' response and 'facet' overall facet.
#' @param plotTbl dataframe. Has columns .id, x, y and origStat. May have other columns. The column '.id' specifies overall facet,
#' 'x' and 'y' the specific groups compared in a row, and 'origStat' how much larger the variable level in 'y' is than the variable
#' level in 'x' (at least for \code{ggbootPW}; for other functions it could be the other way around the code would run fine).
#' @return Dataframe with columns along x- and y-axes changed appropriately.
#' @export
findSortedIndTbl = function( data, plotTbl, calcStat ){


  # table of groups sorted by average response size
  sortTbl = data %>%
    filter( facet == plotTbl$.id[1] ) %>%
    group_by( group ) %>%
    summarise( mean = calcStat( resp ) ) %>%
    arrange( mean )

  # vector of group names sorted by average response size
  sortVec = sortTbl$group
  sortLabVec = setNames( length( sortVec ):1, sortTbl$group )

  outTbl = ldply( seq_along( sortVec[-1]), findXTbl, sortVec = sortVec, plotTbl = plotTbl ) %>%
    mutate( x = as.character( sortLabVec[x] ),
            y = as.character( sortLabVec[y] ) )

  outTbl %>%
    mutate( x = str_c( .id, ".", x ), y = str_c( .id, ".", y ) ) %>%
    arrange( .id, x, y )

}

#' Sort x- and y-columns by size of response across facets.
#'
#' This sorts within facet.
#'
#' @param data dataframe. Has columns group, resp and facet. The column 'group' specifies individual group,
#' 'resp' response and 'facet' overall facet.
#' @param plotTbl dataframe. Has columns .id, x, y and origStat. May have other columns. The column '.id' specifies overall facet,
#' 'x' and 'y' the specific groups compared in a row, and 'origStat' how much larger the variable level in 'y' is than the variable
#' level in 'x' (at least for \code{ggbootPW}; for other functions it could be the other way around the code would run fine).
#' @return Dataframe with columns along x- and y-axes changed appropriately.
#' data = data.frame( facet = c( rep( 0, 6 ), rep( 1, 8 ) ),
#' group = c( 1, 1, 2, 2, 3, 3, 1, 1, 3, 3, 2, 2, 4, 4 ),
#' resp = c( 0.1, 0.3, 5, 2, -2, 1, 5, 2, 3, 4, 5, 6, 7, 3 ) )
#' plotTbl = data.frame( .id = c( rep( 0, 3 ), rep( 1, 6 ) ),
#' x = c( '1', '1', '2', '1', '1', '1', '2', '2', '3' ),
#' y = c( '2', '3', '3', '2', '3', '4', '3', '4', '4' ),
#' origStat = c( 0, 5, 3, 2, 5, 4, 2, 10, 10 ) )
#' findSortedTbl( data, plotTbl)
#' @export
findSortedTbl = function( data, plotTbl, calcStat ){

  plotTbl %<>% mutate( .id = as.character( .id ),
                      x = as.character( x ),
                      y = as.character( y ) )
  data %<>% mutate( facet = as.character( facet ),
                    group = as.character( group ) )
  ldply( split( plotTbl, plotTbl$.id ), findSortedIndTbl, data = data, calcStat = calcStat )
}

#' Change group labels according to sorted response size, within a facet.
#' @param data dataframe. Dataframe with columns 'group' and 'resp', where the elements in 'resp' are the responses of
#' individual subjects, and elements in 'group' specify the subject group.
#' @param groupLabVec named character vector. Specifies labels for each of the unique elements in the 'group' column of
#' \code{data}.
#' @return A named vector, where the groups are sorted according to response size.
#' @examples
#' data = data.frame( group = c( 1, 1, 1, 2, 2, 2, 3, 3, 3 ),
#' resp = c( 5, 4, 6, 10, 12, 7, 6, 1, 4 ) )
#' groupLabVec = c( "1" = "A", "2" = "B", "3" = "C" )
#' sortIndGroupLabVec( data, groupLabVec )
sortIndGroupLabVec = function( data, groupLabVec, calcStat ){
  sortTbl = data %>%
    group_by( group ) %>%
    summarise( mean = calcStat( resp ) ) %>%
    arrange( mean )
  setNames( groupLabVec[ sortTbl$group ], as.character( nrow( sortTbl ):1  ) )

}

#' Change group labels according to sorted response size.
#'
#' @inheritParams sortIndGroupLabVec
#' @param data dataframe. Dataframe with columns 'facet', 'group',  'resp'. The elements in 'resp' are the responses of
#' individual subjects, elements in 'group' specify the subject group, and elements in 'facet' specify the overall
#' grouping variable.
#' @return A named vector, where the groups are sorted according to response size.
#' data = data.frame( group = c( 1, 1, 1, 2, 2, 2, 3, 3, 3, 1, 2, 3 ),
#' resp = c( 5, 4, 6, 10, 12, 7, 6, 1, 4, 4, 8, 7 ),
#' facet = c( rep( "0", 9 ), rep( "1", 3 ) ) )
#' groupLabVec = c( "1" = "A", "2" = "B", "3" = "C" )
#' sortGroupLabVec( data, groupLabVec )
#' @export
sortGroupLabVec = function( data, groupLabVec, calcStat ){
  llply( split( data, data$facet ), sortIndGroupLabVec, groupLabVec = groupLabVec, calcStat = calcStat ) %>% unlist()
}

