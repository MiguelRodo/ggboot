context( "test that sortPW functions work")

# Test preparation --------------------------------------------------------

# function inputs
data = data.frame( facet = c( rep( 0, 6 ), rep( 1, 8 ) ),
                   group = c( 1, 1, 2, 2, 3, 3, 1, 1, 3, 3, 2, 2, 4, 4 ),
                   resp = c( 0.1, 0.3, 5, 2, -2, 1, 5, 2, 3, 4, 5, 6, 7, 3 ) )

plotTbl = data.frame( .id = c( rep( 0, 3 ), rep( 1, 6 ) ),
                      x = c( '1', '1', '2', '1', '1', '1', '2', '2', '3' ),
                      y = c( '2', '3', '3', '2', '3', '4', '3', '4', '4' ),
                      origStat = c( -3.2, 0.7, 4,
                                    -2, 0, -1.5, 2, 0.5, -1.5 ) )

groupLabVec = c( "1" = "A", "2" = "B", "3" = "C", "4" = "D" )
calcStat = pryr::partial( mean, trim = 0.2 )
# expected function outputs
## findSortedTbl
outTbl = data.frame( .id = c( rep( "0",3 ), rep( "1", 6 ) ),
                     x = c( "0.1", "0.1", "0.2",
                            "1.1", "1.1", "1.1", "1.2", "1.2",
                            "1.3" ),
                     y = c( "0.2", "0.3", "0.3", "1.2",
                            "1.3", "1.4", "1.3", "1.4", "1.4"),
                     origStat = c( 3.2, 4, 0.7, 0.5, 2, 2, 1.5, 1.5, 0 ),
                     stringsAsFactors = FALSE )
## sortGroupLabVec
outVec = c( "0.3" = "C", "0.2" = "A", "0.1" = "B", "1.4" = "A", "1.3" = "C", "1.2" = "D", "1.1" = "B")


# Tests -------------------------------------------------------------------

test_that( 'findSortedTbl function works', {
  expect_identical( findSortedTbl( data, plotTbl, calcStat ), outTbl )
} )

test_that( 'sortGroupLabVec function works', {
  expect_identical( sortGroupLabVec( data, groupLabVec, calcStat ), outVec )
} )

