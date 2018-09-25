context( "Testing calcBCa and related functions" )

# FLOW

# Objects used throughout
# - respVec
# - bootObj
# - percCI
# -- bootstrap percentile CI
# - bcaCI
# -- BCA ci

# Objects used throughout
respVec = c( 0.096888, 0.115579, 0.13671, 0.086497, 0.5248,
             1.676752, 0.46717, 0.360975, 0.118718 )
set.seed(3)
bootObj = boot::boot( respVec, function(x,w) mean(x[w]), R = 1e4 )
bcaCI = boot::boot.ci( bootObj, type = "bca" )$bca[4:5]
jackBCaCI = boot:::bca.ci( bootObj, type = "jack" )[4:5]
origStat = mean( respVec )

test_that( "calcBCa works when bootPkgCI is TRUE", {

  # neither bootObj nor bootVec given, and so need to use boot::boot
  expect_equal( calcBCa( origData = respVec, calcStat = mean,
                         bootPkgCI = TRUE, B = 1e4, seed = 3 ),
                bcaCI )
  # bootObj given, and so just apply boot.ci( ..., type = "bca" )
  expect_equal( calcBCa( bootObj = bootObj, bootPkgCI = TRUE, seed = 3,
                         B = 1e4 ),
                bcaCI )
  # bootVec only given, and so calculate L and origStat
  expect_equal( calcBCa( bootStat = bootObj$t, origData = respVec,
                         calcStat = mean, bootPkgCI = TRUE, B = 1e4,
                         seed = 3 ),
                jackBCaCI )
  # bootVec and origStat given, and so calculate L
  expect_equal( calcBCa( bootStat = bootObj$t, origData = respVec,
                         calcStat = mean, origStat = origStat,
                         bootPkgCI = TRUE, B = 1e4, seed = 3 ),
                jackBCaCI )
  # input errors
  expect_error( calcBCa( bootPkgCI = TRUE ) ) # missing calculation function and original data
  expect_error( calcBCa( bootPkgCI = TRUE, calcStat = mean ) ) # missing original data
  expect_error( calcBCa( bootPkgCI = TRUE, origData = respVec ) ) # missing calculation function
} )

test_that( "calcBCa works when bootPkgCI is FALSE", {

  # bootStat and origStat not given
  expect_equal( calcBCa( origData = respVec,
                                 calcStat = mean,
                                 bootPkgCI = FALSE, B = 1e4, seed = 3, type = 'bca' ),
                jackBCaCI)
  # bootStat given but origStat not
  expect_equal( calcBCa( bootStat = bootObj$t, origData = respVec,
                                 calcStat = mean,
                                 bootPkgCI = FALSE, B = 1e4, seed = 3, type = 'bca' ),
                jackBCaCI )
  # origStat given but bootStat not
  expect_equal( calcBCa( origData = respVec,
                                 calcStat = mean, origStat = origStat,
                                 bootPkgCI = FALSE, B = 1e4, seed = 3, type = 'bca' ),
                jackBCaCI )
  # bootStat and origStat given
  expect_equal( calcBCa( bootStat = bootObj$t, origData = respVec,
           calcStat = mean, origStat = origStat,
           bootPkgCI = FALSE, B = 1e4, seed = 3, type = 'bca' ),
           jackBCaCI )
  # bcaA given
  expect_equal( calcBCa( bootStat = bootObj$t, origData = respVec,
                                 calcStat = mean, origStat = origStat,
                                 bcaA = 0.1102757,
                                 bootPkgCI = FALSE, B = 1e4, seed = 3, type = 'bca' ),
                jackBCaCI)
  # type == "bc"
  expect_equal( signif( calcBCa( bootStat = bootObj$t, origData = respVec,
                                 calcStat = mean, origStat = origStat,
                                 bcaA = 0.1102757,
                                 bootPkgCI = FALSE, B = 1e4, seed = 3, type = 'bc' ), 7 ),
                c( 0.1690968, 0.7993466 ) )
  # ci = FALSE
  expect_equal( calcBCa( bootStat = bootObj$t, origData = respVec,
                                 calcStat = mean, ci = FALSE, alpha = 0.025,
                                 bootPkgCI = FALSE, B = 1e4, seed = 3, type = 'bca' ),
                bcaCI[1])
  # input errors
  expect_error( calcBCa( bootStat = bootObj$t,
                         calcStat = mean, bootPkgCI = FALSE, B = 1e4,
                         seed = 3, type = 'bca' ) ) # origData not given
  expect_error( calcBCa( bootStat = bootObj$t, origData = respVec,
                         bootPkgCI = FALSE, B = 1e4,
                         seed = 3, type = 'bca' ) ) # calcStat not given
  expect_error( calcBCa( bootStat = bootObj$t,
                         bootPkgCI = FALSE, B = 1e4,
                         seed = 3, type = 'bca' ) ) # neither given
  expect_error( calcBCa( bootPkgCI = FALSE, B = 1e4,
                         seed = 3, type = 'bca' ) ) # none of bootStat, origStat and bcaA given
  expect_error( calcBCa( bootStat = bootObj$t, bcaA = 0.11,
                         bootPkgCI = FALSE, B = 1e4,
                         seed = 3, type = 'bca' ) ) # only origStat not given
  expect_error( calcBCa( bootStat = bootObj$t, origStat = origStat,
                         bootPkgCI = FALSE, B = 1e4,
                         seed = 3, type = 'bca' ) ) # only bcaA not given
  expect_error( calcBCa( origStat = origStat, bcaA = 0.11,
                         bootPkgCI = FALSE, B = 1e4,
                         seed = 3, type = 'bca' ) ) # only bootStat not given
} )

