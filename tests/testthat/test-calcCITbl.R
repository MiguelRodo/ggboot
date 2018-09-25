context( "Testing calcCITbl" )

# Data --------------------------------------------------------------------

respVec = c( 0.060426, 0.066152, 0.07093, 0.056687, 0.18716, 0.790952, 0.20803, 0.245396, 0.087918,
             0.02499226, 0.2234444308, 0.0148025445, 0.2425358694, 0.025742255, 0.011875387, 0.0148772707,
0.0086363158, 0.014349707, 0.0087206469, 0.055159961, 0.0366530097, 0.000260970000000001, 0.0111716045, 0.011851586,
0.0109080046, 0.002077461, 0.011693718, 0.0182342151, 0.0031248769, 0.0067057876, 0.0172261736,
0.0491776258, 0.026822441, 0.0062511869, 0.0135163775, 0.003760361, 0.0196274421, 0.004280863, 0.0143105389,
0.0150681214, 0.0063319547, 0.0087206469, 0.000260970000000001, 0.0111716045, 0.002077461, 0.011693718, 0.0148772707,
0.055159961, 0.0366530097, 0.011851586, 0.0109080046, 0.0182342151, 0.0172261736, 0.026822441, 0.0135163775 )
splitVec = c( rep( "1", 9), rep( "2", 46 ) )
calcStat = function(x) mean( x, trim = 0.2 )
calcStatVec = calcStat
calcBootStatVec = function(x,w) calcStatVec(x[w])

# table of statistics on original sample
origStatTbl = plyr::ldply( split( respVec, splitVec ),
                           calcStatVec ) %>%
  rename( split = .id, origStat = V1 )

origStatVec = setNames( origStatTbl$origStat, origStatTbl$split )

# acceleration factor
bcaATbl = plyr::ldply( split( respVec, splitVec ), function(x) calcBCaA( x, calcStat ) ) %>%
  rename( split = .id, bcaA = V1 )

bcaAVec = setNames( bcaATbl$bcaA, bcaATbl$split )

# bootstrap sample statistics
set.seed(1)
bootTbl = plyr::ldply( split( respVec, splitVec ),
                       function(x) boot::boot( x, calcBootStatVec, 1e2 )$t ) %>%
  rename( split = .id, bootStat = `1` ) %>%
  mutate( origStat = origStatVec[ split ] ) %>%
  as_tibble()


# Test: bca, no diff ------------------------------------------------------

# Data
currBootTbl = bootTbl %>% filter( split == "1" )
ci1 = calcBCa( bootStat = currBootTbl$bootStat,
         origStat = currBootTbl$origStat[1],
         bcaA = bcaAVec[1] )
z01 = calcBCaZ0( currBootTbl$bootStat, currBootTbl$origStat[1])
a1 = calcBCaA( respVec[splitVec==1], calcStat )
currBootTbl = bootTbl %>% filter( split == "2" )
ci2 = calcBCa( bootStat = currBootTbl$bootStat,
               origStat = currBootTbl$origStat[1],
               bcaA = bcaAVec[2] )
z02 = calcBCaZ0( currBootTbl$bootStat, currBootTbl$origStat[1])
a2 = calcBCaA( respVec[splitVec==2], calcStat )
expTbl = tibble( split = c("1","2"), lb = c( ci1[1], ci2[1] ),
                 ub = c( ci1[2], ci2[2] ),
                 z0CI = c( z01, z02 ), aCI = c( a1, a2 ) )

# Test
test_that( "calcCITbl works for method == bca & no diff", {
  # bca, no diff
  expect_identical( calcCITbl( bootData = bootTbl, alpha = 0.05, method = "bca",
             diff = FALSE, bcaAVec = bcaAVec ), expTbl )
})


# Test: bca, no diff & bc, no diff & bc, diff -----------------------------

# Data
currBootTbl = bootTbl %>% filter( split == "1" )
ci1 = calcBCa( bootStat = currBootTbl$bootStat,
               origStat = currBootTbl$origStat[1],
               bcaA = 0 )
z01 = calcBCaZ0( currBootTbl$bootStat, currBootTbl$origStat[1])
a1 = calcBCaA( respVec[splitVec==1], calcStat )
currBootTbl = bootTbl %>% filter( split == "2" )
ci2 = calcBCa( bootStat = currBootTbl$bootStat,
               origStat = currBootTbl$origStat[1],
               bcaA = 0 )
z02 = calcBCaZ0( currBootTbl$bootStat, currBootTbl$origStat[1])
a2 = calcBCaA( respVec[splitVec==2], calcStat )
expTbl = tibble( split = c("1","2"), lb = c( ci1[1], ci2[1] ),
                 ub = c( ci1[2], ci2[2] ),
                 z0CI = c( z01, z02 ), aCI = c( a1, a2 ) )
expNATbl = expTbl %>% mutate( aCI = NA )

# Tests
test_that( "calcCITbl works for method == bca & no diff", {
  # bca, diff
  expect_identical( calcCITbl( bootData = bootTbl, alpha = 0.05, method = "bca",
                               diff = TRUE, bcaAVec = bcaAVec ),
                    expNATbl )
  # bc, no diff
  expect_identical( calcCITbl( bootData = bootTbl, alpha = 0.05, method = "bc",
                               diff = FALSE, bcaAVec = bcaAVec ),
                    expTbl )
  # bc, ndiff
  expect_identical( calcCITbl( bootData = bootTbl, alpha = 0.05, method = "bc",
                               diff = TRUE, bcaAVec = bcaAVec ),
                    expNATbl )
})

# Test: perc, diff & perc, no diff ----------------------------------------------

# Data
currBootTbl = bootTbl %>% filter( split == "1" )
lb1 = matrixStats::colQuantiles( matrix( currBootTbl$bootStat,
                                   ncol = 1),
                           probs = 0.025 ) %>%
  setNames( NULL )
ub1 = matrixStats::colQuantiles( matrix( currBootTbl$bootStat,
                                         ncol = 1),
                                 probs = 0.975 ) %>%
  setNames( NULL )
z01 = calcBCaZ0( currBootTbl$bootStat, currBootTbl$origStat[1])
a1 = calcBCaA( respVec[splitVec==1], calcStat )
currBootTbl = bootTbl %>% filter( split == "2" )
lb2 = matrixStats::colQuantiles( matrix( currBootTbl$bootStat,
                                         ncol = 1),
                                 probs = 0.025 ) %>%
  setNames( NULL )
ub2 = matrixStats::colQuantiles( matrix( currBootTbl$bootStat,
                                         ncol = 1),
                                 probs = 0.975 ) %>%
  setNames( NULL )
z02 = calcBCaZ0( currBootTbl$bootStat, currBootTbl$origStat[1])
a2 = calcBCaA( respVec[splitVec==2], calcStat )
expTbl = tibble( split = c("1","2"), lb = c( lb1, lb2 ),
                 ub = c( ub1, ub2 ),
                 z0CI = c( z01, z02 ), aCI = c( a1, a2 ) )
expNATbl = expTbl %>% mutate( aCI = NA )

# Tests
test_that( "calcCITbl works for method == bca & no diff", {
  # perc, diff
  expect_identical( calcCITbl( bootData = bootTbl, alpha = 0.05, method = "perc",
                               diff = TRUE, bcaAVec = bcaAVec ),
                    expNATbl )
  # perc, no diff
  expect_identical( calcCITbl( bootData = bootTbl, alpha = 0.05, method = "perc",
                               diff = FALSE, bcaAVec = bcaAVec ),
                    expTbl )
})


