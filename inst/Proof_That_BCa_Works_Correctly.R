mechScoreVec = c( 7, 44, 49, 59, 34, 46, 0,  32, 49, 52, 44, 36, 42, 5,  22, 18, 41, 48, 31, 42, 46, 63 )
vecScoreVec = c(51,  69, 41, 70, 42, 40, 40, 45, 57, 64, 61, 59, 60, 30, 58, 51, 63, 38, 42, 69, 49, 63 )

x = mechScoreVec
y = vecScoreVec
calcSampCC = function( x, y ){
  sum( (x-mean(x)) * (y-mean(y)) ) / ( sum( ( x - mean(x))^2 ) * sum( ( y - mean(y))^ 2) )^0.5
}
calcSampCC( mechScoreVec, vecScoreVec )

library(boot)
dataMat = cbind( mechScoreVec, vecScoreVec) %>% as.matrix()
bootObj = boot( dataMat,
  function( x, w ) calcSampCC( x[w,1], x[w,2] ),
  R = 1e4 )
debugonce( boot:::bca.ci )
boot.ci( boot.out = bootObj, type = c( "perc", "bca" ) )

# own calculation

bootVec = bootObj$t
origStat = calcSampCC( mechScoreVec, vecScoreVec )
quantile( bootVec, 0.025 )
quantile( bootVec, 0.975 )

# preliminaries
z0 = calcBCaZ0Boot( bootVec, origStat )
a = calcBCaA( x, mean )
jackVec = llply( seq_along( mechScoreVec ), function( i ) calcSampCC( mechScoreVec[ -i ], vecScoreVec[-i]  ) ) %>% unlist()
if( calcLU( jackVec ) < length( jackVec ) /2 ) return( 0 ) # return 0 if there are few unique entries
num = sum( ( mean(jackVec) - jackVec )^3 )
den = sum( ( jackVec - mean(jackVec) )^2 )^1.5
a = num / den / 6
# preliminaries
z0 = calcBCaZ0Boot( bootVec, origStat )
a = calcBCaA( origVec, calcStat )
alpha = 0.975
za = qnorm( alpha )
# second term
num = z0 + za
den = 1 - a * ( z0 + za )
term2 = num/den

# output the quantile

setNames( matrixStats::colQuantiles( matrix( bootVec, ncol = 1 ),
                                     probs = pnorm( z0 + term2 ) ), NULL )
# setNames( quantile( bootVec, pnorm( z0 + term2 ) ), NULL )
calcBCaA( irisRespVec, skewness )
skewness(irisRespVec)
irisRespVec = iris %>%
  as_tibble() %>%
  dplyr::filter( Species == 'setosa' ) %>%
  extract2( "Petal.Width" )
hist( irisRespVec )
skewVec = rep( 0, length(irisRespVec ))
i = 1
for( i in 1:length( skewVec ) ){
  skewVec[i] = skewness( irisRespVec[-i] )
}
skewness( irisRespVec[-1])
skewness( skewVec )
skewVec
jackVec = llply( seq_along( mechScoreVec ),
                 function( i ) calcSampCC( mechScoreVec[ -i ], vecScoreVec[ -i]  ) ) %>% unlist()
if( calcLU( jackVec ) < length( jackVec ) /2 ) return( 0 ) # return 0 if there are few unique entries
num = sum( ( jackVec - mean(jackVec) )^3 )
den = sum( ( jackVec - mean(jackVec) )^2 )^1.5
a = num / den / 6
hist( jackVec )
?skew
calcBCaA
calcSampCC
library(e1071)
jack
n
calcBCaA
za = qnorm( 0.025 )
boot.ci
# second term
num = z0 + za
den = 1 - a * ( z0 + za )
term2 = num/den
boot:::bca.ci
# output the quantile

w <- qnorm(sum(t < t0)/length(t))
if (!is.finite(w))
  stop("estimated adjustment 'w' is infinite")
conf = 0.95
alpha <- (1 + c(-conf, conf))/2
zalpha <- qnorm(alpha)
if (is.null(L))
  L = empinf( bootObj, index = 1, t= t.0 )
  L <- empinf(boot.out, index = index, t = t.o, ...)
a <- sum(L^3)/(6 * sum(L^2)^1.5)
debugonce( boot:::bca.ci )
if (!is.finite(a))
  stop("estimated adjustment 'a' is NA")
adj.alpha <- pnorm(w + (w + zalpha)/(1 - a * (w + zalpha)))
qq <- norm.inter(t, adj.alpha)
cbind(conf, matrix(qq[, 1L], ncol = 2L), matrix(hinv(h(qq[,
                                                          2L])), ncol = 2L))
?boot:::norm.inter
norm
?norm
setNames( matrixStats::colQuantiles( matrix( bootVec, ncol = 1 ),
  probs = pnorm( z0 + term2 ) ), NULL )

# compare interval to table 11.1 on page 189 in Computer Age Statistical Inference
# data
currTbl = bl17ExcTbl %>%
  filter( cd == 4 & infxn == 0 ) %>%
  group_by(  vaccine, infxn, ptid) %>%
  summarise( resp = sum( frequency ) ) %>%
  ungroup() %>%
  filter( vaccine == 1 )

# bootstrap.ci calc

## botstrap
bootObj = boot( currTbl$resp,
                function( x, w ) mean( x[w] ),
                R = 1e4 )
boot.ci( boot.out = bootObj, type = "bca" )

# preliminaries
bootVec = bootObj$t
origStat = mean( currTbl$resp )
origVec = currTbl$resp
calcStat = mean
alpha = 0.025
z0 = calcBCaZ0Boot( bootVec, origStat )
a = calcBCaA( origVec, calcStat )
za = qnorm( alpha )

# second term
num = z0 + za
den = 1 - a * ( z0 + za )
term2 = num/den

# output the quantile
boot.ci( bootObj, type = c( "bca", "perc" ) )

setNames( matrixStats::colQuantiles( matrix( bootVec, ncol = 1 ),
                                     probs = pnorm( z0 + term2 ) ), NULL )
setNames( matrixStats::colQuantiles( matrix( bootVec, ncol = 1 ),
                                     probs = pnorm( z0 + term2 ) ), NULL )

function( bootVec, origVec, alpha, origStat, calcStat )
