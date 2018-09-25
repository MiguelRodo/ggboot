library(vacccompdataprep)
library(magrittr)
library(plyr)
library(dplyr)
library(cowplot)


sizeTbl = tmaxSub17ExcTbl %>%
  filter( cd == 4 ) %>%
  group_by( vaccine, infxn, ptid ) %>%
  summarise( resp = sum( resp ) ) %>%
  ungroup()

set.seed(6)
p0 = ggbootPW( sizeTbl, resp = resp, facet = infxn,
          group = vaccine, B = 5, seB = 3,
          facetLabVec = infLabelVec,
          groupLabVec = fullVaccLabelVec,
          facetScale = 'fixed',
          facetOrderVec = c( "1", "3", "4", "5", "6" ),
          plotTblName = "plotTbl",
          rotX = TRUE, fdr = 0.01)

set.seed(5)
p1 = ggbootPW( sizeTbl, resp = resp, facet = infxn,
               group = vaccine, B = 2e3, seB = 2e2, minEff = 0.02,
               fdr = 0.01, plotTblName = "testPWTbl" )

