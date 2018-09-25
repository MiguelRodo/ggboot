library(vacccompdataprep)
library(magrittr)
library(plyr)
library(dplyr)
library(stringr)
library(cowplot)
library(gam)
library(doParallel)


fullVaccLabelVec[8] = "No vaccine"
vaccStimLineTypeVec["1tb104"] = "11"

# infected

## long table
lTbl1 = bl17ExcITbl %>%
  filter( cd == 4 & infxn == 1 ) %>%
  mutate( cytCombo = str_sub( cytCombo, 4 ),
          vaccStim = str_c( vaccine, stim ) ) %>%
  group_by(  vaccine, infxn, ptid, stim, vaccStim ) %>%
  summarise( resp = sum( frequency ) ) %>%
  ungroup()

# uninfected

## long table
lTbl0 = bl17ExcITbl %>%
  filter( cd == 4 & infxn == 0 ) %>%
  mutate( cytCombo = str_sub( cytCombo, 4 ),
          vaccStim = str_c( vaccine, stim ) ) %>%
  group_by(  vaccine, infxn, ptid, stim, vaccStim ) %>%
  summarise( resp = sum( frequency ) ) %>%
  ungroup()


# CHANGE seB and B back to 1e4 and 2

set.seed(2)
ggbootUV( data = lTbl0,
          resp = resp, xAxis = vaccStim, col = vaccine, pMethod = "percT", ciMethod = "bca", seB = 2e2,
          B = 2e3, altSide = 'high', eqErrBarWidth = TRUE, errBarLineType = "11", calcStat = "mean", trim = 0.2,
          rotXText = TRUE, pointSize = 3, errBarSize = 1.75, yLab = "Baseline antigen-specific\nTh1 cytokine+ CD4 T cell frequency (%)",
          nullValue = 0, xLab = "Stimulation antigen", xAxisLabVec = vaccStim2StimLabVec, errBarAlpha = 0.88,
          colLabName = "", colourVec = vaccColVec, colLabVec = fullVaccLabelVec,
          facetLabVec = infLabelVec, nCol = 3, facetScale = 'free_x', fontScaleFactor = 2,
          lineScaleFactor = 3, plotTblName = "bSizeRegStimCd4MeanTbl", bootSECorr = TRUE ) +
  theme( legend.key.height = unit( 12, "mm" ),
         legend.key.width = unit( 12, "mm" ),
         strip.text = element_text( size = 32, colour = 'black' ),
         legend.title = element_blank(),
         axis.title = element_text( size = 22 ),
         strip.background = element_rect(linetype = 'solid', size = 1, fill = 'white',
                                         colour = 'white' ),
         axis.text.x = element_text( hjust = 1, angle = 90, size = 18, vjust = 0.45 ) ) +
  guides( linetype = guide_legend(override.aes = list(size=1.8)),
          colour = guide_legend( order = 1 ),
          bootSECorr = FALSE )
