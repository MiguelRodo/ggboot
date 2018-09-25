library(vacccompdataprep)
library(magrittr)
library(plyr)
library(dplyr)
library(ggboot)
library(cowplot)
library(stringr)
library(cowplot)
library(tidyr)

fullVaccLabelVec[8] = "No vaccine"
vaccStimLineTypeVec["1tb104"] = "11"

lTbl0 = tmaxSub17ExcTbl %>%
  filter( cd == 4 & infxn == 0 ) %>%
  mutate( cytCombo = str_sub( cytCombo, 4 ) ) %>%
  group_by( ptid ) %>%
  filter( sum( abs( tmax ) ) > 0.02 ) %>%
  ungroup()

lPropTbl0 = lTbl0 %>%
  group_by( ptid ) %>%
  mutate( resp = tmax / sum( abs(tmax) ) ) %>%
  ungroup() %>%
  select( -tmax ) %>%
  mutate( cytCombo = shortCytComboLabVec[cytCombo]) %>%
  select( -timePoint )

wTbl0 = lPropTbl0 %>% filter( cd == 4 ) %>% spread( key = cytCombo, value = resp ) %>% filter( vaccine != 1 )

wInfoTbl0 = wTbl0 %>% select_if( is.character )
wNumTbl0 = wTbl0 %>% select_if( is.numeric )

debugonce( stats::mean )
debugonce( ggbootMV)
ggbootMV( data = wNumTbl0 %>% select( `G-2+T+`,everything() ),
               group = wInfoTbl0$vaccine, B = 2e3,
               seed = 2, dispPlot = TRUE, labelVec = fullVaccLabelVec,
               pcaAxesLabSize = 4.3, colVec = vaccColVec,
               legendTitle = "Vaccine", textAdjVal = 0.04,
               selAxisLab = c( "G-2+T+", "G-2+T-", "G+2+T+" ), arrow = TRUE, arrowSize = 1.25  ) +
  theme_cowplot( font_size = 18, line_size = 1 ) +
  theme( legend.title = element_blank(),
         legend.key.height = unit( 0.75, "cm" ) )

p1 = ggbootMV( data = wNumTbl1 %>% select( `G-2+T+`,everything() ),
               group = wInfoTbl1$vaccine, B = 1e2,
               seed = 2, dispPlot = TRUE, labelVec = fullVaccLabelVec,
               pcaAxesLabSize = 4.3, colVec = vaccColVec,
               legendTitle = "Vaccine", textAdjVal = 0.04,
               selAxisLab = c( "G+2+T+", "G+2-T-" ), arrow = TRUE, arrowSize = 1.25  ) +
  theme_cowplot( font_size = 18, line_size = 1 ) +
  theme( legend.title = element_blank(),
         legend.key.height = unit( 0.75, "cm" ) )

p2 = plot_grid( p0, p1, ncol = 1, align = 'h', labels = c( "Uninfected", "Infected" ), label_x = 0 )

cowplot::ggsave( "C:/OD/OneDrive - University of Cape Town/Work/External_Output/Journals/2018/Grouping_and_Characterising_Novel_TB_Vaccine_Candidates/Figure Selection/Figures/fig-prof-biplot-sum-cd4-i.pdf", units = "cm", width = 18, height= 28, scale = 1 )
```

