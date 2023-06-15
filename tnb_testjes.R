require(ggplot2)
require(readxl)
require(dplyr)

library(ggridges)
result_file <- read_excel("oefeningen\\tnbtestjes.xlsx")

#juiste classes en namen
result_file$MEETPUNT <- as.factor(result_file$MEETPUNT)
result_file$NAME <- as.factor(result_file$NAME)
result_file$PROJECT <- as.factor(result_file$PROJECT)
result_file$PROJECT_OMS <- as.factor(result_file$PROJECT_OMS)
result_file$LABNR <- as.factor(result_file$LABNR)
result_file$TESTCODE <- as.factor(result_file$TESTCODE)
result_file$ID <- as.factor(result_file$ID)
result_file$USEDRESULT <- as.numeric(result_file$USEDRESULT) #dit geeft problemen bij niet-numerieke results! (zoals "+")
result_file$ELEMENTCODE <- as.factor(result_file$ELEMENTCODE)
result_file$REFCONCLUSION <- as.logical(result_file$REFCONCLUSION)
result_file$SOORTWATER <- as.factor(result_file$SOORTWATER)


data_stikstof <- droplevels(subset(result_file,TESTCODE=='nh4'|TESTCODE=='nka'|TESTCODE=='nkj'|TESTCODE=='no2'|TESTCODE=='not'|TESTCODE=='tnb'))

#test_stikstof <- subset(data_stikstof$LABNR > '')
#data_stikstof$'nkj+not'<- 

#extreme outlier
#data_stikstof <-  filter(data_stikstof, LABNR!='2115822')

#2de en 3de lijns kwaliteitscontroles weghalen
data_stikstof <-  filter(data_stikstof, PROJECT_OMS!= 'Laboratorium kwaliteit 2e lijns')
data_stikstof <-  filter(data_stikstof, PROJECT_OMS!= 'Laboratorium kwaliteit 3e lijns')
attach(data_stikstof)

plot = ggplot(data = data_stikstof, mapping = aes(x = LABNR, y = USEDRESULT, fill = TESTCODE)) + 
  geom_density_ridges()+
  #geom_point(alpha=0.75, aes(colour = REFCONCLUSION, shape = TESTCODE)) +
  #geom_smooth(method="loess", fullrange = TRUE, span = 0.75, linewidth = 2)+ 
  #facet_wrap(vars(SOORTWATER),scales = 'free_y') +
  #theme(axis.text.x=element_blank())
  labs(title = 'RWZI Kampen Influent Tests (o56infl-ka)', x = 'DATUM', y = 'MEETWAARDE')
show(plot)


boxplot = ggplot(data = data_stikstof, mapping = aes(x = TESTCODE, y = USEDRESULT)) +
  geom_boxplot(aes(fill = TESTCODE)) +
  facet_wrap(vars(SOORTWATER), scales = 'free_y') +
  labs(title = 'Stikstof test boxplots', subtitle = 'van alle analyses sinds 2022 waarvoor nkj EN tnb tests werden aangevraagd',x = '', y = 'MEETWAARDE')
  show(boxplot)

detach(data_stikstof)



