require(ggplot2)
require(readxl)
require(dplyr)
require(lubridate)

library(ggstream)

column_names <- read_excel("oefeningen\\kampeninfluent.xlsx", n_max=0) #yeah this means double loading but alternative is scuffed, only looks at first row anyway
print(column_names)


result_file <- read_excel("oefeningen\\kampeninfluent.xlsx") %>%
  mutate(
    resultIsText = across(contains("result"),  ~ if_else(is.na(as.numeric(
      .
    )), TRUE, FALSE)),
    # mutate(across(
    #   contains(c("result", "resultaat")),
    #   ~ case_when(is.na(as.numeric(.)) ~ result_file$textResult[{{.}}],
    #                 .default = as.numeric(.))
    
    # across(contains(c(
    #    "result", "resultaat"
    #  )) , as.numeric),
    across(contains(c("datum", "date")), as.Date),
    across(contains(
      c(
        "hoednhd",
        "klant",
        "code",
        "ID",
        "labnummer",
        "status",
        "groep",
        "workday",
        "element",
        "parameter"
        
      )
    ), as.factor)
    
  )
#result_file <- type.convert(result_file, as.is = TRUE)
summary(result_file)





#juiste classes en namen
result_file$MEETPUNT <- as.factor(result_file$MEETPUNT)
result_file$NAME <- as.factor(result_file$NAME)
result_file$PROJECT <- as.factor(result_file$PROJECT)
result_file$PROJECT_OMS <- as.factor(result_file$PROJECT_OMS)
result_file$LABNR <- as.factor(result_file$LABNR)
result_file$TESTCODE <- as.factor(result_file$TESTCODE)
result_file$ID <- as.factor(result_file$ID)
result_file$USEDRESULT <- as.numeric(result_file$USEDRESULT)
result_file$ELEMENTCODE <- as.factor(result_file$ELEMENTCODE)
result_file$REFCONCLUSION <- as.logical(result_file$REFCONCLUSION)

levels(result_file$MEETPUNT) <- list(Mengmonster = 'o56infl', IJsselmuiden = 'o56infl-ij' , Kampen = 'o56infl-ka' )

top_n_results <- function(n = 10, full_results){
  #nested_results <- full_results %>% nest(.by = c(MEETPUNT, LABNR), .key = "TESTS") %>% nest(.by = MEETPUNT, .key = "SAMPLES")
  #top_labnummers <- nested_results[[2]]
  #top_labnummers <- nested_results %>% pluck(2) %>% (head(n=n))
  #top_labnummers <- nested_results %>% map(1)
  
  #pick top 10 labnummers per monsterpuntcode
  #top_results <- full_results %>% group_by(MEETPUNT, LABNR)  %>% filter(cur_group_id() >= n_groups(.)-n )
  top_results <- full_results %>% group_by(MEETPUNT)  %>% group_modify(~ head(.x, n))
  return(top_results)
}

test <- top_n_results(full_results = result_file)
View(test)
print(length(unique(test$LABNR)))



#data_kampen <- subset(result_file,MEETPUNT=='Kampen')



# plot = ggplot(data = data_kampen, mapping = aes(x = SAMPLINGDATE, y = USEDRESULT)) + 
#     geom_point(size=1.5, alpha=0.8, aes(colour = MEETPUNT)) +
#     #geom_smooth(method="loess", fullrange = TRUE, span = 0.75, linewidth = 2)+ 
#     facet_wrap(vars(TESTCODE),scales = 'free_y') +
#     labs(title = 'RWZI Kampen Influent Test Resultaten (o56infl-ka)', x = 'DATUM', y = 'MEETWAARDE', colour = 'Project Omschrijving')
# show(plot)

# data_bzv_czv <- subset(data_kampen, TESTCODE=='bzv'| TESTCODE== 'czv')
# bzv_czv_plot = ggplot(data = data_bzv_czv, mapping = aes(x = SAMPLINGDATE, y = USEDRESULT)) +
#       geom_point(size=2.5, alpha=0.8, aes(colour = REFCONCLUSION, shape = TESTCODE))
# show(bzv_czv_plot)
# 
# 
# data_kampen_nka <- subset(data_kampen,TESTCODE == 'nka')
# data_kampen_nka %>% arrange(parse_date_time(data_kampen_nka$SAMPLINGDATE,"Ymd HMS"))
# acf(data_kampen_nka$USEDRESULT)
