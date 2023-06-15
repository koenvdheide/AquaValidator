library(tidyverse)
library(readxl)

excel_results_reader <- function(filePath, sheet = NULL) {
  # idee van: https://readxl.tidyverse.org/articles/cell-and-column-types.html#peek-at-column-names
  # om tegelijk met het inladen de col_types correct te maken, inclusief "list" (ipv "numeric") voor resultaten aangezien niet-numerieke resultaten mogelijk zijn
  # column_names <- names(read_excel(excel_file$datapath,n_max = 0))
  # column_types <- ifelse(grepl(pattern = "^result",x=column_names,ignore.case = TRUE),"list","guess")
  # hierna, if col_types = character verander naar factor behalve voor opmerkingen
  
  
  #nog validatie check nodig dat het werkelijk een excel bestand is
  
  
  #column_names <- read_excel(filePath, n_max=0) #yeah this means double loading but alternative is scuffed, only looks at first row anyway
  #column_types <- if_else(grepl((column_names %>% select(contains("result"))),x=column_names,ignore.case = TRUE),"list","guess")
  excel_data <-
    read_excel(filePath, progress = TRUE, sheet = sheet) %>%
    
    #kan netter?: https://stackoverflow.com/questions/64189561/using-case-when-with-dplyr-across
    mutate(#causes strange grouping results in datatables
      #outputIsText = across(contains("result"), ~ if_else(is.na(as.numeric(.)), TRUE, FALSE)),
      
      # andere mogelijkheid om eerst niet numerieke data etc. uit te filteren dan opnieuw converten via type_convert()?
      across(contains(c(
        "result", "resultaat"
      )) , as.numeric),
      
      # this removes hour/minute/second data for some reason even though %T should cover this, relying on readxl's inbuilt date recognition for now
      # default date recognition does miss MONSTERNAMEDATUM column
      # across(contains(c("datum", "date")),~ as.Date(.x, tryFormats = c("%d-%m-%Y%t%t%T", "%Y-%m-%d%t%t%T", "%Y/%m/%d%t%t%T", "%d-%m-%Y", "%Y-%m-%d", "%Y/%m/%d"))),
      across(contains(
        c(
          "hoednhd",
          "klant",
          "code",
          "smpl",
          "ID",
          "labnummer",
          "status",
          "groep",
          "workday",
          "element",
          "parameter"
          
        )
      ), as.factor))
  
  
  
  #  try({
  
  #poging tot juiste classes
  # excel_data$MEETPUNT <- as.factor(excel_data$MEETPUNT)
  # excel_data$RUNNR <- as.factor(excel_data$RUNNR)
  # excel_data$NAME <- as.factor(excel_data$NAME)
  # excel_data$STATUS <-
  #   as.factor(excel_data$STATUS)
  # excel_data$PROJECT <-
  #   as.factor(excel_data$PROJECT)
  # excel_data$PROJECT_OMS <-
  #   as.factor(excel_data$PROJECT_OMS)
  # excel_data$LABNR <-
  #   as.factor(excel_data$LABNR)
  # excel_data$TESTCODE <-
  #   as.factor(excel_data$TESTCODE)
  # excel_data$ID <- as.factor(excel_data$ID)
  # excel_data$USEDRESULT <-
  #   as.numeric(excel_data$USEDRESULT) #dit geeft problemen bij niet-numerieke results! (zoals "+")
  # excel_data$ELEMENTCODE <-
  #   as.factor(excel_data$ELEMENTCODE)
  # excel_data$REFCONCLUSION <-
  #   as.logical(excel_data$REFCONCLUSION)
  #
  # }, silent = TRUE)
  return (excel_data)
}


path <-
  "oefeningen\\fiatteerlijst+historische_data_samengevoegd_11_05_23.xlsx"

samples <- excel_results_reader(path, sheet = 1)
results <-
  excel_results_reader(path, sheet = 2) #%>% nest(.by = MONSTERPUNTCODE) %>% nest(.by = LABNUMMER)

summary_data <-
  results %>% group_by(LABNUMMER) %>%
  filter(TESTCODE %in% c("bzv","nka","onopa") | ELEMENTCODE %in% c("CZV")) %>%
  pivot_wider(
    id_cols = c(LABNUMMER, RUNNR),
    names_from = c(TESTCODE, ELEMENTCODE),
    values_from = RESULTAAT,
    names_sep = "\t",
    unused_fn = list(MEASUREDATE = list, SAMPLINGDATE = min) #, CZV_BZV_Ratio = list, CZV_NKa_Ratio = list, BZV_onopa_Ratio = list)
  )

    
   # CZV_BZV_Ratio = RESULTAAT[TESTCODE == "czv"] / RESULTAAT[TESTCODE == "bzv"],
    # 
    # CZV_NKa_Ratio = RESULTAAT[TESTCODE == "czv"] / RESULTAAT[TESTCODE == "nka"],
    # 
    # BZV_onopa_Ratio = RESULTAAT[TESTCODE == "bzv"] / RESULTAAT[TESTCODE == "onopa"]
  

View(summary_data)
