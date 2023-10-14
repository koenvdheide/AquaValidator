#reactlog_enable()
aquaApp <- function(...){
options(shiny.maxRequestSize=30*1024^2)

#UI input variables are intentionally in Dutch, makes it easier to keep them separate from output/internal variables on the server side
ui <- function(request) {
  tagList( 
  shinyjs::useShinyjs(), #enable javascript for some more complex modifications of our tables and plots
  navbarPage(
    title= div(tags$img(src='Logo-Aqualysis-RGB-HR.png', align = 'left', width = "130px", height = "34px"),
               HTML('&emsp;'),
               "AquaValidator"),
    
    tabPanel("Fiatteren",
           fluidPage(
             tabsetPanel(
               id = "fiatteer_beeld",
               
               tabPanel(
                 title = "Bestand",
                 value = "tab_bestand",
                 br(),
                 fileInput(
                   "input_file",
                   label = "Kies Excel samplelijst",
                   accept = c(".xlsx", ".xls") #ook (bijvoorbeeld) csv en tsv?
                 )
             ),
               
               tabPanel(
                 title = "Fiatteerlijst",
                 value = "tab_fiatteerlijst",
                 actionButton("tabel_fiatterlijst_wis_selectie", "Wis Selectie"),
                 DT::dataTableOutput("tabel_fiatteerlijst")
               ),
             
               tabPanel(
                 title = "Testresultaten",
                 value = "tab_testresultaten",
                 actionButton("tabel_testresultaten_wis_selectie", "Wis Selectie"),
                 radioButtons(
                   "instellingen_roteer_tabel",
                   label = "Gebruik als kolom:",
                   choices = c("Labnummer" = "labnr",
                               "Tests" = "test",
                               "Resultaat Info" = "result_info"),
                   inline = TRUE
                 ),
                  checkboxInput("instellingen_toon_historie_tabel",
                                "Toon meer monsters van geselecteerde meetpunt(en)",
                                value = FALSE),
                 DT::dataTableOutput("tabel_sampleresults")
               ),
             
               tabPanel(
                 value = "tab_fiatteer_grafiek",
                 title = "Grafieken",
                 plotOutput(
                   "fiatteer_grafiek",
                   click = "fiatteer_grafiek_klik",
                   dblclick = dblclickOpts(id = "fiatteer_grafiek_dblklik"),
                   hover = hoverOpts(id = "fiatteer_grafiek_zweef"),
                   brush = brushOpts(id = "fiatteer_grafiek_gebied")
                 ),
                 plotOutput(
                   "ratios_grafiek",
                   click = "ratios_grafiek_klik",
                   dblclick = dblclickOpts(id = "ratios_grafiek_dblklik"),
                   hover = hoverOpts(id = "ratios_grafiek_zweef"),
                   brush = brushOpts(id = "ratios_grafiek_gebied")
                 ),
                 DT::dataTableOutput("fiatteer_grafiek_tabel")
                 
               )
             )
           )),
  
  tabPanel("Instellingen",
           id = "instellingen_tab",
           
           sidebarLayout(sidebarPanel(
             checkboxInput("instellingen_extra_opties",
                           "Extra opties tonen?",
                           value = FALSE)
           ),
           mainPanel(
             tabsetPanel(
               
               tabPanel(
                 "Algemeen",
                 # selectInput(
                 #   "instellingen_gebruiker",
                 #   label = "Gebruiker",
                 #   choices = list( "Matthijs Dobbelaar", "Hans Kieftenbelt", "Angelique Vollenbroek"),
                 #   
                 # ),
                 numericInput(
                   "instellingen_hoeveelheid_resultaten",
                   "Hoeveel resultaten moeten er per monsterpunt worden getoond?",
                   value = 10,
                   min = 1
                 ),
                 bookmarkButton(
                   label = "Sla instellingen op",
                   title = "Sla huidige instellingen op, ze worden automatisch ingeladen als je de validator later weer opstart."
                 )
               ),
               tabPanel(
                 "Input",
                 textInput(
                   "input_file_fiatteer_blad",
                   label = "Naam fiatteerlijst in Excel (hoofdlettergevoelig!)",
                   value = "fiatteerlijst",
                   placeholder = "Naam van het blad met de fiatteerlijst"
                 ),
                 textInput(
                   "input_file_resultaten_blad",
                   label = "Naam resultaten in Excel (hoofdlettergevoelig!)",
                   value = "resultaten",
                   placeholder = "Naam van het blad met de resultaten"
                 ),
                 br(),
                 textInput(
                   "input_file_labnummer_kolom",
                   label = "Welke kolom identificeert een sample?",
                   value = "LABNUMMER",
                   placeholder = "Naam van de kolom met een unieke waarde per sample"
                 ),
                 textInput(
                   "input_file_meetpunt_kolom",
                   label = "Welke kolom identificeert een meetpunt?",
                   value = "MONSTERPUNTCODE",
                   placeholder = "Naam van de kolom met een unieke waarde per meetpunt"
                 ),
                 textInput(
                   "input_file_testresultaat_kolom",
                   label = "Welke kolom identificeert een testresultaat?",
                   value = "RESULTAAT",
                   placeholder = "Naam van de kolom met de testresultaten"
                 ),
               ),
               tabPanel("Fiatteerlijst"), 
               
               tabPanel("Testresultaten")
               
             )
           ))),
  
  tabPanel("Hulp",
           id = "hulp_tab",
           sidebarLayout(sidebarPanel(),
                         mainPanel())),
  
  footer = div( actionButton("button_valideer", label = "Samples valideren"),
                actionButton("button_duplo_aanvraag", label = "Tests duplo aanvragen"),
                actionButton("button_cancel", label = "Tests cancellen"))
)
)}
server <- function(input, output, session) {
##################### common server variables #######################  
  
  #settings <- reactiveValues(
  #  username = NULL
  #)
  
  #database connection
  # sql_connection_string <-
  #   "Driver=Oracle in OraClient19Home1;Host=;Port=1521;"
  # 
  # sql_connection <- DBI::dbConnect(
  #   odbc::odbc(),
  #   Driver = 'Oracle in OraClient19Home1',
  #   Host = 'db01-dcz-olin',
  #   Service = QMP,
  #   #SVC    = "(schema)",
  #   UID = 'kheide',
  #   #PWD = askpass(prompt = "DB paswoord:"),
  #   Port = 1521
  # )
  
  #input file data is split up into these variables
  fiatteer_samples <- reactiveVal(tibble()) #"fiatteerlijst" sheet
  complete_results <- reactiveVal(tibble()) #"resultaten" sheet
  complete_ratios <- tibble() #ratios calculated from the "resultaten" sheet
  


  
  #proxies are needed to reflect user edits onto the underlying data (like when adding comments)
  fiatteerlijst_proxy <- DT::dataTableProxy("tabel_fiatteerlijst")
  sampleresults_proxy <- DT::dataTableProxy("tabel_sampleresults")
  results_table_dataframe <- tibble() #The results tab has multiple possible layouts. To make sure edits and select commands work well we need a tibble to save the data in its current layout.
  
  #when user selects points in graphs that selection ends up in these variables
  #renamed to "highlighted" samples to avoid  confusion with "selected"  samples in tables
  plot_highlighted_samples <- reactiveVal() #filled when user does a double click or box drag selection for results plot 
  plot_hovered_samples <- reactiveVal() #filled when user hovers over points for results plot
  plot_highlighted_ratios <- reactiveVal() #filled when user does a double click or box drag selection for ratios plot
  
  #output
  validated_samples <- tibble()
  validated_results <- tibble()
  validated_path <- "F:/2-Ano/Alg/13_Fiatteren/Validator/samples_goedgekeurd.csv"
  
  duplo_samples <- tibble()
  duplo_results <- tibble()
  duplo_path <- "F:/2-Ano/Alg/13_Fiatteren/Validator/resultaten_duplo_aangevraagd.csv"
  
  cancelled_samples <- tibble()
  cancelled_results <- tibble()
  cancelled_path <- "F:/2-Ano/Alg/13_Fiatteren/Validator/resultaten_cancelled.csv"
  
  
  
#####################################loading file###############################
  #This observer triggers when the user has selected a (Excel) file. The observer first retrieves the filepath and expected Excel sheet names.
  #it then calls excel_reader to read two Excel sheets and assigns each sheet to its own tibble. Finally ratios are calculate from the results tibble.
  observeEvent(input$input_file, {
    loadingtip <- showNotification("Laden...", duration = NULL, closeButton = FALSE)
    
    tryCatch({
      file_path <- input$input_file$datapath
      fiatteerblad <- input$input_file_fiatteer_blad
      resultatenblad <- input$input_file_resultaten_blad
      
      # measurepointcolumn <- input$input_file_meetpunt_kolom
      # resultscolumn <- input$input_file_testresultaat_kolom
      # labnrcolumn <- input$input_file_labnummer_kolom
      
      fiatteer_samples(excel_reader(
        file_path,
        sheet = fiatteerblad
        
        #resultcolumn = resultscolumn,
        #labnummercolumn = labnrcolumn,
        #meetpuntcolumn = measurepointcolumn
      ) %>% arrange(PRIOFINISHDATE))
      
    }, error = function(e){
      showModal(modalDialog(title = "Probleem met inladen van fiatteerlijst:",e)) #geef de error als een popup scherm zodat de gebruiker het ziet
    })
    
    tryCatch({
      complete_results(
        excel_reader(
          file_path,
          sheet = resultatenblad
          
          #resultcolumn = resultscolumn,
          #labnummercolumn = labnrcolumn,
          #meetpuntcolumn = measurepointcolumn
        ) %>% mutate(
          RESULTAAT_ASNUMERIC = if_else(TESTSTATUS != 1000, as.numeric(RESULTAAT), NA),
          GEVALIDEERD = TESTSTATUS == 300,
          UITVALLEND = TESTSTATUS != 300 & TESTSTATUS != 1000 & REFCONCLUSION == 0)
      )
      
      complete_ratios <<- calculate_ratios(complete_results())
      
    }, error = function(e){
      showModal(modalDialog(title = "Probleem met inladen van meetresultaten:",e)) #geef de error als een popup scherm zodat de gebruiker het ziet
    })
    
    on.exit(removeNotification(loadingtip), add = TRUE)
    on.exit(freezeReactiveValue(input,"fiatteer_beeld" ), add = TRUE)
    on.exit(updateTabsetPanel(inputId = "fiatteer_beeld", selected = "tab_fiatteerlijst"), add = TRUE) #move user's view to fiatteerlijst tab when loading is done.

  })
  
  load_two_sheet_excel_file <- function(filePath){
    
  }
  
#' Just a wrapper for readxl's read_excel that includes some datatype casting for columns we expect to be in the file.
#' 
#' @param filePath URL to the Excel file.
#' @param sheet name or index of the Excel sheet to read in.
#'
#' @return a tibble as returned by read_excel.
#'
  excel_reader <-    function(filePath,
                               sheet = NULL
                               #resultcolumn,
                               #labnummercolumn,
                               #meetpuntcolumn
                               ){
      
    excel_data <- readxl::read_excel(filePath, progress = TRUE, sheet = sheet) %>%
        mutate(
          # this removes hour/minute/second from sampling&measurement dates for some reason even though %T should cover this, relying on readxl's inbuilt date recognition for now
          # default date recognition doesn't see MONSTERNAMEDATUM column as valid dates for some reason
          # try parse_date_time() instead?
          across(contains(c("datum", "date")),
                 ~ as.Date(.x, tryFormats = c("%d-%m-%Y%t%t%T", "%Y-%m-%d%t%t%T", "%Y/%m/%d%t%t%T", 
                                              "%d-%m-%Y", "%Y-%m-%d", "%Y/%m/%d")
                          )
                 ),
          across(contains(
            c("hoednhd",
              "klant",
              "code",
              "smpl",
              "ID",
              "labnummer",
              "status",
              "groep",
              "workday",
              "element",
              "parameter",
              "soortwater")
            ), as.factor),
        ) 
    return (excel_data)
    }
  
  # this commented out block was an attempt to generalize ratio calculation code, didn't work out.
  # ratios_list <- list(
  #   BZV_ONOPA = c("BZV5","onopa", "ELEMENTCODE", "TESTCODE"),
  #   CZV_BZV = c("CZV","BZV5", "ELEMENTCODE", "ELEMENTCODE"),
  #   CZV_NKA = c("CZV","nka","ELEMENTCODE", "TESTCODE"),
  #   CZV_TNB = c("CZV","tnb","ELEMENTCODE","TESTCODE"),
  #   CZV_TOC = c("CZV","TOC","ELEMENTCODE", "ELEMENTCODE"),
  #   OFOS_TPA = c("ofos","tpa","TESTCODE","TESTCODE")
  # )
  # ratio_calculator <- function(ratio_name){
  #   desired_ratio <- ratios_list[[ratio_name]]
  #   print(desired_ratio[[4]])
  #   ratio_row = ifelse(
  #     any({{desired_ratio[[3]]}} == desired_ratio[[1]]) & any({{desired_ratio[[4]]}} == desired_ratio[[2]]),
  #     RESULTAAT_ASNUMERIC[{{desired_ratio[[3]]}} == desired_ratio[[1]]] / RESULTAAT_ASNUMERIC[{{ desired_ratio[[4]] }} == desired_ratio[[2]]],
  #     NA
  #   )
  #   return(ratio_row)
  #}

#' Takes in sample results, creates a copy of these  results, attempts to calculate (currently) six different ratios per row (returning NA if a ratio is not applicable). Each of the six ratios is its own column.
#' The copied results dataframe is then wiped of all columns except NAAM, SAMPLINGDATE and the ratio columns. This trimmed copy is then pivoted along the ratios so that each ratio column value becomes its own row instead. This pivoted dataframe is then returned. 
#' @param results Sample results to calculate ratios from.
#'
#' @return Same as results parameter.
#'
  calculate_ratios <- function(results){
    
    relevant_results <- results %>% filter(!is.na(RESULTAAT_ASNUMERIC)) #don't bother calculating ratios for results that are not valid numbers
    calculated_ratios <-
      relevant_results %>%
      group_by(LABNUMMER, MONSTERPUNTCODE) %>%
      reframe(
        NAAM = NAAM,
        SAMPLINGDATE = SAMPLINGDATE,
        # CZV_BZV_RATIO = ratio_calculator("CZV_BZV"),
        # CZV_NKA_RATIO = ratio_calculator("CZV_NKA"),
        # BZV_ONOPA_RATIO = ratio_calculator("BZV_ONOPA"),
        # CZV_TOC_RATIO = ratio_calculator("CZV_TOC"),
        # CZV_TNB_RATIO = ratio_calculator("CZV_TNB")
        CZV_BZV_RATIO = ifelse(
          any(ELEMENTCODE == "CZV") & any(ELEMENTCODE == "BZV5"),
          RESULTAAT_ASNUMERIC[ELEMENTCODE == "CZV"] / RESULTAAT_ASNUMERIC[ELEMENTCODE == "BZV5"],
          NA
        ),
        CZV_NKA_RATIO = ifelse(
          any(ELEMENTCODE == "CZV") & any(TESTCODE == "nka"),
          RESULTAAT_ASNUMERIC[ELEMENTCODE == "CZV"] / RESULTAAT_ASNUMERIC[TESTCODE == "nka"],
          NA
        ),
        BZV_ONOPA_RATIO = ifelse(
          any(ELEMENTCODE == "BZV5") & any(TESTCODE == "onopa"),
          RESULTAAT_ASNUMERIC[ELEMENTCODE == "BZV5"] / RESULTAAT_ASNUMERIC[TESTCODE == "onopa"],
          NA
        ),
        CZV_TOC_RATIO = ifelse(
          any(ELEMENTCODE == "CZV") & any(ELEMENTCODE == "TOC"),
          RESULTAAT_ASNUMERIC[ELEMENTCODE == "CZV"] / RESULTAAT_ASNUMERIC[ELEMENTCODE == "TOC"],
          NA
        ),
        CZV_TNB_RATIO =ifelse(
          any(ELEMENTCODE == "CZV") & any(TESTCODE == "tnb"),
          RESULTAAT_ASNUMERIC[ELEMENTCODE == "CZV"] / RESULTAAT_ASNUMERIC[TESTCODE == "tnb"],
          NA
        )
      ) %>% tidyr::pivot_longer(
        cols = c(CZV_BZV_RATIO, CZV_NKA_RATIO, BZV_ONOPA_RATIO,CZV_TOC_RATIO,CZV_TNB_RATIO),
        names_to = "RATIO",
        values_to = "WAARDE",
        values_drop_na = TRUE #needed because ggplot's geom_line adds a break when it encounters an NA value and this would make for pretty ugly ratios plots.
      ) %>% distinct() #any() causes duplicate ratios to be calculated per LABNUMMER, this removes them.
    
    return(calculated_ratios)
  }

############################fiatteerlijst tab######################################## 
  #Here we fill out the fiatteerlijst table (the table that the user actually sees).
  #This table is mostly identical to fiatteer_samples but with two differences: we add a column with rejected tests and we hide a few irrelevant columns from view for the user.
  output$tabel_fiatteerlijst <- DT::renderDataTable({
    req(input$input_file) #don't try to create the table until there's a valid input file
    
    fiatteer_results <- semi_join(complete_results(),fiatteer_samples(), by = c("LABNUMMER")) #get the results belonging to our current samples
    rejected_tests <- get_rejected_tests(fiatteer_results)
    fiatteer_data <- fiatteer_samples() %>% #adds rejected_tests as a fiatteerlijst column to show which tests need validation per sample
                      nest_join(rejected_tests,
                                by = "LABNUMMER",
                                name = "UITVALLENDE_TESTS_LIST") %>%
                                tidyr::hoist(UITVALLENDE_TESTS_LIST, 
                                             UITVALLERS = "TESTCODE")
                              
    table_builder(fiatteer_data, 
                  comment_col = TRUE,
                  columnDefs = list(list(
                    visible = FALSE ,
                    targets = c(
                      "STATUS",
                      "FIATGROEP",
                      "SAMPLE_ID"
                    )
                  )
                )
      )
  })
  
#' Simple function that gets rows that are UITVALLEND (meaning they need validation right now) and returns their LABNUMMER and TESTCODE.
#' @param results Original (full) results dataframe.
#'
#' @return Same as results but with only LABNUMMER and TESTCODE columns.
  get_rejected_tests <- function(results){
    rejected_tests <-
      results %>% 
      filter(UITVALLEND == TRUE) %>% 
      select(LABNUMMER, TESTCODE)
    
  }
  #this observer triggers when a user clicks the "wis selectie" button in the top left.
  observeEvent(input$tabel_fiatterlijst_wis_selectie,{
    DT::selectRows(fiatteerlijst_proxy, selected = NULL)
  })
  
  #this observer triggers when a user edits a column in the fiatteerlijst table (currently only possible for SAMPLE_OPMERKING)
  #reminder that if the order of columns in "fiatteer_samples" gets changed this can potentially edit the wrong columns!
  observeEvent(input$tabel_fiatteerlijst_cell_edit,{
    isolate({
      fiatteer_samples(DT::editData(fiatteer_samples(),
                           input$tabel_fiatteerlijst_cell_edit,
                           proxy = fiatteerlijst_proxy,
                           rownames = FALSE))
            })
    
  })
  
  #Here we watch for user selecting one or more rows in the fiatteerlijst table and return the row data when needed.
  #if this reactive gets called (for example by opening the results tab) while the user has not selected any rows it returns a popup.
  selected_sample_rows <- reactive({
    data <- fiatteer_samples() #redundant but needed because subsetting a reactiveVal is buggy
    selected_rows <- input$tabel_fiatteerlijst_rows_selected
    if (isTruthy(selected_rows)) { 
      return(data[selected_rows,])
    }
    else{
      showModal(modalDialog(
        title = "Geen Selectie",
        "Kies eerst een sample!",
        easyClose = TRUE
      ))
      return()
    }
  })
  
  #This retrieves the test results belonging to USER SELECTED samples in the fiatteerlijst
  selected_sample_current_results <- reactive({
    req(selected_sample_rows())
    selected_labnummer <- select(selected_sample_rows(), LABNUMMER)
    matching_result <- semi_join(complete_results(),
                                 selected_sample_rows(),
                                 by = c('LABNUMMER'))
    return(matching_result)
  })
  
  #This retrieves ALL test results belonging to the same MONSTERPUNTCODE as our selected samples in the fiatteerlijst 
  #(note that this includes the current result)
  selected_sample_historical_results <- reactive({
    selected_meetpunt <- select(selected_sample_current_results(), MONSTERPUNTCODE)
    matching_results <- semi_join(complete_results(),selected_sample_current_results(), by = c('MONSTERPUNTCODE')) %>%
                        arrange(desc(SAMPLINGDATE)) %>% #it SHOULD already put the most recent result first but this ensures it
                        top_n_results(n = input$instellingen_hoeveelheid_resultaten)
    
    plot_highlighted_samples(rep(FALSE, nrow(matching_results))) #fill plot_highlighted_samples so it doesn't throw out of bounds errors later
    return(matching_results)
  })
  
#' Returns the results belonging to n most recent labnummers for each monsterpuntcode.
#' Nightmare code because we have to deal with three group layers, we want all the results (layer 1) of the n most recent labnummers (layer 2) for each monsterpuntcode (layer 3).
#' @param n Maximum number of labnummers to return results for. So n = 10 means this returns the results of the 10 most recent labnummers.
#' @param full_results Dataframe containing the results we want the top n of
#'
#' @return Same as full_results.
#'
  top_n_results <- function(n, full_results, group_one = MONSTERPUNTCODE, group_two = LABNUMMER) {
    top_results <-
      full_results %>% 
      group_by({{group_one}})  %>% 
      group_modify(~ {.x %>% group_by({{group_two}}) %>% 
                      filter(cur_group_id() >= n_groups(.) - n)}) %>% 
      ungroup()
  }
  
#############################results tab########################################
  #Resultaten table. Table consists of results belonging to the samples that user selected in the fiatteerlijst table.
  #There are 3 different possible layouts. "labnr" where each labnummer is its own column, "result_info" where the columns are left as is and "tests" where each kind of test is its own column
  #
  #Because formatStyle can only color table cells with values that are part of the same table we have to add columns to each table which indicate what background colors we want for our result cells. These columns are useless for the users so we hide them from view.
  output$tabel_sampleresults <- DT::renderDataTable({
    results <- historical_or_current_results()
    
    if(input$instellingen_roteer_tabel  == "labnr"){
      
      labnr_widened_results <- results %>% tidyr::pivot_wider(
        id_cols = c(TESTCODE,ELEMENTCODE),
        names_from = c(NAAM, LABNUMMER, HOEDNHD, RUNNR),
        values_from = RESULTAAT,
        names_sep = "<br>",
        names_sort = TRUE,
        unused_fn = list(RESULT_OPMERKING = list, MEASUREDATE = list, SAMPLINGDATE = list))

        #we need this dataframe to color rejected test results red
        labnr_widened_uitvallend <- results %>% tidyr::pivot_wider(
          id_cols = c(TESTCODE,ELEMENTCODE),
          names_from = c(NAAM, LABNUMMER, HOEDNHD, RUNNR),
          values_from = UITVALLEND,
          names_sort = TRUE,
          names_sep = "<br>") %>% mutate(TESTCODE = NULL,
                                         ELEMENTCODE = NULL)
        
        #and we need this dataframe to color cancelled tests green
        labnr_widened_teststatus <- results %>% tidyr::pivot_wider(
          id_cols = c(TESTCODE,ELEMENTCODE),
          names_from = c(NAAM, LABNUMMER, HOEDNHD, RUNNR),
          values_from = TESTSTATUS,
          names_sort = TRUE,
          names_sep = "<br>") %>% mutate(TESTCODE = NULL,
                                         ELEMENTCODE = NULL)
        labnr_widened_combined <- cbind(labnr_widened_results, labnr_widened_uitvallend, labnr_widened_teststatus)

        #here we count columns in a bunch of different ways so that we know the positions of columns to hide in the table that the user sees.        
        extra_column_amount <- seq.int(1,(ncol(labnr_widened_uitvallend) + ncol(labnr_widened_teststatus)), by = 1)
        what_sample_columns_are_there <- results %>% count(NAAM, LABNUMMER, RUNNR)
        number_of_sample_columns <- nrow(what_sample_columns_are_there)
        original_number_of_columns <- ncol(labnr_widened_results)
        original_and_uitvallend_columns <- ncol(labnr_widened_uitvallend) + original_number_of_columns
        total_columns <- ncol(labnr_widened_combined)
        results_columns <- 3:(2 + number_of_sample_columns) #starts at 3 to offset for the elementcode and testcode columns, why do we have to add 2 instead of 3 to offset at the end? NO IDEA
        
        results_table_dataframe <<- labnr_widened_combined
        table_labnr <- table_builder(labnr_widened_combined,
                                     sort_by = 0,
                                     selection = list(mode = "multiple", target = 'cell'),
                                     columnDefs = list(list(
                                       visible = FALSE,
                                       targets = -extra_column_amount))#hide the columns we use for coloring
                                     ) %>%
          DT::formatStyle(
           columns = results_columns, 
           valueColumns = (1 + original_number_of_columns):total_columns,
           target = 'cell',
           backgroundColor = DT::styleEqual(TRUE, 'salmon')
           ) %>%
          DT::formatStyle(
            columns = results_columns, 
            valueColumns = (1 + original_and_uitvallend_columns):total_columns,
            target = 'cell',
            backgroundColor = DT::styleEqual(c(1000,300), c('lightgreen','gray'))
          ) #%>% #rounding removes n.b. results
            #DT::formatRound(columns = results_columns,
            #digits = 3) 
       return(table_labnr)
      
    } else if (input$instellingen_roteer_tabel == "result_info") {
      
      #we need to get these indices because DT accepts only indexes (and not names) for grouping columns
      labnr_column_index <- which(colnames(results) == "LABNUMMER")
      labnr_column_index <- (labnr_column_index[1] - 1) #which() returns vector,first element contains the index we need. We subtract 1 from index because R counts indexes from 1, DT counts from 0
      
      description_column_index <- which(colnames(results) == "NAAM")
      description_column_index <- (description_column_index[1] - 1)
      
      #notice that we don't need to create the extra dataframes for coloring columns here because these columns are already present.
      results_table_dataframe <<- results
      table_sample <- table_builder(
        results,
        sort_by = labnr_column_index,
        comment_col = TRUE,
        group = TRUE,
        group_cols = c(description_column_index,labnr_column_index), 
        columnDefs = list(list(
          visible = FALSE ,
          targets = c(
            "MONSTERPUNTCODE",
            "RESULTAAT_ASNUMERIC",
            "UITVALLEND",
            "NAAM",
            "TESTSTATUS",
            "REFCONCLUSION",
            "UITVALLEND",
            "MEETPUNT_ID",
            "SAMPLE_ID",
            "SAMPLE_TEST_ID",
            "SAMPLE_RESULT_ID",
            "GEVALIDEERD",
            "SAMPLE_OPMERKING",
            #"RESULTAAT_AFGEROND",
            "REFVALUE",
            "SOORTWATER")))
        )  %>% DT::formatStyle(
          columns = 'LABNUMMER',
          valueColumns = 'LABNUMMER',
          backgroundColor = DT::styleEqual(selected_sample_current_results()$LABNUMMER, 'yellow', 
                                           default = 'gray')
        ) %>% DT::formatStyle(
          columns = 'RESULTAAT',
          valueColumns = 'UITVALLEND',
          target = 'cell',
          backgroundColor = DT::styleEqual(TRUE, 'salmon')
        ) %>% DT::formatStyle(
          columns = 'RESULTAAT',
          valueColumns = 'TESTSTATUS',
          target = 'cell',
          backgroundColor = DT::styleEqual(c(1000,300), c('lightgreen','gray'))
        )#%>% DT::formatRound(columns = 'RESULTAAT', 
         #                  digits = 3) #rounding removes n.b. results
      return(table_sample)
      
    } else if (input$instellingen_roteer_tabel == "test") {
        test_widened_results <- results %>% 
          tidyr::pivot_wider(
            id_cols = c(NAAM,LABNUMMER, RUNNR, HOEDNHD),
            names_from = c(TESTCODE, ELEMENTCODE),
            values_from = RESULTAAT,
            names_sep = "<br>",
            names_sort = TRUE,
            unused_fn = list(MEASUREDATE = list, 
                             SAMPLINGDATE = list))
        
        #we need this dataframe to color rejected test results red
        test_widened_uitvallend <- results %>% 
          tidyr::pivot_wider(
            id_cols = c(NAAM,LABNUMMER, RUNNR, HOEDNHD),
            names_from = c(TESTCODE, ELEMENTCODE),
            values_from = UITVALLEND,
            names_sep = "<br>",
            names_sort = TRUE) %>% mutate(NAAM = NULL,
                                          LABNUMMER = NULL,
                                          HOEDNHD = NULL,
                                          RUNNR = NULL)
        
        #and we need this dataframe to color cancelled tests green
        test_widened_teststatus <- results %>% tidyr::pivot_wider(
          id_cols = c(NAAM,LABNUMMER, RUNNR, HOEDNHD),
          names_from = c(TESTCODE, ELEMENTCODE),
          values_from = TESTSTATUS,
          names_sep = "<br>",
          names_sort = TRUE) %>% mutate(NAAM = NULL,
                                        LABNUMMER = NULL,
                                        HOEDNHD = NULL,
                                        RUNNR = NULL)
        
        test_widened_combined <- cbind(test_widened_results, test_widened_uitvallend, test_widened_teststatus)
        
        #here we count columns in a bunch of different ways so that we know the positions of columns to hide in the table that the user sees.
        extra_column_amount <- seq.int(0,(ncol(test_widened_uitvallend) + ncol(test_widened_teststatus)), by = 1)
        what_test_columns_are_there <- results %>% count(TESTCODE, ELEMENTCODE)
        number_of_test_columns <- nrow(what_test_columns_are_there)
        original_number_of_columns <- ncol(test_widened_results)
        original_and_uitvallend_columns <- ncol(test_widened_uitvallend) + original_number_of_columns
        total_columns <- ncol(test_widened_combined)
        
        labnr_column_index <- which(colnames(test_widened_results) == "LABNUMMER") 
        labnr_column_index <- (labnr_column_index[1] - 1) #which() returns vector,first element contains the actual index. subtract 1 from index because R counts indexes from 1, DT counts from 0
        
        description_column_index <- which(colnames(test_widened_results) == "NAAM")
        description_column_index <- (description_column_index[1] - 1)

        results_table_dataframe <<- test_widened_combined
        table_test <- table_builder(test_widened_combined,
                                    sort_by = labnr_column_index,
                                    selection = list(mode = "multiple", target = 'cell'),
                                    group = TRUE,
                                    group_cols = c(description_column_index, labnr_column_index),
                                    columnDefs = list(list(
                                      visible = FALSE, targets = c(description_column_index, -extra_column_amount))) #hide the columns we use for coloring
                                ) %>% DT::formatStyle(
                                  columns = 'LABNUMMER',
                                  valueColumns = 'LABNUMMER',
                                  backgroundColor = DT::styleEqual(
                                    selected_sample_current_results()$LABNUMMER,
                                    'yellow',
                                    default = 'gray'
                                  )
                                ) %>% DT::formatStyle(
                                  columns = 5:(4 + number_of_test_columns), #why start at 5 but only need to add 4 for the end? idk
                                  valueColumns = (1 + original_number_of_columns):original_and_uitvallend_columns,
                                  target = 'cell',
                                  backgroundColor = DT::styleEqual(TRUE, 'salmon')
                                ) %>% DT::formatStyle(
                                  columns = 5:(4 + number_of_test_columns), 
                                  valueColumns = (1 + original_and_uitvallend_columns):total_columns,
                                  target = 'cell',
                                  backgroundColor = DT::styleEqual(c(1000,300), c('lightgreen','gray'))
                                )
        return(table_test)
    }
  })
  
  results_table_dataframe <<- tibble()
  
  #This reactive switches the table view between only current results or to include historical results.
  historical_or_current_results <- reactive({
    if(input$instellingen_toon_historie_tabel  == FALSE){
      return(selected_sample_current_results())
    } else {
      return(selected_sample_historical_results())
    }
  })
  
  #Observer that triggers when user clicks the "wis selectie" button in the top left to clear the selected rows.
  observeEvent(input$tabel_testresultaten_wis_selectie,{
    DT::selectRows(sampleresults_proxy, selected = NULL)
  })
  
  
  observeEvent(input$tabel_sampleresults_cell_edit,{
    isolate({
      #complete_results(DT::editData(complete_results(),
      #                     input$tabel_sampleresults_cell_edit,
      #                     rownames = FALSE))
    })
  })
  
  #like selected_sample_rows but with logic for handling individual cells being selected
  selected_results <- reactive({
    if (isTruthy(input$tabel_sampleresults_rows_selected)) {
      return(results_table_dataframe[input$tabel_sampleresults_rows_selected,])
    }
    if(isTruthy(input$tabel_sampleresults_cells_selected)){
      row_id <- input$tabel_sampleresults_cells_selected[,1]
      column_id <- input$tabel_sampleresults_cells_selected[,2] +1 #need +1 because DT counts columns from 1 while R counts from 0
                     
      selected_cell <- results_table_dataframe[row_id,column_id ]
      return(selected_cell)
    }
    else{
      showModal(modalDialog(
        title = "Geen Resultaten",
        "Geen resultaten geselecteerd!",
        easyClose = TRUE
      ))
      return()
    }
  })
  
####################################fiatteer plot###############################
  #This generates the plot(s) showing test results. 
  output$fiatteer_grafiek <- renderPlot({
    req(selected_sample_historical_results())
    
    hide_ratio_graph_without_ratios()
    
    #Retrieve both current and historical results (despite historical_results already including current_results) because we want current results as its own variable so we can highlight them in the plot.
    historical_results <- selected_sample_historical_results() 
    current_results <- selected_sample_current_results()
    plottable_results <- historical_results %>% filter(!is.na(RESULTAAT_ASNUMERIC)) #remove NA results because geom_line creates breaks when encountering NA values.
    
    selected_samples <- plot_highlighted_samples()
    
    results_plot <- plottable_results %>% plot_builder(SAMPLINGDATE,
                                                       RESULTAAT_ASNUMERIC,
                                                       current_results, 
                                                       selected_samples, 
                                                       shape = UITVALLEND,
                                                       TESTCODE)
            
    #move hover_data to something that doesn't call the WHOLE PLOT AGAIN
    #plot <- plot + geom_text(data = hover_data(), aes(label=LABNUMMER))
    
    return(results_plot)
  })

  # fiatteer_plot_user_settings <-
  #   reactive({
  #     user_selection <- list(
  #       colour = input$grafiek_kleur_selectie,
  #       plot_choice = input$grafiek_keuze,
  #       wrap_choice = input$grafiek_wrap_keuze,
  #       wrap_category = input$grafiek_wrap_categorie_selectie
  #     )
  #     # toon alle factor levels bij de wrap categorie (nog niet werkzaam)
  #     #freezeReactiveValue(input, "grafiek_wrap_selectie")
  #     #updateCheckboxGroupInput(inputId = "grafiek_wrap_selectie",inline = TRUE,choices = input$grafiek_wrap_categorie_selectie,selected = input$grafiek_wrap_categorie_selectie)
  #     
  #     return(user_selection)
  #   })
  
  #Triggers when user hovers over a datapoint in a results plot.
  observeEvent(input$fiatteer_grafiek_zweef,{
    plot_hovered_samples(nearPoints(selected_sample_historical_results(),input$fiatteer_grafiek_zweef))
  })
  
  observeEvent(input$fiatteer_grafiek_klik, {
    #moved to double click because of a shiny issue with firing click events while making a brush selection
  })
  
  #This handles "box" selections where users hold and drag a selection across datapoints in a results plot.
  observeEvent(input$fiatteer_grafiek_gebied, {
    isolate({
      
      selected_test_results <- brushedPoints(selected_sample_historical_results(), 
                                            input$fiatteer_grafiek_gebied)
      
      associated_samples <-semi_join(selected_sample_historical_results(), 
                                     selected_test_results,
                                     by = 'LABNUMMER')
      plot_highlighted_samples(associated_samples)
      
      associated_ratios <- semi_join(selected_sample_historical_ratios(),
                                     selected_test_results, 
                                     by = 'LABNUMMER')
      plot_highlighted_ratios(associated_ratios) #ensures that ratios belonging to the same samples get highlighted too
      
    })

  })
  
  #Triggers when user double clicks in a results plot.
  observeEvent(input$fiatteer_grafiek_dblklik, {
    isolate({
      
      selected_test_result <- nearPoints(selected_sample_historical_results(),
                                         input$fiatteer_grafiek_dblklik)
      
      associated_sample <-semi_join(selected_sample_historical_results(), 
                                    selected_test_result, 
                                    by = 'LABNUMMER')
      plot_highlighted_samples(associated_sample)
      
      associated_ratios <- semi_join(selected_sample_historical_ratios(),
                                     selected_test_result, 
                                     by = 'LABNUMMER')
      plot_highlighted_ratios(associated_ratios) #ensures that ratios belonging to the same samples get highlighted too

    })
  })

################################ratios plot#####################################
  
#' Just checks if currently selected samples have any ratios associated with them. After all, it's entirely possible to have a combination of test results that we don't calculate ratios for.
#' @return Boolean that's TRUE if currently selected samples have valid ratios, FALSE if not. 
  has_ratios <- function() {
      nrow(selected_sample_historical_ratios()) != 0
    }
  
#' Hides the ratios plot object if there are no ratios to show.
#' This is needed because Shiny "reserves" space for each plot object even if that object is empty. Hiding the plot object this way clears up this space.
#' @return Nothing.
  hide_ratio_graph_without_ratios <- function(){
    shinyjs::toggle(id = "ratios_grafiek", condition = has_ratios())
  }
  
  #Gets the ratios belonging to the currently selected sample(s)
  selected_sample_current_ratios <-  reactive({
    selected_monsterpuntcode <- select(selected_sample_current_results(), LABNUMMER)
    selected_sample_current_ratios <- complete_ratios %>% filter(LABNUMMER %in% selected_monsterpuntcode$LABNUMMER)
    return(selected_sample_current_ratios)
  })
  
  #Gets the ratios belonging to all the historical results of the currently selected sample(s)
  selected_sample_historical_ratios <- reactive({
    selected_monsterpuntcode <- select(selected_sample_historical_results(), LABNUMMER)
    current_ratios <- complete_ratios %>% filter(LABNUMMER %in% selected_monsterpuntcode$LABNUMMER)
    return(current_ratios)
  })
  
  #Here we render the ratio plots. Note that the rendering only happens if there actually are ratios to show.
  output$ratios_grafiek <- renderPlot({
    historical_ratios <- selected_sample_historical_ratios()
    current_ratios <- selected_sample_current_ratios()
    selected_ratios <- plot_highlighted_ratios()
    req(has_ratios())

    ratios_plot <- historical_ratios %>% plot_builder(SAMPLINGDATE,
                                                       WAARDE,
                                                       current_ratios,
                                                       selected_ratios,
                                                       shape = NULL,
                                                       RATIO)
    return(ratios_plot)
    
  })
  
  observeEvent(input$ratios_grafiek_klik, {
    #moved to double click because of a shiny issue with firing click events while making a brush selection
  })
  
  #Triggers when user drags a "box" selection across datapoints in a ratios plot.
  observeEvent(input$ratios_grafiek_gebied, {
    isolate({
      
      selected_ratios <- brushedPoints(selected_sample_historical_ratios(),
                                       input$ratios_grafiek_gebied)
      
      associated_ratios <- semi_join(selected_sample_historical_ratios(),
                                     selected_ratios, 
                                     by = 'LABNUMMER')
      plot_highlighted_ratios(associated_ratios)
      
      associated_samples <- semi_join(selected_sample_historical_results(),
                                      selected_ratios,
                                      by = 'LABNUMMER')
      plot_highlighted_samples(associated_samples) #make sure to also highlight test results belonging to the same sample
    })
  })
  
  #Triggers when user makes a double click in a ratios plot.
  observeEvent(input$ratios_grafiek_dblklik, {
    isolate({
      
      selected_ratios <- nearPoints(selected_sample_historical_ratios(),
                                    input$ratios_grafiek_dblklik)
      
      associated_ratios <- semi_join(selected_sample_historical_ratios(),
                                     selected_ratios,
                                     by = 'LABNUMMER')
      plot_highlighted_ratios(associated_ratios)
      
      associated_sample <- semi_join(selected_sample_historical_results(),
                                     selected_ratios,
                                     by = 'LABNUMMER')
      plot_highlighted_samples(associated_sample) #make sure to also highlight test results belonging to the same sample
      
    })
  })
  
#############################plot table#########################################
  #This renders the datatable below the plots where we show details about the highlighted samples.
  output$fiatteer_grafiek_tabel <- DT::renderDataTable({
    req(plot_highlighted_samples())
    
    highlighted_samples <- plot_highlighted_samples() %>%
      select(
        NAAM,
        LABNUMMER,
        RUNNR,
        TESTCODE,
        ELEMENTCODE,
        RESULTAAT,
        REFMESSAGE,
        RESULT_OPMERKING,
        SAMPLINGDATE,
        MEASUREDATE,
        UITVALLEND,
        TESTSTATUS
      )
    table_builder(
      highlighted_samples,
      group = TRUE,
      group_cols = c(0,1), #change to 1,2 if comment column is back
      sort_by = 1,
      columnDefs = list(list(
        visible = FALSE , targets = c('NAAM','UITVALLEND','TESTSTATUS')
      ))
        ) %>% DT::formatStyle(columns = 'RESULTAAT',
                              valueColumns = 'UITVALLEND',
                              target = 'cell',
                              backgroundColor = DT::styleEqual(TRUE,'salmon')
        ) %>% DT::formatStyle(columns = 'RESULTAAT',
                              valueColumns = 'TESTSTATUS',
                              target = 'cell',
                              backgroundColor = DT::styleEqual(1000, 'lightgreen')
                              )
  })
  
###############################validation#######################################
#' Writes the identifiers (like SAMPLE_ID) and comments of given samples and results to a .csv file. 
#' This is intended to be used to export validated or rejected samples.
#'
#' @return Boolean. TRUE if export succeeded, FALSE if not.
  validation_exporter <- function(samples_to_export, results_to_export, export_path){
    
    username <- as.character(Sys.getenv("USERNAME"))
    time <- as.character(Sys.time())
    
    samples_to_export_columns <-
      samples_to_export %>% select(SAMPLE_OPMERKING,
                                   LABNUMMER,
                                   MONSTERPUNTCODE,
                                   NAAM,
                                   SAMPLE_ID)
    
    selected_results_export_columns <-
      results_to_export %>% select(SAMPLE_ID,
                                   TESTCODE,
                                   ELEMENTCODE,
                                   RESULT_OPMERKING,
                                   MEETPUNT_ID,
                                   SAMPLE_TEST_ID,
                                   SAMPLE_RESULT_ID)
    

    #is full_join excessive? seems to lead to duplicates? is left_join sufficient?
    export_data <- full_join(samples_to_export_columns,
                             selected_results_export_columns,
                             by = 'SAMPLE_ID') %>% 
                    distinct() %>%
                    tibble::add_column(ANALIST = username, TIJD = time)
    tryCatch({
      if (!file.exists(export_path)) { #first export that creates the file should also write the column names
        readr::write_csv2(export_data, export_path, col_names = TRUE) 
      }
      else {
        readr::write_csv2(export_data, export_path, append = TRUE)
      }
      return(TRUE)
      
    }, error = function(e){
      showModal(modalDialog(title = "Error bij wegschrijven",e)) #geef de error als een popup scherm zodat de gebruiker het ziet
      return(FALSE)
      })
  }
  
  #Triggers when user clicks the "valideer" button on the bottom. Retrieves the currently selected samples and their associated results, it then adds these to a global(!) dataframe variable and tries to export this dataframe as a .csv file.
  #We transfer the validated samples to a globally available dataframe so that if the export fails for whatever reason the validated samples don't get lost.
  #Also, if the next export attempt succeeds it will also write the samples that were present in the previously failed attempt.
  observeEvent(input$button_valideer, {
    selected_rows <- selected_sample_rows()
    selected_rows_results <-selected_sample_current_results()
    req(selected_sample_rows())
    
    validated_samples <<- validated_samples %>% rbind(selected_rows) %>% distinct()  #possibility for duplicates when export fails and then succeeds the next time so we filter those
    validated_results <<- validated_results %>% rbind(selected_rows_results) %>% distinct()
    
    export_succeeded <- validation_exporter(validated_samples,
                                            validated_results,
                                            validated_path)
    
    if(isTRUE(export_succeeded)){ #only do this if exporting was successful
      fiatteer_samples(anti_join(fiatteer_samples(),validated_samples, by = 'LABNUMMER')) #remove finished samples from view
      validated_samples <<- tibble()
      validated_results <<- tibble()
      showNotification("Gevalideerde samples zijn geëxporteerd")
    }
  })
  
  #Identical to the button_valideer observer except this writes the rejected results.
  observeEvent(input$button_duplo_aanvraag, {
    selected_rows <- selected_sample_rows()
    selected_results <- selected_results()
    req(selected_results())
    
    duplo_samples <<- duplo_samples %>% rbind(selected_rows) %>% distinct()
    duplo_results <<- duplo_results %>% rbind(selected_results) %>% distinct()
    
    export_succeeded <- validation_exporter(duplo_samples,
                                            duplo_results,
                                            duplo_path)
    if(isTRUE(export_succeeded)){ 
      fiatteer_samples(anti_join(fiatteer_samples(),duplo_samples, by = 'LABNUMMER')) 
      duplo_samples <<- tibble()
      duplo_results <<- tibble()
      showNotification("Duplo resultaten zijn geëxporteerd")
    }
    
  })
  
  #Identical to the button_valideer observer except this writes the cancelled results.
  observeEvent(input$button_cancel, {
    selected_rows <- selected_sample_rows()
    selected_results <- selected_results()
    req(selected_results())
    
    cancelled_samples <<- cancelled_samples %>% rbind(selected_rows) %>% distinct()
    cancelled_results <<- cancelled_results %>% rbind(selected_results) %>% distinct()
    
    export_succeeded <- validation_exporter(cancelled_samples,
                                            cancelled_results,
                                            cancelled_path)
    if(isTRUE(export_succeeded)){ 
      fiatteer_samples(anti_join(fiatteer_samples(),cancelled_samples, by = 'LABNUMMER')) 
      cancelled_samples <<- tibble()
      cancelled_results <<- tibble()
      showNotification("Cancelled resultaten zijn geëxporteerd")
    }
  })
  
############################common functions####################################  

#' A wrapper for DT::datatable() that has sensible defaults for creating datatables and hides the convoluted syntax of some arguments.
#' @param table_data Dataframe that we want to present as a datatable.
#' @param dom Activates and sets the position of various datatable elements like the export buttons.
#' @param sort_by Datatables can start out already sorted by a given column. Which (if any) column should this be?
#' @param sort_direction Sort descending (default) with 'desc' or ascending with 'asc'.
#' @param comment_col Indicates whether there is a comment column. If TRUE it assumes the first column is for comments and makes it editable.
#' @param group FALSE means the datatable has no grouped rows, TRUE enables row grouping.
#' @param group_cols If grouping is enabled, which columns should we group rows by?
#' @param columnDefs A versatile option for setting a broad range of different attributes for columns. We generally use it to hide columns from view.
#'
#' @return DataTable as returned by DT::datatable()
  table_builder <- function(table_data,
                            dom = 'Bltipr',
                            sort_by = NA,
                            sort_direction = 'desc',
                            selection = "multiple",
                            comment_col = FALSE,
                            group = FALSE,
                            group_cols = 0,
                            columnDefs = NULL) {
    
    if (!is.na(sort_by)) {
      order = list(list(sort_by, sort_direction))
    } else {
      order = list()
    }
    
    if (group == TRUE) {
      extensions = c("Buttons", "RowGroup")
      rowGroup = list(dataSrc = group_cols)
    } else {
      extensions = c("Buttons")
      rowGroup = NULL
    }
    
    if (comment_col == TRUE){
      editable = list(target = "cell", disable = list(columns = c(1:ncol(table_data))))
    } else {
      editable = FALSE
    }
    
    DT::datatable(
      data = table_data,
      rownames = rownames,
      extensions = extensions,
      filter = "top",
      editable = editable,
      selection = selection,
      escape = FALSE,
      options = list(
        dom = dom, #dom needed to remove search bar (redundant with column search)
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        order = order,
        #ordering= 0, 
        rowGroup = rowGroup,
        columnDefs = columnDefs
      ) 
    ) 
  }
  
  
#' Wrapper for ggplot() that creates plots with consistent settings such as theme, labels, font size etc.
#' @param data dataframe to use for creating the plots.
#' @param x name of column (passed as string) to be used as x-axis.
#' @param y name of column (passed as string) to be used as y-axis.
#' @param current_data dataframe with results belonging to current samples, these results will be highlighted in the plots.
#' @param clicked_data dataframe with results belonging to points selected by the user, these results will also be highlighted.
#' @param facets name of column (passed as string) to split up the data by
#' @param shape name of column (passed as string) to be used as shape categories.
#'
#' @return ggplot object as created by ggplot().
  plot_builder <- function(data, x, y, current_data, clicked_data, facets, shape){
    suppressWarnings({
  
        plot <- ggplot(data = data,
                             mapping = aes(x = {{x}}, y = {{y}}, colour = NAAM, group = MONSTERPUNTCODE, shape = {{shape}})) +
          
          geom_line(alpha = 0.7) +
          geom_point(size = 2.5, alpha = 0.5) +
          geom_point(data = current_data, size = 3.5) +
          
          scale_x_date(date_labels = "%d-%m-%y", breaks = scales::breaks_pretty(n=12)) +
          guides(size = "none", x = guide_axis(angle = 45)) +
          
          facet_wrap(vars({{facets}}), scales = 'free_y') + 
          theme(strip.text = element_text(size = 16)) 
        
        if (isTruthy(clicked_data)) #clicked data has to exist first
        {
          isolate({
            plot <- plot + geom_point(data = clicked_data, size = 2.7, alpha = 1)
          })
        }
      })
      return(plot)
  }
  
##############################bookmarking#######################################
  
  onBookmark(function(state){
    #state$values$username <- input$instellingen_gebruiker
  })
  onBookmarked(updateQueryString)
  
  onRestore(function(state){
    #updateSelectInput(inputId = "instellingen_gebruiker",selected = state$values$username)
  })
  
}

shinyApp(ui = ui,
         server = server,
         enableBookmarking = "server",
         options = list())
}
aquaApp()