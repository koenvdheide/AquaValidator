#reactlog_enable()
aquaApp <- function(...){
options(shiny.maxRequestSize=30*1024^2)

#UI input variables are intentionally in Dutch, makes it easier to keep them separate from output/internal variables on the server side
ui <- function(request) {
  
  tagList( 
  shinyjs::useShinyjs(),
  
  navbarPage(
    title= div(tags$img(src='Logo-Aqualysis-RGB-HR.png', align = 'left', width = "130px", height = "34px"),
               HTML('&nbsp;'),
               "Validatie"),
    
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
                 title = "Testresultaten",          #textOutput("tab_testresultaten_titel") 
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
                  checkboxInput("instellingen_verberg_historie_tabel",
                                "Toon meer monsters van geselecteerde meetpunt(en)",
                                value = TRUE),
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
                 textInput(
                   "instellingen_gebruiker",
                   label = "Gebruiker",
                   value = "",
                   placeholder = "Je naam (initialen)"
                 ),
                 numericInput(
                   "instellingen_hoeveelheid_resultaten",
                   "Hoeveel resultaten moeten er per monsterpunt worden getoond?",
                   value = 10,
                   min = 1
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
               
               tabPanel(
                 "Testresultaten",
                 # checkboxInput("instellingen_roteer_tabel",
                 #               "Toon resultaten als rijen i.p.v. kolommen")
               )
               
             )
           ))),
  
  tabPanel("Hulp",
           id = "hulp_tab",
           sidebarLayout(sidebarPanel(),
                         mainPanel())),
  
  footer = div(#bookmarkButton(label = "Sla fiatteer voortgang op"),
                actionButton("button_duplo_aanvraag", label = "Vraag duplo aan voor geselecteerde resultaten"),
                actionButton("button_valideer", label = "Valideer geselecteerde samples"))
)
)}
server <- function(input, output, session) {
##################### common server variables #######################  
  
  settings <- reactiveValues(
    username = NULL
  )
  
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
  
  #input 
  samples <- reactiveVal(tibble())
  results <- reactiveVal(tibble())
  ratios <- tibble()
  
  #proxies
  fiatteerlijst_proxy <- DT::dataTableProxy("tabel_fiatteerlijst")
  sampleresults_proxy <- DT::dataTableProxy("tabel_sampleresults")
  
  #graph user input
  plot_selected_samples <- reactiveVal()
  plot_hover_selected_samples <- reactiveVal()
  plot_selected_ratios <- reactiveVal()
  
  #output
  validated_samples <- tibble()
  validated_results <- tibble()
  
  rejected_samples <- tibble()
  rejected_results <- tibble()
  
#####################################loading file###############################
  observeEvent(input$input_file, {
    loadingtip <- showNotification("Laden...", duration = NULL, closeButton = FALSE)
    
    tryCatch({
      file_path <- input$input_file$datapath
      fiatteerblad <- input$input_file_fiatteer_blad
      resultatenblad <- input$input_file_resultaten_blad
      # measurepointcolumn <- input$input_file_meetpunt_kolom
      # resultscolumn <- input$input_file_testresultaat_kolom
      # labnrcolumn <- input$input_file_labnummer_kolom
      
      samples(excel_reader(
        file_path,
        sheet = fiatteerblad
        #resultcolumn = resultscolumn,
        #labnummercolumn = labnrcolumn,
        #meetpuntcolumn = measurepointcolumn
      ) %>% 
        #tibble::add_column(SAMPLE_OPMERKING = "", .before = 1) %>% #don't move the comment column without also changing editData!
        arrange(PRIOFINISHDATE))
    }, error = function(e){
      showModal(modalDialog(title = "Error bij fiatteerlijst inladen",e)) #geef de error als een popup scherm zodat de gebruiker het ziet
    })
    tryCatch({
      results(
        excel_reader(
          file_path,
          sheet = resultatenblad
          #resultcolumn = resultscolumn,
          #labnummercolumn = labnrcolumn,
          #meetpuntcolumn = measurepointcolumn
        ) %>% mutate(
          RESULTAAT_ASNUMERIC = if_else(TESTSTATUS != 1000, as.numeric(RESULTAAT), NA),
          GEVALIDEERD = TESTSTATUS == 300,
          UITVALLEND = TESTSTATUS != 300 & TESTSTATUS != 1000 & REFCONCLUSION == 0)) #%>%
          #see AAV-177 issue
          #tibble::add_column(RESULT_OPMERKING = "", .before = 1)) #don't move the comment column without also changing editData!
      
      ratios <<- make_ratios(results())
      
    }, error = function(e){
      showModal(modalDialog(title = "Error bij resultaten inladen",e)) #geef de error als een popup scherm zodat de gebruiker het ziet
    })

    on.exit(removeNotification(loadingtip), add = TRUE)
    on.exit(uiUpdater(uiComponent = "TabsetPanel", inputId = "fiatteer_beeld",selected = "tab_fiatteerlijst"), add = TRUE)
  })
  
  excel_reader <-
    function(filePath,
             sheet = NULL
             #resultcolumn,
             #labnummercolumn,
             #meetpuntcolumn
             ){
      
    excel_data <-
      readxl::read_excel(filePath, progress = TRUE, sheet = sheet) %>%
        mutate(
          # this removes hour/minute/second from sampling&measurement dates for some reason even though %T should cover this, relying on readxl's inbuilt date recognition for now
          # default date recognition doesn't see MONSTERNAMEDATUM column as valid dates for some reason
          # try parse_date_time() instead?
          across(contains(c("datum", "date")),
                 ~ as.Date(.x, tryFormats = c(
                                              "%d-%m-%Y%t%t%T", 
                                              "%Y-%m-%d%t%t%T", 
                                              "%Y/%m/%d%t%t%T", 
                                              "%d-%m-%Y", 
                                              "%Y-%m-%d", 
                                              "%Y/%m/%d")
                          )
                 ),
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
              "parameter",
              "soortwater"
            )
          ), as.factor)
        ) 
    return (excel_data)
    }
  
  ratios_list <- list(
    BZV_ONOPA = c("BZV5","onopa", "ELEMENTCODE", "TESTCODE"),
    CZV_BZV = c("CZV","BZV5", "ELEMENTCODE", "ELEMENTCODE"),
    CZV_NKA = c("CZV","nka","ELEMENTCODE", "TESTCODE"),
    CZV_TNB = c("CZV","tnb","ELEMENTCODE","TESTCODE"),
    CZV_TOC = c("CZV","TOC","ELEMENTCODE", "ELEMENTCODE"),
    OFOS_TPA = c("ofos","tpa","TESTCODE","TESTCODE")
  )
  ratio_calculator <- function(ratio_name){
    desired_ratio <- ratios_list[[ratio_name]]
    print(desired_ratio[[4]])
    ratio_row = ifelse(
      any({{desired_ratio[[3]]}} == desired_ratio[[1]]) & any({{desired_ratio[[4]]}} == desired_ratio[[2]]),
      RESULTAAT_ASNUMERIC[{{desired_ratio[[3]]}} == desired_ratio[[1]]] / RESULTAAT_ASNUMERIC[{{ desired_ratio[[4]] }} == desired_ratio[[2]]],
      NA
    )
    return(ratio_row)
  }

  make_ratios <- function(results){
    
    relevant_results <- results %>% filter(!is.na(RESULTAAT_ASNUMERIC))
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
          any(ELEMENTCODE == "CZV") &
            any(ELEMENTCODE == "BZV5"),
          RESULTAAT_ASNUMERIC[ELEMENTCODE == "CZV"] / RESULTAAT_ASNUMERIC[ELEMENTCODE == "BZV5"],
          NA
        ),
        CZV_NKA_RATIO = ifelse(
          any(ELEMENTCODE == "CZV") &
            any(TESTCODE == "nka"),
          RESULTAAT_ASNUMERIC[ELEMENTCODE == "CZV"] / RESULTAAT_ASNUMERIC[TESTCODE == "nka"],
          NA
        ),
        BZV_ONOPA_RATIO = ifelse(
          any(ELEMENTCODE == "BZV5") &
            any(TESTCODE == "onopa"),
          RESULTAAT_ASNUMERIC[ELEMENTCODE == "BZV5"] / RESULTAAT_ASNUMERIC[TESTCODE == "onopa"],
          NA
        ),
        CZV_TOC_RATIO = ifelse(
          any(ELEMENTCODE == "CZV") &
            any(ELEMENTCODE == "TOC"),
          RESULTAAT_ASNUMERIC[ELEMENTCODE == "CZV"] / RESULTAAT_ASNUMERIC[ELEMENTCODE == "TOC"],
          NA
        ),
        CZV_TNB_RATIO =ifelse(
          any(ELEMENTCODE == "CZV") &
            any(TESTCODE == "tnb"),
          RESULTAAT_ASNUMERIC[ELEMENTCODE == "CZV"] / RESULTAAT_ASNUMERIC[TESTCODE == "tnb"],
          NA
        )
      ) %>% tidyr::pivot_longer(
        cols = c(CZV_BZV_RATIO, CZV_NKA_RATIO, BZV_ONOPA_RATIO,CZV_TOC_RATIO,CZV_TNB_RATIO),
        names_to = "RATIO",
        values_to = "WAARDE",
        values_drop_na = TRUE #needed so that ggplot's geom_line doesn't stop when it encounters an NA value while plotting the ratios
      ) %>% distinct()
    
    return(calculated_ratios)
  }

############################fiatteer tab######################################## 
  output$tabel_fiatteerlijst <- DT::renderDataTable({
    req(input$input_file)
    results_to_validate <- semi_join(results(),samples(), by = c("LABNUMMER"))
    
    rejected_tests <-
      results_to_validate %>% 
      filter(UITVALLEND == TRUE) %>% 
      select(LABNUMMER, TESTCODE)
    
    fiatteer_data <- samples() %>%
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
                      #"NIET_NUMBER"
                    )
                  )
                  )
    )
  })
  observeEvent(input$tabel_fiatterlijst_wis_selectie,{
    DT::selectRows(fiatteerlijst_proxy, selected = NULL)
  })
  
  observeEvent(input$tabel_fiatteerlijst_cell_edit,{
    #reminder that if "samples" columns change in order this can overwrite the wrong columns!
    isolate({
    samples(DT::editData(samples(),
                         input$tabel_fiatteerlijst_cell_edit,
                         proxy = fiatteerlijst_proxy,
                         rownames = FALSE))
    })
    
  })
  
  selected_sample <- reactive({
    data <- samples() #redundant but needed because subsetting a reactiveval is buggy
    if (isTruthy(input$tabel_fiatteerlijst_rows_selected)) {
      return(data[input$tabel_fiatteerlijst_rows_selected,])
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
  
  selected_sample_current_results <- reactive({
    req(selected_sample())
    selected_labnummer <- select(selected_sample(), LABNUMMER)
    matching_result <- semi_join(results(),
                                 selected_sample(),
                                 by = c('LABNUMMER'))
    return(matching_result)
  })
  
  selected_sample_historical_results <- reactive({
    #includes current result for now
    selected_meetpunt <- select(selected_sample_current_results(), MONSTERPUNTCODE)
    matching_results <- semi_join(results(),
                                  selected_sample_current_results(),
                                  by = c('MONSTERPUNTCODE')) %>%
                        arrange(desc(SAMPLINGDATE)) %>% #it SHOULD already put the most recent result first but this ensures it
                        top_n_results(n = input$instellingen_hoeveelheid_resultaten)
    
    plot_selected_samples(rep(FALSE, nrow(matching_results))) #fill plot_selected_samples so it doesn't throw out of bounds errors later
    return(matching_results)
  })
  
  #nightmare code because we have to deal with three group layers, we want all the results (layer 1) of the most recent labnummers (layer 2) for each monsterpuntcode (layer 3)
  top_n_results <- function(n, full_results) {
    top_results <-
      full_results %>% 
      group_by(MONSTERPUNTCODE)  %>% 
      group_modify(~ {.x %>% group_by(LABNUMMER) %>% 
                      filter(cur_group_id() >= n_groups(.) - n)}) %>% 
      ungroup()
  }
  
#############################results tab########################################
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

        labnr_widened_uitvallend <- results %>% tidyr::pivot_wider(
          id_cols = c(TESTCODE,ELEMENTCODE),
          names_from = c(NAAM, LABNUMMER, HOEDNHD, RUNNR),
          values_from = UITVALLEND,
          names_sort = TRUE,
          names_sep = "<br>") %>% mutate(TESTCODE = NULL,
                                         ELEMENTCODE = NULL)
        uitvallend_column_sequence <- seq.int(1,ncol(labnr_widened_uitvallend), by = 1)
        
        labnr_widened_combined <- cbind(labnr_widened_results, labnr_widened_uitvallend)
  
        what_sample_columns_are_there <- results %>% count(NAAM, LABNUMMER, RUNNR)
        number_of_sample_columns <- nrow(what_sample_columns_are_there)
        original_number_of_columns <- ncol(labnr_widened_results)
        total_number_of_columns <- ncol(labnr_widened_combined)
        
        table_labnr <- table_builder(labnr_widened_combined,
                                     sort_by = 0,
                                     columnDefs = list(list(
                                       visible = FALSE,
                                       targets = -uitvallend_column_sequence #hide columns on the right coming from UITVALLEND
                                       )
                                       )
                                     ) %>%
          DT::formatStyle(
           columns = 3:(2 + number_of_sample_columns), #starts at 3 to offset for the two code columns, why do we have to add 2 instead of 3 to offset at the end? NO IDEA
           valueColumns = (1 + original_number_of_columns):total_number_of_columns,
           target = 'cell',
           backgroundColor = DT::styleEqual(TRUE, 'salmon')
           )
       return(table_labnr)
      
    } else if (input$instellingen_roteer_tabel == "result_info") {
      labnr_column_index <- which(colnames(results) == "LABNUMMER")
      labnr_column_index <- (labnr_column_index[1] - 1) #which() returns vector,first element contains the actual index. subtract 1 from index because R counts indexes from 1, DT counts from 0
      
      description_column_index <- which(colnames(results) == "NAAM")
      description_column_index <- (description_column_index[1] - 1)
      
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
            "PROJECTCODE",
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
            "SOORTWATER"
          )
          )
        )
      )  %>% DT::formatStyle(
        columns = 'LABNUMMER',
        valueColumns = 'LABNUMMER',
        backgroundColor = DT::styleEqual(selected_sample_current_results()$LABNUMMER, 'yellow', 
                                         default = 'gray'
        )
      ) %>% DT::formatStyle(
        columns = 'RESULTAAT',
        valueColumns = 'UITVALLEND',
        target = 'cell',
        backgroundColor = DT::styleEqual(TRUE, 'salmon')
      ) %>% DT::formatStyle(
        columns = 'RESULTAAT',
        valueColumns = 'TESTSTATUS',
        target = 'cell',
        backgroundColor = DT::styleEqual(1000, 'darkgreen')
      )
      return(table_sample)
      
      #%>% formatSignif(columns = c(-2,-3), digits = 3) #nog kijken hoe we datums uitzonderen
      
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
        uitvallend_column_sequence <- seq.int(0,ncol(test_widened_uitvallend), by = 1)
        
        test_widened_combined <- cbind(test_widened_results, test_widened_uitvallend)
        
        what_test_columns_are_there <- results %>% count(TESTCODE, ELEMENTCODE)
        number_of_test_columns <- nrow(what_test_columns_are_there)
        original_number_of_columns <- ncol(test_widened_results)
        total_number_of_columns <- ncol(test_widened_combined)
        
        labnr_column_index <- which(colnames(test_widened_results) == "LABNUMMER") 
        labnr_column_index <- (labnr_column_index[1] - 1) #which() returns vector,first element contains the actual index. subtract 1 from index because R counts indexes from 1, DT counts from 0
        
        description_column_index <- which(colnames(test_widened_results) == "NAAM")
        description_column_index <- (description_column_index[1] - 1)

        table_test <- table_builder(
                                test_widened_combined,
                                sort_by = labnr_column_index,
                                group = TRUE,
                                group_cols = c(description_column_index, labnr_column_index),
                                columnDefs = list(list(
                                  visible = FALSE, targets = c(description_column_index, -uitvallend_column_sequence) #hide columns on the right coming from UITVALLEND
                                ))
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
                                  valueColumns = (1 + original_number_of_columns):total_number_of_columns,
                                  target = 'cell',
                                  backgroundColor = DT::styleEqual(TRUE, 'salmon')
                                  )
        return(table_test)
    }
  })
  
  historical_or_current_results <- reactive({
    if(input$instellingen_verberg_historie_tabel  == TRUE){
      return(selected_sample_current_results())
    } else {
      return(selected_sample_historical_results())
    }
  })
  
  observeEvent(input$tabel_testresultaten_wis_selectie,{
    DT::selectRows(sampleresults_proxy, selected = NULL)
  })
  
  observeEvent(input$tabel_sampleresults_cell_edit,{
    isolate({
      #doesn't work with results because that is not actually the dataframe used in sampleresults!
      #doesn't work with historical_or_current_results because that is a function (technically) so editData tries to edit a FUNCTION
      #results(DT::editData(results(),
      #                     input$tabel_sampleresults_cell_edit,
      #                     rownames = FALSE))
    })
  })
  
  selected_results <- reactive({
    data <- historical_or_current_results() #redundant but needed because subsetting a reactiveval is buggy
    if (isTruthy(input$tabel_sampleresults_rows_selected)) {
      return(data[input$tabel_sampleresults_rows_selected,])
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
  output$fiatteer_grafiek <- renderPlot({
    req(selected_sample_historical_results())
    hide_ratio_graph_without_ratios()
    
    historical_results <- selected_sample_historical_results() 
    current_results <- selected_sample_current_results()
    selected_results <- plot_selected_samples()
    #plot_user_choices <- fiatteer_plot_user_settings()
    
    plottable_results <- historical_results %>% filter(!is.na(RESULTAAT_ASNUMERIC))
    
    results_plot <- plottable_results %>% plot_builder(
                                                       SAMPLINGDATE,
                                                       RESULTAAT_ASNUMERIC,
                                                       current_results, 
                                                       selected_results, 
                                                       shape = UITVALLEND,
                                                       TESTCODE)
            
    #move hover_data to something that doesn't call the WHOLE PLOT AGAIN
    #plot <- plot + geom_text(data = hover_data(), aes(label=LABNUMMER))
    
    # if (plot_user_choices$fiatteer_wrap_choice == TRUE)
    # {
    #   plot <-
    #     plot + facet_wrap(plot_user_choices$wrap_category, scales = 'free_y')
    # }
    #
    return(results_plot)
  })

  fiatteer_plot_user_settings <-
    reactive({
      user_selection <- list(
        colour = input$grafiek_kleur_selectie,
        plot_choice = input$grafiek_keuze,
        wrap_choice = input$grafiek_wrap_keuze,
        wrap_category = input$grafiek_wrap_categorie_selectie
      )
      # toon alle factor levels bij de wrap categorie (nog niet werkzaam)
      #freezeReactiveValue(input, "grafiek_wrap_selectie")
      #updateCheckboxGroupInput(inputId = "grafiek_wrap_selectie",inline = TRUE,choices = input$grafiek_wrap_categorie_selectie,selected = input$grafiek_wrap_categorie_selectie)
      
      return(user_selection)
    })
  
  observeEvent(input$fiatteer_grafiek_zweef,{
    plot_hover_selected_samples(nearPoints(selected_sample_historical_results(),input$fiatteer_grafiek_zweef))
  })
  
  observeEvent(input$fiatteer_grafiek_klik, {
    #moved to double click because of a shiny issue with firing click events while making a brush selection
  })
  
  observeEvent(input$fiatteer_grafiek_gebied, {
    isolate({
      
      selected_test_results <- brushedPoints(selected_sample_historical_results(), 
                                            input$fiatteer_grafiek_gebied)
      
      associated_samples <-semi_join(selected_sample_historical_results(), 
                                     selected_test_results,
                                     by = 'LABNUMMER')
      plot_selected_samples(associated_samples)
      
      associated_ratios <- semi_join(selected_sample_historical_ratios(),
                                     selected_test_results, 
                                     by = 'LABNUMMER')
      plot_selected_ratios(associated_ratios)
      
    })

  })
  
  observeEvent(input$fiatteer_grafiek_dblklik, {
    isolate({
      
      selected_test_result <- nearPoints(selected_sample_historical_results(),
                                         input$fiatteer_grafiek_dblklik)
      
      associated_sample <-semi_join(selected_sample_historical_results(), 
                                    selected_test_result, 
                                    by = 'LABNUMMER')
      plot_selected_samples(associated_sample)
      
      associated_ratios <- semi_join(selected_sample_historical_ratios(),
                                     selected_test_result, 
                                     by = 'LABNUMMER')
      plot_selected_ratios(associated_ratios)
      #showModal(modalDialog(DT::dataTableOutput("fiatteer_grafiek_tabel")))

    })
  })

################################ratios plot#####################################
  has_ratios <- function() {
      nrow(selected_sample_historical_ratios()) != 0
    }
  
  hide_ratio_graph_without_ratios <- function(){
    shinyjs::toggle(id = "ratios_grafiek", condition = has_ratios())
  }
  
  selected_sample_current_ratios <-  reactive({
    selected_monsterpuntcode <- select(selected_sample_current_results(), LABNUMMER)
    selected_sample_current_ratios <- ratios %>% filter(LABNUMMER %in% selected_monsterpuntcode$LABNUMMER)
    return(selected_sample_current_ratios)
  })
  
  selected_sample_historical_ratios <- reactive({
    selected_monsterpuntcode <- select(selected_sample_historical_results(), LABNUMMER)
    current_ratios <- ratios %>% filter(LABNUMMER %in% selected_monsterpuntcode$LABNUMMER)
    return(current_ratios)
  })
  
  output$ratios_grafiek <- renderPlot({
    historical_ratios <- selected_sample_historical_ratios()
    current_ratios <- selected_sample_current_ratios()
    clicked_ratios <- plot_selected_ratios()
    req(has_ratios())

    ratios_plot <-
      historical_ratios %>% plot_builder(SAMPLINGDATE,
                                         WAARDE,
                                         current_ratios,
                                         clicked_ratios,
                                         shape = NULL,
                                         RATIO)
    return(ratios_plot)
    
  })
  
  observeEvent(input$ratios_grafiek_klik, {
    #moved to double click because of a shiny issue with firing click events while making a brush selection
  })
  
  observeEvent(input$ratios_grafiek_gebied, {
    isolate({
      
      selected_ratios <- brushedPoints(selected_sample_historical_ratios(),
                                       input$ratios_grafiek_gebied)
      
      associated_ratios <- semi_join(selected_sample_historical_ratios(),
                                     selected_ratios, 
                                     by = 'LABNUMMER')
      plot_selected_ratios(associated_ratios)
      
      associated_samples <- semi_join(selected_sample_historical_results(),
                                      selected_ratios,
                                      by = 'LABNUMMER')
      plot_selected_samples(associated_samples)
      #showModal(modalDialog(DT::dataTableOutput("fiatteer_grafiek_tabel")))
    })
  })
  
  observeEvent(input$ratios_grafiek_dblklik, {
    isolate({
      
      selected_ratios <- nearPoints(selected_sample_historical_ratios(),
                                    input$ratios_grafiek_dblklik)
      
      associated_ratios <- semi_join(selected_sample_historical_ratios(),
                                     selected_ratios,
                                     by = 'LABNUMMER')
      plot_selected_ratios(associated_ratios)
      
      associated_sample <- semi_join(selected_sample_historical_results(),
                                     selected_ratios,
                                     by = 'LABNUMMER')
      plot_selected_samples(associated_sample)
      
    })
  })
  
#############################plot table#########################################
  output$fiatteer_grafiek_tabel <- DT::renderDataTable({
    req(plot_selected_samples())
    
    selected_data <- plot_selected_samples() %>% 
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
        UITVALLEND
      )
    table_builder(
      selected_data,
      group = TRUE,
      group_cols = c(0,1), #change to 1,2 if comment column is back
      sort_by = 1,
      columnDefs = list(list(
        visible = FALSE , targets = c('NAAM','UITVALLEND')
      ))
    ) %>% DT::formatStyle(columns = 'RESULTAAT',
                          valueColumns = 'UITVALLEND',
                          target = 'cell',
                          backgroundColor = DT::styleEqual(TRUE,'salmon'))
  })
  
###############################validation#######################################
  validation_exporter <- function(selected_samples, selected_results, export_path){
    selected_samples_export_columns <- selected_samples %>% select(#SAMPLE_OPMERKING,
                                                                   SAMPLE_ID)
    selected_results_export_columns <- selected_results %>% select(SAMPLE_ID,
                                                                   #RESULT_OPMERKING,
                                                                   MEETPUNT_ID,
                                                                   SAMPLE_TEST_ID,
                                                                   SAMPLE_RESULT_ID)
    
    export_data <- full_join(selected_samples_export_columns,
                             selected_results_export_columns,
                             by = 'SAMPLE_ID') #is full_join excessive? seems to lead to duplicates?
    tryCatch({
      readr::write_csv2(export_data, export_path, append = TRUE)
      return(TRUE)
      
    }, error = function(e){
      showModal(modalDialog(title = "Error bij wegschrijven",e)) #geef de error als een popup scherm zodat de gebruiker het ziet
      return(FALSE)
      })
  }
  
  observeEvent(input$button_valideer, {
    selected_rows <- selected_sample()
    selected_rows_results <-selected_sample_current_results()
    req(selected_sample())
    
    validated_samples <<- validated_samples %>% rbind(selected_rows)
    validated_results <<- validated_results %>% rbind(selected_rows_results)
    
    export_succeeded <- validation_exporter(validated_samples,
                                            validated_results,
                                            "F:/2-Ano/Alg/13_Fiatteren/Validator/gevalideerde_samples.csv")
    
    if(isTRUE(export_succeeded)){ #only do this if exporting was successful 
      validated_samples <<- tibble()
      validated_results <<- tibble()
      samples(anti_join(samples(),selected_rows, by = 'LABNUMMER')) #remove finished samples from view
      showModal(modalDialog(title = "Validatie Gelukt", "Gevalideerde samples zijn geëxporteerd" ))
    }
  })
  
  observeEvent(input$button_duplo_aanvraag, {
    selected_rows <- selected_sample()
    selected_result_rows <- selected_results()
    req(selected_results())
    
    rejected_samples <<- rejected_samples %>% rbind(selected_rows)
    rejected_results <<- rejected_results %>% rbind(selected_result_rows)
    
    export_succeeded <- validation_exporter(rejected_samples,
                                            rejected_results,
                                            "F:/2-Ano/Alg/13_Fiatteren/Validator/afgewezen_sample_resultaten.csv")
    if(isTRUE(export_succeeded)){ 
      rejected_samples <<- tibble()
      rejected_results <<- tibble()
      #samples(anti_join(samples(),selected_rows, by = 'LABNUMMER')) #do we want to remove REJECTED samples from view?
      showModal(modalDialog(title = "Duplo Opdracht Gelukt", "Afgewezen resultaten zijn geëxporteerd"))
    }
    
  })
  
############################common functions####################################  
  uiUpdater <-
    function(uiComponent,
             inputId,
             label = NULL,
             choices = NULL,
             data = NULL,
             selected = NULL,
             choiceNames = NULL,
             choiceValues = NULL) {
      freezeReactiveValue(input, inputId)
      
      switch(
        uiComponent,
        #add dumb trick to add "none"/"geen" as first choice for optional inputs?
        CheckboxGroup = updateCheckboxGroupInput(
          inputId = inputId,
          choices = choices,
          selected = selected
        ),
        
        VarSelect = updateVarSelectInput(
          inputId = inputId,
          data = data,
          selected = selected
        ),
        
        TabsetPanel = updateTabsetPanel(inputId = inputId, selected = selected),
        
        NavbarPage = updateNavbarPage(inputId = inputId, selected = selected)
      )
      
    }
  
  table_builder <- function(table_data,
                            rownames = FALSE,
                            dom = 'Bltipr',
                            sort_by = NA,
                            sort_direction = 'desc',
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
  
  plot_builder <- function(data, x, y, current_data, clicked_data, facets, shape){

      plot <- ggplot(data = data,
                           mapping = aes(x = {{x}}, y = {{y}}, colour = NAAM, group = MONSTERPUNTCODE, shape = {{shape}})) +
        
        geom_line(alpha = 0.7) +
        geom_point(size = 2.5, alpha = 0.5) +
        geom_point(data = current_data, size = 3.5) +
        
        scale_x_date(date_labels = "%d-%m-%y", breaks = scales::breaks_pretty(n=12)) +
        guides(size = "none", x = guide_axis(angle = 45)) +
        
        facet_wrap(vars({{facets}}), scales = 'free_y') #still need to check first that ratio's really exist
      
      if (isTruthy(clicked_data)) #clicked data has to exist first
      {
        isolate({
          plot <- plot + geom_point(data = clicked_data, size = 3.5)
        })
      }
      
      return(plot)
  }
}

shinyApp(ui = ui,
         server = server,
         enableBookmarking = "server",
         options = list())
}
#pkgload::load_all() #this makes loading the project as a package freak out for some reason, moved to startup.R
aquaApp()