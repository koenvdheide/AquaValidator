# library(shiny)

# library(shinyjs)
# library(tidyverse)
# library(ggplot2)
# library(leaflet)
# library(readxl)
# #library(askpass)
# library(DT)
# #library(reactlog)
# 
# library(dbplyr)
# library(odbc)

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
                 DT::dataTableOutput("tabel_fiatteerlijst")
               ),
             
               # tabPanel(
               #   value = "tab_tabel_samenvatting",
               #   title = "Tabel Samenvatting",
               #   DT::dataTableOutput("tabel_samenvatting")
               # ),
             
               tabPanel(
                 title = "Testresultaten",          #textOutput("tab_sample_titel") 
                 value = "tab_sample",
                 radioButtons(
                   "instellingen_roteer_tabel",
                   label = "Gebruik als kolom:",
                   choices = c("Labnummer" = "labnr",
                               "Tests" = "test",
                               "Sample Info" = "sample"),
                   inline = TRUE
                 ),
                  checkboxInput("instellingen_verberg_historie_tabel",
                                "Toon alleen huidig geselecteerde sample(s)",
                                value = TRUE),
                 DT::dataTableOutput("tabel_sampleresults")
               ),
             
               tabPanel(
                 value = "tab_fiatteer_grafiek",
                 title = "Grafiek",
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
  results_to_validate <- tibble()
  ratios <- tibble()
  
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
        tibble::add_column(#KLAAR = '<input type="checkbox" id="klaar" class="styled">', 
          SAMPLE_OPMERKING = "", .before = 1) %>% #don't move the comment column!
        arrange(PRIOFINISHDATE))
      
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
          UITVALLEND = TESTSTATUS != 300 & REFCONCLUSION == 0) %>%
          #see AAV-177 issue
          tibble::add_column(RESULT_OPMERKING = "", .before = 1)) #don't move the comment column!
      results_to_validate <<- semi_join(results(),samples(), by = c("LABNUMMER"))
      
      ratios <<- ratios_calculator(results())
      
    }, error = function(e){
      showModal(modalDialog(title = "Error",e)) #geef de error als een popup scherm zodat de gebruiker het ziet
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
          across(contains(c("datum", "date")), ~ as.Date(.x, tryFormats = c(
                                                            "%d-%m-%Y%t%t%T", 
                                                            "%Y-%m-%d%t%t%T", 
                                                            "%Y/%m/%d%t%t%T", 
                                                            "%d-%m-%Y", 
                                                            "%Y-%m-%d", 
                                                            "%Y/%m/%d"))
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
  
  standard_ratios <- list(
    BZV_ONOPA = c("BZV5","onopa", "ELEMENT/TEST"),
    CZV_BZV = c("CZV","BZV5", "ELEMENT/ELEMENT"),
    CZV_NKA = c("CZV","nka","ELEMENT/TEST"),
    CZV_TNB = c("CZV","tnb","ELEMENT/TEST"),
    CZV_TOC = c("CZV","TOC","ELEMENT/ELEMENT"),
    OFOS_TPA = c("ofos","tpa","TEST/TEST")
    
  )
  ratios_calculator <- function(results){
    
    calculated_ratios <-
      results %>%
      group_by(LABNUMMER, MONSTERPUNTCODE) %>%
      reframe(
        NAAM = NAAM,
        SAMPLINGDATE = SAMPLINGDATE,
        CZV_BZV_RATIO = ifelse(
          any(ELEMENTCODE == "CZV") & any(ELEMENTCODE == "BZV5"),
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
    
    # numerator <- NULL
    # 
    # denominator <- NULL
    # 
    #  ratio = ifelse(
    #    any(ELEMENTCODE == numerator) & any(ELEMENTCODE == denominator),
    #    RESULTAAT_ASNUMERIC[ELEMENTCODE == numerator] / RESULTAAT_ASNUMERIC[ELEMENTCODE == denominator],
    #    NA
    #  )
  }

############################fiatteer tab######################################## 
  output$tabel_fiatteerlijst <- DT::renderDataTable({
    req(input$input_file)
    
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
  
  observeEvent(input$tabel_fiatteerlijst_cell_edit,{
    #reminder that if "samples" columns change/rearrange this can overwrite the wrong columns!
    isolate({
    samples(DT::editData(samples(),
                         input$tabel_fiatteerlijst_cell_edit,
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
    matching_results <- semi_join(results(), selected_sample_current_results(),
                                  by = c('MONSTERPUNTCODE')) %>%
      arrange(desc(SAMPLINGDATE)) %>% #it SHOULD already put the most recent result first but this ensures it
      top_n_results(n = input$instellingen_hoeveelheid_resultaten)
    
    plot_selected_samples(rep(FALSE, nrow(matching_results))) #fill plot_selected_samples so it doesn't throw out of bounds errors later
    return(matching_results)
  })
  
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
        names_from = c(NAAM,LABNUMMER,RUNNR),
        values_from = RESULTAAT, 
        names_sep = "<br>",
        unused_fn = list(MEASUREDATE = list, SAMPLINGDATE = list, UITVALLEND = list))
      
      #%>% mutate()
      
      # labnr_widened_uitvallend <- results %>% tidyr::pivot_wider(
      #   id_cols = c(TESTCODE,ELEMENTCODE),
      #   names_from = c(NAAM,LABNUMMER,RUNNR),
      #   values_from = UITVALLEND, 
      #   names_sep = "<br>",
      #   unused_fn = list(MEASUREDATE = list, SAMPLINGDATE = list)
      #   )
      
      table_labnr <- table_builder(labnr_widened_results, sort_by = 0) #%>%
      #     formatStyle(
      #      columns = 'RESULTAAT',
      #      valueColumns = 'UITVALLEND',
      #      target = 'cell',
      #      backgroundColor = DT::styleEqual(TRUE, 'salmon')
      #      )
      #   
      return(table_labnr)
      
    } else if (input$instellingen_roteer_tabel == "sample") {
      table_sample <- table_builder(
        results,
        sort_by = 2,
        comment_col = TRUE,
        group = TRUE,
        group_cols = c(2,3), #change to 1,2 if comment column is removed
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
            "SOORTWATER"
          )
        ))
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
      )
      return(table_sample)
      
      #%>% formatSignif(columns = c(-2,-3), digits = 3) #nog kijken hoe we datums uitzonderen
      
    } else if (input$instellingen_roteer_tabel == "test") {
      test_widened_results <- results %>% 
        tidyr::pivot_wider(
          id_cols = c(NAAM,LABNUMMER, RUNNR),
          names_from = c(TESTCODE, ELEMENTCODE),
          values_from = RESULTAAT,
          names_sep = "<br>",
          unused_fn = list(MEASUREDATE = list, 
                           SAMPLINGDATE = list, 
                           UITVALLEND = list))
      
      table_test <-
        table_builder(
          test_widened_results,
          sort_by = 1,
          group = TRUE,
          group_cols = c(0, 1),
          #change to 1,2 if comment column is back
          columnDefs = list(list(
            visible = FALSE , targets = c(0)
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
          columns = 'RUNNR',
          valueColumns = 'UITVALLEND',
          target = 'cell',
          backgroundColor = DT::styleEqual(TRUE, 'red')
        )
      #%>% formatSignif(columns = c(-2,-3), digits = 3) #nog kijken hoe we datums uitzonderen
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
    
    historical_results <- selected_sample_historical_results() 
    current_results <- selected_sample_current_results()
    #plot_user_choices <- fiatteer_plot_user_settings()
    
    results_plot <- ggplot(data = historical_results,
                           mapping = aes(x = SAMPLINGDATE, y = RESULTAAT_ASNUMERIC, colour = NAAM, group = MONSTERPUNTCODE)) +
      
      geom_line(alpha = 0.7) +
      geom_point(size = 2.5, alpha = 0.5, aes(shape = UITVALLEND)) +
      geom_point(data = current_results, size = 3.5, aes(shape = UITVALLEND)) +
      
      labs(x = "Sampling Datum", y = "Resultaat") +
      scale_x_date(date_labels = "%x", breaks = scales::breaks_pretty(n = 12)) +
      guides(size = "none", x = guide_axis(angle = 45)) +
      
      facet_wrap(vars(TESTCODE), scales = 'free_y')
    
    if (isTruthy(plot_selected_samples())) #clicked data has to exist first
    {
      isolate({
        selected_results <- plot_selected_samples()
        results_plot <-
          results_plot + geom_point(data = selected_results, size = 3.5, aes(shape = UITVALLEND))
      })
    }
    
    
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
    
    ratios_plot <-ggplot(data = historical_ratios,
                         mapping = aes(x = SAMPLINGDATE, y = WAARDE, colour = NAAM, group = MONSTERPUNTCODE)) +
      
      geom_line(alpha = 0.7) +
      geom_point(size = 2.5, alpha = 0.5) +
      geom_point(data = current_ratios, size = 3.5) +
      
      labs(x = "Sampling datum", y = "Berekende waarde") +
      scale_x_date(date_labels = "%x") +
      guides(size = "none", x = guide_axis(angle = 45)) +
      
      facet_wrap(vars(RATIO), scales = 'free_y') #still need to check first that ratio's really exist
    
    if (isTruthy(plot_selected_ratios())) #clicked data has to exist first
    {
      isolate({
        selected_ratios <- plot_selected_ratios()
        ratios_plot <-
          ratios_plot + geom_point(data = selected_ratios,
                                   size = 3.5)
      })
    }
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
        SAMPLINGDATE,
        MEASUREDATE,
        UITVALLEND
      )
    table_builder(
      selected_data,
      group = TRUE,
      group_cols = c(0,1),
      sort_by = 1,
      #change to 1,2 if comment column is back
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
    selected_samples_export_columns <- selected_samples %>% select(SAMPLE_OPMERKING,
                                                                   SAMPLE_ID)
    selected_results_export_columns <- selected_results %>% select(SAMPLE_ID,
                                                                   #RESULT_OPMERKING,
                                                                   MEETPUNT_ID,
                                                                   SAMPLE_TEST_ID,
                                                                   SAMPLE_RESULT_ID)
    
    export_data <- full_join(selected_samples_export_columns,
                             selected_results_export_columns,
                             by = 'SAMPLE_ID')
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
    
    validation_exporter(validated_samples, validated_results , "F:/2-Ano/Alg/13_Fiatteren/Validator/gevalideerde_samples.csv")
    
    if(isTRUE(validation_exporter)){ #only do this if exporting was successful 
      validated_samples <<- tibble()
      validated_results <<- tibble()
      samples(anti_join(samples(),selected_rows, by = 'LABNUMMER')) #remove finished samples from view
    }
  })
  
  observeEvent(input$button_duplo_aanvraag, {
    selected_rows <- selected_sample()
    selected_result_rows <- selected_results()
    req(selected_results())
    
    #samples(anti_join(samples(),selected_rows, by = 'LABNUMMER')) #do we want to remove REJECTED samples from view?
    
    rejected_samples <<- rejected_samples %>% rbind(selected_rows)
    rejected_results <<- rejected_results %>% rbind(selected_result_rows)
    
    rejected_samples_export <- rejected_samples %>% select(SAMPLE_OPMERKING,
                                                           SAMPLE_ID
    )
    rejected_results_export <- rejected_results %>% select(#RESULT_OPMERKING,
                                                           SAMPLE_ID,
                                                           MEETPUNT_ID,
                                                           SAMPLE_TEST_ID,
                                                           SAMPLE_RESULT_ID
    )
    export_data <- full_join(rejected_samples_export,
                             rejected_results_export,
                             by = 'SAMPLE_ID')
    
    tryCatch({
      readr::write_csv2(export_data, "F:/2-Ano/Alg/13_Fiatteren/Validator/afgewezen_sample_resultaten.csv",append = TRUE)
      rejected_samples <<- tibble()
      rejected_results <<- tibble()
    }, error = function(e){
      showModal(modalDialog(title = "Error bij wegschrijven",e)) #geef de error als een popup scherm zodat de gebruiker het ziet
    })
    
    
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
  
  plot_builder <- function(){
    #common code for building results & ratios plots
  }
  
}

shinyApp(ui = ui,
         server = server,
         enableBookmarking = "server",
         options = list())
}
#pkgload::load_all() #this makes loading the project as a package freak out for some reason
aquaApp()