library(shiny)
library(shinyjs)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(readxl)
library(askpass)
library(DT)
library(reactlog)

library(dbplyr)
library(odbc)

reactlog_enable()
options(shiny.maxRequestSize=30*1024^2)

#UI input variables are intentionally in Dutch, makes it easier to keep them separate from output/internal variables on the server side
ui <- function(request) {
  tagList( 
  useShinyjs(),
  navbarPage(
    title= div(tags$img(src='Logo-Aqualysis-RGB-HR.png', align = 'left', width = "130px", height = "34px"),HTML('&nbsp;'),  "Validatie"),
  
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
                   
                   #ook (bijvoorbeeld) csv en tsv?
                   label = "Kies Excel samplelijst",
                   accept = c(".xlsx", ".xls")
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
                                "Toon alleen huidig geselecteerde sample(s)"),
                 DT::dataTableOutput("tabel_sample")
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
  header = bookmarkButton(
    label = "Sla fiatteer voortgang & opmerkingen op"
  )
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
  
  #data
  samples <- NULL
  results <- NULL
  ratios <- NULL
  
  #graph user input
  graph_selection <- reactiveVal()
  hover_selection <- reactiveVal()
  ratio_selection <- reactiveVal()
  
##################### common server functions #######################
  
  excel_results_reader <-
    function(filePath,
             sheet = NULL
             #resultcolumn,
             #labnummercolumn,
             #meetpuntcolumn
             ){
      
    excel_data <-
      read_excel(filePath, progress = TRUE, sheet = sheet) %>%
      
      #kan netter?: https://stackoverflow.com/questions/64189561/using-case-when-with-dplyr-across
      mutate(
        NIET_NUMBER = across(contains(c("result", "resultaat")), ~ if_else(is.na(as.numeric(.)), ., NA)),
        #across(contains(c("result", "resultaat")), as.list),
        across(contains(c("result", "resultaat")), as.numeric),
        
        #"{resultcolumn}" := as.numeric,
        #"{labnummercolumn}" := as.numeric,
        #"{meetpuntcolumn}" := as.factor,
        
        
        # this removes hour/minute/second from sampling&measurement dates for some reason even though %T should cover this, relying on readxl's inbuilt date recognition for now
        # default date recognition doesn't see MONSTERNAMEDATUM column as valid dates for some reason
        # try parse_date_time() instead?
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
        ), as.factor)
      )
    return (excel_data)
  }
  
  inputUpdater <-
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
        cbg = updateCheckboxGroupInput(
          inputId = inputId,
          choices = choices,
          selected = selected
        ),
        vs = updateVarSelectInput(
          inputId = inputId,
          data = data,
          selected = selected
        ),
        tp = updateTabsetPanel(inputId = inputId, selected = selected),
        np = updateNavbarPage(inputId = inputId, selected = selected)
      )
      
    }
  
  results_widened <- function (original_results) {
    original_results %>% pivot_wider(
      id_cols = c(NAAM,LABNUMMER, RUNNR),
      names_from = c(TESTCODE, ELEMENTCODE),
      values_from = RESULTAAT,
      names_sep = "<br>",
      unused_fn = list(MEASUREDATE = list, SAMPLINGDATE = list, UITVALLEND = list)
    )
  }
  
  results_selection <- function(){
    #historical results & current_result
  }
  
  top_n_results <- function(n, full_results) {
    top_results <-
      full_results %>% group_by(MONSTERPUNTCODE)  %>% group_modify(~ {
        .x %>% group_by(LABNUMMER) %>% filter(cur_group_id() >= n_groups(.) - n)
      }) %>% ungroup()
  }
  
  ratios_calculator <- function(results){
    #dataframe with labnummer and ratios per labnummer
    
    
  }
  plot_builder <- function(){
    #common code for building results & ratios plots
  }
###########################reactive functions##################################
  
  selected_sample <- reactive({
    req(samples)
    if (isTruthy(input$tabel_fiatteerlijst_rows_selected)) {
      return(samples[input$tabel_fiatteerlijst_rows_selected,])
    }
    else{
      showModal(modalDialog(
        title = "selectie_missing",
        "Kies eerst een sample!",
        easyClose = TRUE
      ))
      return()
    }
  })
  current_result <- reactive({
    req(selected_sample())
    selected_labnummer <- select(selected_sample(), LABNUMMER)
    #print(selected_labnummer)
    matching_result <- semi_join(results,
                                  selected_sample(),
                                  by = c('LABNUMMER')) %>% select(
                                    MONSTERPUNTCODE,
                                    NAAM,
                                    LABNUMMER,
                                    TESTCODE,
                                    ELEMENTCODE,
                                    TESTSTATUS,
                                    RESULTAAT,
                                    #NON_NUMERICAL_VALUE,
                                    RUNNR,
                                    REFMESSAGE,
                                    REFCONCLUSION,
                                    GEVALIDEERD,
                                    UITVALLEND,
                                    SAMPLINGDATE,
                                    MEASUREDATE,
                                    SOORTWATER
                                  )
  })
  
  historical_results <- reactive({
    #includes current result for now
    req(results)
    selected_meetpunt <- select(current_result(), MONSTERPUNTCODE)
    matching_results <- semi_join(results,
                                  current_result(),
                                  by = c('MONSTERPUNTCODE')) %>% select(
                                    MONSTERPUNTCODE,
                                    NAAM,
                                    LABNUMMER,
                                    TESTCODE,
                                    ELEMENTCODE,
                                    TESTSTATUS,
                                    RESULTAAT,
                                    #NON_NUMERICAL_VALUE,
                                    RUNNR,
                                    REFMESSAGE,
                                    REFCONCLUSION,
                                    GEVALIDEERD,
                                    UITVALLEND,
                                    SAMPLINGDATE,
                                    MEASUREDATE,
                                    SOORTWATER
                                  ) %>%
            arrange(desc(SAMPLINGDATE)) %>% #it SHOULD already put the most recent result first but this ensures it
            top_n_results(n = input$instellingen_hoeveelheid_resultaten)
    
    graph_selection(rep(FALSE, nrow(matching_results))) #fill graph_selection so it doesn't throw out of bounds errors later
    return(matching_results)
    
  })
  current_ratio <-  reactive({
    req(ratios)
    selected_monsterpuntcode <- select(current_result(), LABNUMMER)
    current_ratio <- ratios %>% filter(LABNUMMER %in% selected_monsterpuntcode$LABNUMMER)
    return(current_ratio)
  })
  
  historical_ratios <- reactive({
    req(ratios)
    selected_monsterpuntcode <- select(historical_results(), LABNUMMER)
    current_ratios <- ratios %>% filter(LABNUMMER %in% selected_monsterpuntcode$LABNUMMER)
    return(current_ratios)
  })

  # selected_results_widened <- reactive ({
  #   historical_results() %>% pivot_wider(
  #     id_cols = c(LABNUMMER, RUNNR),
  #     names_from = c(TESTCODE, ELEMENTCODE),
  #     values_from = RESULTAAT,
  #     names_sep = "<br>",
  #     unused_fn = list(MEASUREDATE = list, SAMPLINGDATE = min) #, CZV_BZV_Ratio = list, CZV_NKa_Ratio = list, BZV_onopa_Ratio = list)
  #   )
  # })
  # 
  
  fiatteer_plot_user_selection <-
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
  
###########################observers###################################
  
  observeEvent(input$input_file, {
    loadingtip <- showNotification("Laden...", duration = NULL, closeButton = FALSE)
    tryCatch({
      file_path <- input$input_file$datapath
      
      fiatteerblad <- input$input_file_fiatteer_blad
      resultatenblad <- input$input_file_resultaten_blad
      measurepointcolumn <- input$input_file_meetpunt_kolom
      resultscolumn <- input$input_file_testresultaat_kolom
      labnrcolumn <- input$input_file_labnummer_kolom
      

        
      samples <<- excel_results_reader(
        file_path,
        sheet = fiatteerblad
        #resultcolumn = resultscolumn,
        #labnummercolumn = labnrcolumn,
        #meetpuntcolumn = measurepointcolumn
        ) %>% 
        arrange(PRIOFINISHDATE)
      
      results <<-
        excel_results_reader(
          file_path,
          sheet = resultatenblad
          #resultcolumn = resultscolumn,
          #labnummercolumn = labnrcolumn,
          #meetpuntcolumn = measurepointcolumn
        ) %>% 
        mutate(GEVALIDEERD = TESTSTATUS == 300,
               UITVALLEND = TESTSTATUS != 300 & REFCONCLUSION == 0)
      ratios <<-
        results %>%
        #mutate(RESULTAAT = as.numeric(unnest)) %>%
        group_by(LABNUMMER, MONSTERPUNTCODE) %>%
        reframe(
          NAAM = NAAM,
          SAMPLINGDATE = SAMPLINGDATE,
          CZV_BZV_RATIO = ifelse(
            any(ELEMENTCODE == "CZV") & any(ELEMENTCODE == "BZV5"),
            RESULTAAT[ELEMENTCODE == "CZV"] / RESULTAAT[ELEMENTCODE == "BZV5"],
            NA
          ),

          CZV_NKA_RATIO = ifelse(
            any(ELEMENTCODE == "CZV") &
              any(TESTCODE == "nka"),
            RESULTAAT[ELEMENTCODE == "CZV"] / RESULTAAT[TESTCODE == "nka"],
            NA
          ),

          BZV_ONOPA_RATIO = ifelse(
            any(ELEMENTCODE == "BZV5") &
              any(TESTCODE == "onopa"),
            RESULTAAT[ELEMENTCODE == "BZV5"] / RESULTAAT[TESTCODE == "onopa"],
            NA
          ),
          CZV_TOC_RATIO = ifelse(
            any(ELEMENTCODE == "CZV") &
              any(ELEMENTCODE == "TOC"),
            RESULTAAT[ELEMENTCODE == "CZV"] / RESULTAAT[ELEMENTCODE == "TOC"],
            NA
          ),
          CZV_TNB_RATIO =ifelse(
            any(ELEMENTCODE == "CZV") &
              any(TESTCODE == "tnb"),
            RESULTAAT[ELEMENTCODE == "CZV"] / RESULTAAT[TESTCODE == "tnb"],
            NA
          )

        ) %>% pivot_longer(
          cols = c(CZV_BZV_RATIO, CZV_NKA_RATIO, BZV_ONOPA_RATIO,CZV_TOC_RATIO,CZV_TNB_RATIO),
          names_to = "RATIO",
          values_to = "WAARDE",
          values_drop_na = TRUE #needed so that ggplot's geom_line doesn't stop when it encounters an NA value while plotting the ratios
        )

    }, error = function(e){
      showModal(modalDialog(title = "Error",e)) #geef de error als een popup scherm zodat de gebruiker het ziet
    })
    on.exit(removeNotification(loadingtip), add = TRUE)
    on.exit(inputUpdater(uiComponent = "tp", inputId = "fiatteer_beeld",selected = "tab_fiatteerlijst"), add = TRUE)
  })
  
  observeEvent(input$tabel_sample_rows_selected,{
   #look at selected_sample() but keep in mind this table has widened results!
  })
  
  observeEvent(input$fiatteer_grafiek_zweef,{
    hover_selection(nearPoints(historical_results(),input$fiatteer_grafiek_zweef))
  })
  
  observeEvent(input$fiatteer_grafiek_klik, {
    #moved to double click because of a shiny issue with firing click events while making a brush selection
    
    # isolate({
    #   selected_test <- nearPoints(historical_results(),
    #                               input$fiatteer_grafiek_klik)
    #   selected_sample <-
    #     semi_join(historical_results(), selected_test, by = 'LABNUMMER')
    # 
    #   graph_selection(selected_sample)
    # })
  })
  
  observeEvent(input$fiatteer_grafiek_gebied, {
    #freezeReactiveValue(input, "ratios_grafiek_klik") #helpt niet
    isolate({
      selected_tests <-
        brushedPoints(historical_results(), input$fiatteer_grafiek_gebied)
      selected_samples <-
        semi_join(historical_results(), selected_tests, by = 'LABNUMMER')
      
      graph_selection(selected_samples)
      
      related_ratios <- semi_join(historical_ratios(),selected_tests, by = 'LABNUMMER')
      ratio_selection(related_ratios)
      #showModal(modalDialog(DT::dataTableOutput("fiatteer_grafiek_tabel")))
    })

  })
  
  observeEvent(input$fiatteer_grafiek_dblklik, {
    isolate({
      selected_test <- nearPoints(historical_results(),
                                  input$fiatteer_grafiek_dblklik)
      selected_sample <-
        semi_join(historical_results(), selected_test, by = 'LABNUMMER')
      
      graph_selection(selected_sample)
      
      related_ratios <- semi_join(historical_ratios(),selected_test, by = 'LABNUMMER')
      ratio_selection(related_ratios)
      #showModal(modalDialog(DT::dataTableOutput("fiatteer_grafiek_tabel")))

    })
  })
  
  observeEvent(input$ratios_grafiek_klik, {
    #moved to double click because of a shiny issue with firing click events while making a brush selection
    
    # isolate({
    #   selected_ratios <-
    #     nearPoints(historical_ratios(), input$ratios_grafiek_klik)
    #   related_ratios <- semi_join(historical_ratios(),selected_ratios, by = 'LABNUMMER')
    #   ratio_selection(related_ratios)
    #   
    #   selected_samples <-
    #     semi_join(historical_results(), selected_ratios, by = 'LABNUMMER')
    #   graph_selection(selected_samples)
    # })
  })
  
  observeEvent(input$ratios_grafiek_gebied, {
    isolate({
      selected_ratios <-
        brushedPoints(historical_ratios(), input$ratios_grafiek_gebied)
      related_ratios <- semi_join(historical_ratios(),selected_ratios, by = 'LABNUMMER')
      ratio_selection(related_ratios)
      
      selected_samples <- semi_join(historical_results(), selected_ratios, by = 'LABNUMMER')
      graph_selection(selected_samples)
      #showModal(modalDialog(DT::dataTableOutput("fiatteer_grafiek_tabel")))
    })
  })
  
  observeEvent(input$ratios_grafiek_dblklik, {
    isolate({
      selected_ratios <-
        nearPoints(historical_ratios(), input$ratios_grafiek_dblklik)
      related_ratios <- semi_join(historical_ratios(),selected_ratios, by = 'LABNUMMER')
      ratio_selection(related_ratios)
      
      selected_samples <-
        semi_join(historical_results(), selected_ratios, by = 'LABNUMMER')
      graph_selection(selected_samples)
      #showModal(modalDialog(DT::dataTableOutput("fiatteer_grafiek_tabel")))
    })
  })
  
#######################output functions############################  

   # updateTabsetPanel(inputId = "fiatteer_beeld",selected = "tab_sample")
  
  # output$tab_sample_titel <- renderText({
  #   if (!is.null(isolate(historical_results()))) {
  #     paste("Samples", "(", length(unique(historical_results()$LABNUMMER)), ")")
  #   }
  #   else{
  #     paste("Samples")
  #   }
  # })
  
  output$tabel_fiatteerlijst <- DT::renderDataTable({
    DT::datatable(
      data = samples,
      filter = "top",
      rownames = FALSE,
      extensions = c("Buttons"),
      options = list(
        searchHighlight = TRUE,
        dom = 'Bltipr', #dom needed to remove search bar (redundant with column search) 
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )    
    
  })
  
   output$tabel_sample <- DT::renderDataTable({
     
     if(input$instellingen_verberg_historie_tabel  == TRUE){
       results <- current_result()
     }else{
       results <- historical_results()
     }
    
      if(input$instellingen_roteer_tabel  == "labnr"){
        labnr_widened_results <- results %>% pivot_wider(
          id_cols = c(TESTCODE,ELEMENTCODE),
          names_from = c(NAAM,LABNUMMER,RUNNR),
          values_from = RESULTAAT,
          names_sep = "<br>"
          # unused_fn = list(MEASUREDATE = list, SAMPLINGDATE = list, UITVALLEND = list)
        )
        
        DT::datatable(
          data = labnr_widened_results,
          rownames = FALSE,
          extensions = c("Buttons", "RowGroup"),
          filter = "top",
          escape = FALSE,
          options = list(
            dom = 'Bltipr', #dom needed to remove search bar (redundant with column search)
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            order = list(list(0, 'desc'))
            #ordering= 0, 
            #rowGroup = list(
              #dataSrc = c(0)
              # startRender = JS(
              #   "function(rows, group) {",
              #   "return 'Sampling Datum:' +' ('+rows.count()+' rows)';",
              #   "}"
              # )
            ),
           # columnDefs = list(list(visible=FALSE , targets = c("MONSTERPUNTCODE","NAAM","TESTSTATUS","REFCONCLUSION","UITVALLEND","SOORTWATER")))
          #) 
        )
      }else if(input$instellingen_roteer_tabel == "sample"){
       DT::datatable(
         data = results,
         rownames = FALSE,
         extensions = c("Buttons", "RowGroup"),
         filter = "top",
         escape = FALSE,
         options = list(
           dom = 'Bltipr', #dom needed to remove search bar (redundant with column search)
           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
           order = list(list(2, 'desc')),
           #ordering= 0, 
           rowGroup = list(
             dataSrc = c(1,2)
             # startRender = JS(
             #   "function(rows, group) {",
             #   "return 'Sampling Datum:' +' ('+rows.count()+' rows)';",
             #   "}"
             # )
           ),
           columnDefs = list(list(visible=FALSE , targets = c("MONSTERPUNTCODE","NAAM","TESTSTATUS","REFCONCLUSION","UITVALLEND","SOORTWATER")))
         ) 
       )  %>% formatStyle(columns = 'LABNUMMER',
                          valueColumns = 'LABNUMMER',
                          backgroundColor = styleEqual(current_result()$LABNUMMER, 'yellow',default = 'gray')
       ) %>% formatStyle(columns = 'RESULTAAT',
                         valueColumns = 'UITVALLEND',
                         target = 'cell',
                         backgroundColor = styleEqual(TRUE,'salmon'))
       
       #%>% formatSignif(columns = c(-2,-3), digits = 3) #nog kijken hoe we datums uitzonderen
     } else if (input$instellingen_roteer_tabel == "test"){ 
     test_widened_results <- results_widened(results)
     DT::datatable(
       data = test_widened_results,
       rownames = FALSE,
       extensions = c("Buttons", "RowGroup"),
       filter = "top",
       escape = FALSE,
       options = list(
         dom = 'Bltipr', #dom needed to remove search bar (redundant with column search)
         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
         order = list(list(1, 'desc')),
         #ordering= 0, 
         rowGroup = list(
           dataSrc = c(0,1)
           # startRender = JS(
           #   "function(rows, group) {",
           #   "return 'Sampling Datum:' +' ('+rows.count()+' rows)';",
           #   "}"
           # )
         ),
         columnDefs = list(list(visible=FALSE , targets = c(0)))
       ) 
     )  %>% formatStyle(columns = 'LABNUMMER',
                        valueColumns = 'LABNUMMER',
                        backgroundColor = styleEqual(current_result()$LABNUMMER, 'yellow',default = 'gray')
                        ) %>% formatStyle(columns = 'RUNNR',
                                          valueColumns = 'UITVALLEND',
                                          target = 'cell',
                                          backgroundColor = styleEqual(TRUE,'red'))
     
     
     #%>% formatSignif(columns = c(-2,-3), digits = 3) #nog kijken hoe we datums uitzonderen
}
   })

   
  output$fiatteer_grafiek <- renderPlot({
    plot_data <- historical_results()
    current_data <- current_result()
    #plot_user_choices <- fiatteer_plot_user_selection()
    
    results_plot <- ggplot(data = plot_data,
                   mapping = aes(x = SAMPLINGDATE, y = RESULTAAT, colour = NAAM, group = MONSTERPUNTCODE)) +
      geom_line(alpha = 0.7) +
      geom_point(size = 2.5, alpha = 0.5, aes(shape = UITVALLEND)) +
      geom_point(data = current_data, size = 3.5, aes(shape = UITVALLEND)) +
      guides(size = "none") +
      facet_wrap(vars(TESTCODE), scales = 'free_y')
    
        #clicked data has to exist first
    if (isTruthy(graph_selection()))
    {
      isolate({
        selected_data <- graph_selection()
        results_plot <-
          results_plot + geom_point(data = selected_data,
                                    size = 3.5,
                                    aes(shape = UITVALLEND))
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
  
  output$ratios_grafiek <- renderPlot({
    plot_ratios <- historical_ratios()
    current_ratios <- current_ratio()
    
    ratios_plot <-
      ggplot(data = plot_ratios,
             mapping = aes(x = SAMPLINGDATE, y = WAARDE, colour = NAAM, group = MONSTERPUNTCODE)) +
      geom_line(alpha = 0.7) +
      geom_point(size = 2.5, alpha = 0.5) +
      geom_point(data = current_ratios, size = 3.5) +
      guides(size = "none") +
      facet_wrap(vars(RATIO), scales = 'free_y') #still need to check ratio's really exist
    
    #clicked data has to exist first
    if (isTruthy(ratio_selection()))
    {
      isolate({
        selected_ratios <- ratio_selection()
        ratios_plot <-
          ratios_plot + geom_point(data = selected_ratios,
                                   size = 3.5)
      })
    }
    return(ratios_plot)
  })
  
  
  output$fiatteer_grafiek_tabel <- DT::renderDataTable({
    req(graph_selection())
    selected_data <-
      graph_selection() %>% select(
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
    
    DT::datatable(
      data = selected_data,
      rownames = FALSE,
      extensions = ("RowGroup"),
      filter = "top",
      escape = FALSE,
      options = list(
        dom = 'ltipr',         #dom = 'tr',
        # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        order = list(list(1, 'desc')),
        rowGroup = list(dataSrc = c(0, 1)),
        columnDefs = list(list(
          visible = FALSE , targets = c('NAAM','UITVALLEND')
        ))
      )
    ) %>% formatStyle(columns = 'RESULTAAT',
                      valueColumns = 'UITVALLEND',
                      target = 'cell',
                      backgroundColor = styleEqual(TRUE,'salmon'))
  })
}

shinyApp(ui = ui,
         server = server,
         enableBookmarking = "server",
         options = list())  