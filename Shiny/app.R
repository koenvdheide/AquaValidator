library(shiny)
library(shinyjs)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(readxl)
library(DT)
library(reactlog)

library(dbplyr)
library(odbc)

reactlog_enable()

#UI input variables are intentionally in Dutch, makes it easier to keep them separate from output/internal variables on the server side
ui <- tagList( 
  useShinyjs(),
  navbarPage(
  "Aqualysis Validatie",
  tabPanel("Fiatteren",
           
           fluidPage(
             tabsetPanel(
               id = "fiatteer_beeld",
               tabPanel(
                 title = "Bestand",
                 value = "tab_bestand",
                 br(),
                 fileInput(
                   "fiatteer_input_file",
                   
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
                 title = "Samples",          #textOutput("tab_sample_titel") 
                 value = "tab_sample",
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
             numericInput("instellingen_hoeveelheid_resultaten",
                          "Hoeveel resultaten moeten er per monsterpunt worden getoond?",
                          value = 10,
                          min = 1)
             
           ))),
  tabPanel("Hulp",
           id = "hulp_tab",
           sidebarLayout(sidebarPanel(),
                         mainPanel()))
)
)
server <- function(input, output, session) {
##################### common server variables #######################  
  
  #default groups (can be overridden)
  #data_groups <- reactiveValues(data_groups = c("MEETPUNT", "LABNR", "TESTCODE", "ELEMENTCODE"))
  
  #default settings
  #settings <- reactiveValues(settings = c(""))
  
  #database connection
  sql_connection_string <-
    reactiveVal("Driver=Oracle in OraClient19Home1;Host=db01-dcz-olin;Port=1521;")
  
  #data
  samples <- NULL
  results <- NULL
  ratios <- NULL
  
  #graph user input
  graph_selection <- reactiveVal()
  hover_selection <- reactiveVal()
  
##################### common server functions #######################
  
  excel_results_reader <- function(filePath, sheet = NULL) {
    
    excel_data <-
      read_excel(filePath, progress = TRUE, sheet = sheet) %>%
      
      #kan netter?: https://stackoverflow.com/questions/64189561/using-case-when-with-dplyr-across
      mutate(
        #causes strange grouping results in datatables 
        #outputIsText = across(contains("result"), ~ if_else(is.na(as.numeric(.)), TRUE, FALSE)),
        
        # andere mogelijkheid om eerst niet numerieke data etc. uit te filteren dan opnieuw converten via type_convert()?
        across(contains(c("result", "resultaat")) , as.numeric),
        
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
      unused_fn = list(MEASUREDATE = list, SAMPLINGDATE = list)
    )
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
###########################reactive functions##################################
  
  selected_sample <- reactive({
    req(samples)
    if (!is.null(input$tabel_fiatteerlijst_rows_selected)) {
      return(samples[input$tabel_fiatteerlijst_rows_selected,])
    }
    else{
      return(samples)
    }
  })
  current_result <- reactive({
    req(results)
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
  
  observeEvent(input$fiatteer_input_file, {
    loadingtip <- showNotification("Laden...", duration = NULL, closeButton = FALSE)
    tryCatch({
      loadedsamples <-
        excel_results_reader(input$fiatteer_input_file$datapath, sheet = "fiatteerlijst")
      samples <<- loadedsamples %>% arrange(PRIOFINISHDATE)
      
      results <<-
        excel_results_reader(input$fiatteer_input_file$datapath, sheet = "resultaten") %>%
        mutate(GEVALIDEERD = TESTSTATUS == 300,
               UITVALLEND = TESTSTATUS != 300 & REFCONCLUSION == 0)
      
      ratios <<-
        results %>%
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
          )
          
        ) %>% pivot_longer(
          cols = c(CZV_BZV_RATIO, CZV_NKA_RATIO, BZV_ONOPA_RATIO),
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
  
  observeEvent(input$fiatteer_grafiek_zweef,{
    hover_selection(nearPoints(historical_results(),input$fiatteer_grafiek_zweef))
  })
  
  observeEvent(input$fiatteer_grafiek_klik, {
    selected_test <- nearPoints(historical_results(),
                      input$fiatteer_grafiek_klik)
    selected_sample <- semi_join(historical_results(), selected_test, by = 'LABNUMMER')
    
    graph_selection(selected_sample)
  })
  
  observeEvent(input$fiatteer_grafiek_gebied, {
    selected_tests <- brushedPoints(historical_results(), input$fiatteer_grafiek_gebied)
    selected_samples <- semi_join(historical_results(), selected_tests, by = 'LABNUMMER')
    
    graph_selection(selected_samples)
  })
  
  # observeEvent(input$fiatteer_grafiek_dblklik, {
  #   graph_selection(NULL)
  # })
  
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
     #test_results <- top_n_results(input$instellingen_hoeveelheid_resultaten, historical_results())
     widened_results <- results_widened(historical_results())
     DT::datatable(
       data = widened_results,
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
           dataSrc = c(0)
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
                        backgroundColor = styleEqual(current_result()$LABNUMMER, 'red',default = 'gray')
                        ) %>% formatSignif(columns = ~ LABNUMMER + RUNNR + MEASUREDATE + SAMPLINGDATE, digits = 3)

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
    
    selected_data <- graph_selection() 
    #clicked data has to exist first
    if (isTruthy(selected_data))
    {
      results_plot <-
        results_plot + geom_point(data = selected_data,
                                  size = 3.5,
                                  aes(shape = UITVALLEND))
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
    

    
    #input$ratios_grafiek_dblklik
    #input$ratios_grafiek_zweef
    
    ratios_plot <-
      ggplot(data = plot_ratios,
             mapping = aes(x = SAMPLINGDATE, y = WAARDE, colour = NAAM, group = MONSTERPUNTCODE)) +
      geom_line(alpha = 0.7) +
      geom_point(size = 2.5, alpha = 0.5) +
      geom_point(data = current_ratios, size = 3.5) +
      guides(size = "none") +
      facet_wrap(vars(RATIO), scales = 'free_y')
    
    #clicked data has to exist first
    if (isTruthy(input$ratios_grafiek_klik)) {
      selected_ratios <-
        nearPoints(historical_ratios(), input$ratios_grafiek_klik)
      
      ratios_plot <-
        ratios_plot + geom_point(data = selected_ratios,
                                 size = 3.5)
      
      selected_samples <- semi_join(historical_results(), selected_ratios, by = 'LABNUMMER')
      graph_selection(selected_samples)
    } else if (isTruthy(input$ratios_grafiek_gebied)) {
      #klik heeft priority over area selection, is dit gewenst?
      selected_ratios <-
        brushedPoints(historical_ratios(), input$ratios_grafiek_gebied)
      
      ratios_plot <-
        ratios_plot + geom_point(data = selected_ratios,
                                 size = 3.5)
      
      selected_samples <- semi_join(historical_results(), selected_ratios, by = 'LABNUMMER')
      graph_selection(selected_samples)
    }
    return(ratios_plot)
  })
  
  
  output$fiatteer_grafiek_tabel <- DT::renderDataTable({
    req(graph_selection())
    selected_data <-
      graph_selection() %>% select(NAAM, LABNUMMER:REFMESSAGE, SAMPLINGDATE, MEASUREDATE)
    DT::datatable(
      data = selected_data,
      rownames = FALSE,
      extensions = ("RowGroup"),
      filter = "top",
      escape = FALSE,
      options = list(
        dom = 'tr',
        # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        order = list(list(1, 'desc')),
        rowGroup = list(dataSrc = c(0,1)),
        columnDefs = list(list(
          visible = FALSE , targets = c(0)
        ))
      )
    )
  })
}

shinyApp(ui = ui,
         server = server,
         options = list())  