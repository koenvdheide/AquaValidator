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
                 title = "Sample",
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
                   hover = hoverOpts(id = "fiatteer_grafiek_zweef", delay = 100),
                   brush = brushOpts(id = "fiatteer_grafiek_gebied")
                 ),
                 plotOutput(
                   "ratios_grafiek"
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
    
    print("loading excel")
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
    
    
    
    #  try({
    
    #poging tot juiste classes (niet langer relevant, laat staan voor de zekerheid)
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
      id_cols = c(LABNUMMER, RUNNR),
      names_from = c(TESTCODE, ELEMENTCODE),
      values_from = RESULTAAT,
      names_sep = "<br>",
      unused_fn = list(MEASUREDATE = list, SAMPLINGDATE = min)
    )
  }
  
  ratios_calculator <- function(results){
    #dataframe with labnummer and ratios per labnummer
    
    
  }
###########################reactive functions##################################
  
  selected_sample <- reactive({
    req(samples)
    if (!is.null(input$tabel_fiatteerlijst_rows_selected)) {
     # print(samples()[input$tabel_fiatteerlijst_rows_selected,])
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
    matching_results <- semi_join(results,
                                  selected_sample(),
                                  by = c('LABNUMMER')) %>% select(
                                    LABNUMMER,
                                    TESTCODE,
                                    ELEMENTCODE,
                                    RESULTAAT,
                                    RUNNR,
                                    REFMESSAGE,
                                    SAMPLINGDATE,
                                    MEASUREDATE
                                  )
  })
  
  historical_results <- reactive({
    #includes current result for now
    req(results)
    selected_meetpunt <- select(selected_sample(), MONSTERPUNTCODE)
    matching_results <- semi_join(results,
                                  selected_sample(),
                                  by = c('MONSTERPUNTCODE')) %>% select(
                                    LABNUMMER,
                                    TESTCODE,
                                    ELEMENTCODE,
                                    RESULTAAT,
                                    RUNNR,
                                    REFMESSAGE,
                                    SAMPLINGDATE,
                                    MEASUREDATE
                                  ) %>%
      arrange(desc(SAMPLINGDATE)) #it SHOULD already put the most recent result first but this ensures it
    #if statement for user selecting 10 historical results OR all (or a custom number?)

    first_ten_results <- matching_results %>% group_by(LABNUMMER) %>% filter(cur_group_id() <= 10 )
    matching_results <- first_ten_results
    
    graph_selection(rep(FALSE, nrow(matching_results))) #fill graph_selection so it doesn't throw out of bounds errors later
    return(matching_results)
    
  })
  
  selected_ratios <- reactive({
    req(ratios)
    selected_monsterpuntcode <- select(selected_sample(), MONSTERPUNTCODE)
    current_ratios <- ratios %>% filter(MONSTERPUNTCODE %in% selected_monsterpuntcode$MONSTERPUNTCODE)
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
    
    loadedsamples <- excel_results_reader(input$fiatteer_input_file$datapath, sheet = 1)
    samples <<- loadedsamples %>% arrange(PRIOFINISHDATE)
    
    results <<- excel_results_reader(input$fiatteer_input_file$datapath, sheet = 2)
    
    ratios <<-
      results %>%
      group_by(LABNUMMER,MONSTERPUNTCODE) %>%
      reframe(
        SAMPLINGDATE = min(SAMPLINGDATE),
        # MONSTERPUNTCODE = list? min? zou allemaal zelfde moeten zijn
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

    on.exit(removeNotification(loadingtip), add = TRUE)
    on.exit(inputUpdater(uiComponent = "tp", inputId = "fiatteer_beeld",selected = "tab_fiatteerlijst"), add = TRUE)
  })
  
  observeEvent(input$fiatteer_grafiek_zweef,{
    hover_selection(nearPoints(historical_results(),input$fiatteer_grafiek_zweef))
  })
  
  observeEvent(input$fiatteer_grafiek_klik, {
    graph_selection(nearPoints(historical_results(),
                               input$fiatteer_grafiek_klik))
  })
  
  observeEvent(input$fiatteer_grafiek_gebied, {
    graph_selection(brushedPoints(historical_results(), input$fiatteer_grafiek_gebied))
  })
  
  # observeEvent(input$fiatteer_grafiek_dblklik, {
  #   graph_selection(NULL)
  # })
  
#######################output functions############################  

   # updateTabsetPanel(inputId = "fiatteer_beeld",selected = "tab_sample")
  
  output$tabel_fiatteerlijst <- DT::renderDataTable({
    DT::datatable(
      data = samples,
      filter = "top",
      rownames = FALSE,
      extensions = c("Buttons"),
      options = list(
        searchHighlight = TRUE,
        dom = 'Bltipr',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    ) #dom needed to remove search bar (redundant with column search)    )
    
  })
  
   output$tabel_sample <- DT::renderDataTable({
     DT::datatable(
       data = results_widened(historical_results()),
       rownames = FALSE,
       extensions = c("Buttons", "RowGroup"),
       filter = "top",
       escape = FALSE,
       options = list(
         dom = 'Bltipr',
         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
         rowGroup = list(
           dataSrc = 0
           # startRender = JS(
           #   "function(rows, group) {",
           #   "return 'Sampling Datum:' +' ('+rows.count()+' rows)';",
           #   "}"
           # )
         )
       ) #dom needed to remove search bar (redundant with column search)
     )
     #toon test resultaten horende bij labnummer dat gebruiker in tabel_fiatteerlijst aaklikt
     #toon laatste resultaten met (last())
   })

   
  output$fiatteer_grafiek <- renderPlot({
    plot_data <- historical_results()

   # kept_data <- plot_data[graph_selection(), , drop = FALSE]
    #plot_user_choices <- fiatteer_plot_user_selection()
   # clicked_data <- plot_data[graph_selection(), , drop = FALSE]
    selected_data <- graph_selection() 
    
    results_plot <- ggplot(data = plot_data,
                   mapping = aes(x = SAMPLINGDATE, y = RESULTAAT, colour = TESTCODE)) +
      geom_line() +
      geom_point() +
        #aes(shape = selected_data),  
     # scale_size(limits = c("FALSE","TRUE"), range = c(1.5,2.5)) +
      facet_wrap(vars(TESTCODE), scales = 'free_y')
    
    
    # if(!is.null(selected_data)){ #clicked data has to show up in plot
    #   plot <- plot + geom_point()
    # }

    
    #ratios plot idea:
    # data = ratiosdata, group = ratios, x = SAMPLINGDATE, y = RATIO
    
    #move hover_data to something that doesn't call the WHOLE PLOT AGAIN
    #plot <- plot + geom_text(data = hover_data(), aes(label=LABNUMMER))
    
    
    #geom_smooth(method="loess", fullrange = TRUE, span = 0.75, linewidth = 2) +
    #labs(title = plot_user_choices$title)
    
    # if (plot_user_choices$fiatteer_wrap_choice == TRUE)
    # {
    #   plot <-
    #     plot + facet_wrap(plot_user_choices$wrap_category, scales = 'free_y')
    # }
    #
    return(results_plot)
  })
  
  output$ratios_grafiek <- renderPlot({
    plot_ratios <- selected_ratios()
    #selected ratios
    View(plot_ratios)
    ratios_plot <-
      ggplot(data = plot_ratios,
             mapping = aes(x = SAMPLINGDATE, y = WAARDE, colour = RATIO, group = MONSTERPUNTCODE)) +
      geom_line() +
      geom_point() +
      geom_smooth() +
      facet_wrap(vars(RATIO), scales = 'free_y')
    
    return(ratios_plot)
  })

  output$fiatteer_grafiek_tabel <- DT::renderDataTable({
    req(graph_selection())
   # selected_data <- filter(historical_results(), graph_selection())
    selected_data <- graph_selection()
    DT::datatable(
      data = selected_data,
      rownames = FALSE,
     # extensions = c("Buttons", "RowGroup"),
      filter = "top",
      escape = FALSE,
      options = list(
        dom = 'tr'
       # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
       # rowGroup = list(
          #dataSrc = 0
          # startRender = JS(
          #   "function(rows, group) {",
          #   "return 'Sampling Datum:' +' ('+rows.count()+' rows)';",
          #   "}"
          # )
         # )
      ) #
    )
  })
}


shinyApp(ui = ui,
         server = server,
         options = list())  