library(shiny)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(readxl)
library(DT)

library(dbplyr)
library(odbc)


#check out https://laustep.github.io/stlahblog/posts/DTcallbacks.html#getting-the-selected-rows



############################ common functions ##########################
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
      ), as.factor)
      
    )
  
  
  
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

#UI input variables are intentionally in Dutch, should make it easier to keep them separate from output/internal variables
ui <- navbarPage(
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
                   hover = hoverOpts(id = "fiatteer_grafiek_zweef"),
                   brush = brushOpts(id = "fiatteer_grafiek_gebied")
                 )
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

server <- function(input, output, session) {
  ################ common server functions #############
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
  #############################################################
  
  #default groups (can be overridden)
  #data_groups <- reactiveValues(data_groups = c("MEETPUNT", "LABNR", "TESTCODE", "ELEMENTCODE"))
  
  #default settings
  #settings <- reactiveValues(settings = c(""))
  


  
  sql_connection_string <-
    reactiveVal("Driver=Oracle in OraClient19Home1;Host=db01-dcz-olin;Port=1521;")
  
  fiatteer_list <-
    reactive({
      req(input$fiatteer_input_file)
      excel_results_reader(input$fiatteer_input_file$datapath, sheet = 1)
    })
  
  fiatteer_results <-
    reactive({
      req(input$fiatteer_input_file)
      excel_results_reader(input$fiatteer_input_file$datapath, sheet = 2)
    })
  
  selected_sample <- reactive({
    req(fiatteer_list())
    if (!is.null(input$tabel_fiatteerlijst_rows_selected)) {
      return(fiatteer_list()[input$tabel_fiatteerlijst_rows_selected,])
    }
    else{
      return(fiatteer_list())
    }
  })
  current_result <- reactive({
    req(fiatteer_results())
    selected_labnummer <- select(selected_sample(), LABNUMMER)
    matching_results <- semi_join(fiatteer_results(),
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
    req(fiatteer_results())
    selected_meetpunt <- select(selected_sample(), MONSTERPUNTCODE)
    matching_results <- semi_join(fiatteer_results(),
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
                                  )
    # CZV_BZV_Ratio = RESULTAAT[ELEMENTCODE == "CZV"] / RESULTAAT[ELEMENTCODE ==
    #                                                            "BZV5"]
    # CZV_NKa_Ratio = RESULTAAT[ELEMENTCODE == "CZV"] / RESULTAAT[ELEMENTCODE ==
    #                                                            "nka"]
    # BZV_onopa_Ratio = RESULTAAT[ELEMENTCODE == "BZV5"] / RESULTAAT[ELEMENTCODE ==
    #                                                              "OB"]
    })
  
  selected_results_widened <- reactive ({
    historical_results() %>%      pivot_wider(
      id_cols = c(LABNUMMER, RUNNR),
      names_from = c(TESTCODE, ELEMENTCODE),
      values_from = RESULTAAT,
      names_sep = "<br>",
      unused_fn = list(MEASUREDATE = list, SAMPLINGDATE = list)
    )
  })
  
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
  observeEvent(input$fiatteer_input_file, {
    loadingtip <- showNotification("Laden...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(loadingtip), add = TRUE)
    on.exit(inputUpdater(uiComponent = "tp", inputId = "fiatteer_beeld",selected = "tab_fiatteerlijst"), add = TRUE)
  })
  
  #possibly better select options&other ideas at https://laustep.github.io/stlahblog/posts/DTcallbacks.html


   # updateTabsetPanel(inputId = "fiatteer_beeld",selected = "tab_sample")

  #check out https://rstudio.github.io/DT/extensions.html
  
  output$tabel_fiatteerlijst <- DT::renderDataTable({
    DT::datatable(
      data = fiatteer_list()[order(fiatteer_list()$PRIOFINISHDATE),],
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
       data = selected_results_widened(),
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
    #plot_data <- selected_results()
    #plot_user_choices <- fiatteer_plot_user_selection()
    
    #plotting only makes sense if there is a x-axis and y-axis
    
    plot <- ggplot(data = fiatteer_results(),
                   mapping = aes(x = SAMPLINGDATE, y = RESULTAAT)) +
      geom_point(size = 1.5,
                 alpha = 0.8,
                 aes(colour = TESTCODE)) +
      facet_wrap(vars(TESTCODE), scales = 'free_y')
    #geom_smooth(method="loess", fullrange = TRUE, span = 0.75, linewidth = 2) +
    #labs(title = plot_user_choices$title)
    
    # if (plot_user_choices$fiatteer_wrap_choice == TRUE)
    # {
    #   plot <-
    #     plot + facet_wrap(plot_user_choices$wrap_category, scales = 'free_y')
    # }
    #
    return(plot)
  })
}


shinyApp(ui = ui,
         server = server,
         options = list())  