library(shiny)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(readxl)
library(DT)

library(dbplyr)
library(odbc)


# merge analyse & fiatteren tabs
# hide extra options by default (that could be overwhelming) unless user clicks "extra options" checkmark
# add settings tab and help tab, settings for technical stuff like setting the database connection data and help tab for user explanation


analysis_options_tabs <- tabsetPanel(
  id = "extra_gebruiker_opties",
  type = "hidden",
  
  tabPanel(
    "tab_tabel_opties",
    checkboxGroupInput("tabel_kolom_selectie", "Toon kolommen:")
  ),
  
  tabPanel(
    "tab_tabel_samenvatting_opties",
    # actionButton("tabel_samenvatting_algemeen","Algemene samenvatting"),
    checkboxGroupInput(
      "tabel_samenvatting_groep",
      choices = NULL,
      label = "Toon samenvatting per:",
      selected = NULL,
    )
  ),
  tabPanel(
    "tab_grafiek_opties",
    #selectInput("grafiek_x_as_selectie",choices = NULL,label = "Grafiek x as"),
    #selectInput("grafiek_y_as_selectie",choices = NULL,label = "Grafiek y as"),
    
    textInput("grafiek_titel", "Grafiek titel", value =
                "Meetresultaten"),
    
    # dateRangeInput("grafiek_datum_bereik", "Grafiek datum bereik:", language="nl", separator = " tot "),
    #nog verbinden met datum range in grafiek (alleen logisch als x of y as een datum is?)
    
    
    varSelectInput(
      "grafiek_x_as_selectie",
      data = NULL,
      label = "Grafiek x-as:",
      selected = NULL
    ),
    varSelectInput(
      "grafiek_y_as_selectie",
      data = NULL ,
      label = "Grafiek y-as:",
      selected = NULL
    ),
    varSelectInput(
      "grafiek_wrap_categorie_selectie",
      data = NULL ,
      label = "Grafiek per categorie:",
      selected = NULL
    ),
    varSelectInput(
      "grafiek_kleur_selectie",
      data = NULL ,
      label = "Grafiek kleur per:",
      selected = NULL
    ),
    
    #nog een input maken voor welke testcodes gebruikers willen zien
    
    sliderInput(
      "grafiek_aantal_historisch",
      "Maximaal aantal historische datapunten",
      min = 1,
      max = 50,
      value = 10
    )
  ),
  tabPanel("tab_kaart_opties")
)



  
  tabPanel("Analyse",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               fileInput(
                 "analyse_input_file",
                 
                 #ook (bijvoorbeeld) csv en tsv?
                 label = "Kies Excel bestand",
                 accept = c(".xlsx", ".xls")
               ),
               #              actionButton("ophaal_knop",
               #                           label = "Haal samplelijst op"),
               br(),
               analysis_options_tabs,
             ),
             
             
             mainPanel(
               width = 9,
               tabsetPanel(
                 id = "analyse_selected_tab",
                 
                 tabPanel(
                   title = "Tabel",
                   value = "tab_tabel",
                   DT::dataTableOutput("tabel")
                 ),
                 tabPanel(
                   value = "tab_tabel_samenvatting",
                   title = "Tabel Samenvatting",
                   DT::dataTableOutput("tabel_samenvatting")
                 ),
                 tabPanel(
                   value = "tab_grafiek",
                   title = "Grafiek",
                   plotOutput(
                     "grafiek",
                     click = "grafiek_klik",
                     dblclick = dblclickOpts(id = "grafiek_dblklik"),
                     hover = hoverOpts(id = "grafiek_zweef"),
                     brush = brushOpts(id = "grafiek_gebied")
                   ),
                   checkboxInput("grafiek_wrap_keuze",
                                 "Meerdere grafieken?",
                                 value = FALSE)
                   #DT::dataTableOutput("grafiek_tabel"),
                   #checkboxGroupInput("grafiek_wrap_selectie", "Kies grafieken:",inline = TRUE)
                 ),
                 tabPanel(title = "Kaart", value = "tab_kaart",
                          leafletOutput("kaart"))
               )
             )
           ))



  






  
  

  
  #nog generiek maken ipv afhankelijk van USEDRESULT
 # fiatteer_summary <-
  #  reactive({
      # data <- fiatteer_file_data()
      # 
      # return(
      #   summary_data <-
      #     data %>% group_by(MEETPUNT, LABNR) %>%
      #     summarise(
      #       #move ratios to sample table
      #       Tests = list(TESTCODE),
      #       CZV_BZV_Ratio = USEDRESULT[TESTCODE == "czv"] / USEDRESULT[TESTCODE ==
      #                                                                  "bzv"],
      #       CZV_NKa_Ratio = USEDRESULT[TESTCODE == "czv"] / USEDRESULT[TESTCODE ==
      #                                                                  "nka"],
      #       BZV_onopa_Ratio = USEDRESULT[TESTCODE == "bzv"] / USEDRESULT[TESTCODE ==
      #                                                                    "onopa"]
      # 
      # 
      #     )
      # )
#
   # })
  
  
  

  



    
################ analyse tab #############
  analyse_file_data <- reactive({
    req(input$analyse_input_file)
    fiatteer_data <- excel_results_reader(input$analyse_input_file$datapath)
    
    inputUpdater(
      "cbg",
      inputId = "tabel_kolom_selectie",
      choices = colnames(fiatteer_data),
      selected = colnames(fiatteer_data)
    )
    
    inputUpdater(
      "cbg",
      inputId = "tabel_samenvatting_groep",
      choices = colnames(fiatteer_data),
      selected = c("MEETPUNT", "TESTCODE")
    )
    
    inputUpdater("vs",
                 inputId = "grafiek_x_as_selectie",
                 data = fiatteer_data,
                 selected = "SAMPLINGDATE")
    inputUpdater("vs",
                 inputId = "grafiek_y_as_selectie",
                 data = fiatteer_data,
                 selected = "USEDRESULT")
    inputUpdater("vs",
                 inputId = "grafiek_wrap_categorie_selectie",
                 data = fiatteer_data,
                 selected = "TESTCODE")
    inputUpdater("vs",
                 inputId = "grafiek_kleur_selectie",
                 data = fiatteer_data,
                 selected = "TESTCODE")
    
    #blokkeer opties bij varSelect die de grafiek overbelasten (zoals results bij colour), misschien filtereren op lengte?

    return(fiatteer_data)
    
  }) %>% bindCache(input$analyse_input_file) #cache the data and prevent function calls from (accidentally) triggering data processing again unless analyse_input_file actually changes
  
  observeEvent(input$ophaal_knop,
               {
                 #DBI::dbCanConnect(odbc::odbc(), Driver = "Oracle in OraClient19Home1",Host =  "db01-dcz-olin", Port = 1521 )
                 
                 showModal(modalDialog(title = "WERKT NIET", easyClose = TRUE))
                 print("sql button")
                 
                # con <- DBI::dbConnect(odbc::odbc(), "oracledb", Driver = "Oracle in OraClient19Home1",Host =  "db01-dcz-olin", Port = 1521 )
               })
  #.connection_string = "Driver = {Microsoft ODBC for Oracle};Host=db01-dcz-olin;Port= 1521;Database=qmp"
  # observeEvent(input$tabel_samenvatting_algemeen,{
  #   req(input$analyse_input_file)
  #   
  # })
  
  observeEvent(input$analyse_selected_tab, {
    #verandert linker opties paneel afhankelijk van welk tab de gebruiker rechts aanklikt
    inputUpdater("tp",
                 inputId = "extra_gebruiker_opties",
                 selected = paste0(input$analyse_selected_tab, "_opties"))
  })
  
  #nog generiek maken ipv afhankelijk van USEDRESULT
  table_summary_selection <-
    reactive({
      return(
        summary_data <-
          analyse_file_data() %>% group_by(pick(input$tabel_samenvatting_groep)) %>%
          
          summarise(
            Metingen = n(),
            Minimum = min(USEDRESULT),
            Gemiddelde = mean(USEDRESULT),
            Mediaan = median(USEDRESULT),
            Maximum = max(USEDRESULT),
            Standaarddeviatie = sd(USEDRESULT),
            AbsoluteMediaanAfwijking = mad(USEDRESULT)
          )
      )
    })
  
  
  plot_user_selection <-
    reactive({
      user_selection <- list(
        title = input$grafiek_titel,
        x_axis = input$grafiek_x_as_selectie,
        y_axis = input$grafiek_y_as_selectie,
        colour = input$grafiek_kleur_selectie,
        shape = input$grafiek_vorm_selectie,
        wrap_choice = input$grafiek_wrap_keuze,
        wrap_category = input$grafiek_wrap_categorie_selectie
      )
      # toon alle factor levels bij de wrap categorie (nog niet werkzaam)
      #freezeReactiveValue(input, "grafiek_wrap_selectie")
      #updateCheckboxGroupInput(inputId = "grafiek_wrap_selectie",inline = TRUE,choices = input$grafiek_wrap_categorie_selectie,selected = input$grafiek_wrap_categorie_selectie)
      
      return(user_selection)
    })
  
  
  map <-
    leaflet(options = leafletOptions(minZoom = 7,
                                     maxZoom = 15)) %>%
    
    setMaxBounds(3.25, 50.5, 7.6, 54) %>%
    setView(6, 52.5, zoom = 9) %>%
    addWMSTiles(
      "https://service.pdok.nl/brt/achtergrondkaart/wmts/v2_0/standaard/EPSG:3857/{z}/{x}/{y}.png",
      layers = "standaard",
      group = "Standaard",
      options = WMSTileOptions(format = "image/png")
    ) %>%
    addWMSTiles(
      "https://service.pdok.nl/brt/achtergrondkaart/wmts/v2_0/water/EPSG:3857/{z}/{x}/{y}.png",
      layers = "water",
      group = "Water",
      options = WMSTileOptions(format = "image/png")
    ) %>%
    addWMSTiles(
      "https://service.pdok.nl/rws/kaderrichtlijnwater/wms/v1_0?request=getCapabilities&service=WMS",
      layers = "oppervlaktewaterlichamen_lijnen,oppervlaktewaterlichamen_vlakken",
      group = "Oppervlaktewaterlichamen",
      options = WMSTileOptions(transparent = TRUE,
                               format = "image/png")
    ) %>%
    addWMSTiles(
      "https://geodata.nationaalgeoregister.nl/hwh/eenheden/wms/v1_0?service=wms",
      layers = "AU.AdministrativeUnit",
      group = "Waterschapsgebieden",
      options = WMSTileOptions(format = "image/png",
                               transparent = TRUE)
      
    ) %>%
    addLayersControl(
      baseGroups = c("Standaard", "Water"),
      overlayGroups = c("Oppervlaktewaterlichamen",
                        "Waterschapsgebieden"),
      options = layersControlOptions(collapsed = TRUE)
    )
  
  # brakke tileset
  # %>%
  #   addWMSTiles(
  #     "https://service.pdok.nl/hwh/wbehgebimwa/wms/v1_0?request=GetCapabilities&service=WMS",
  #     layers = "afvoergebiedAanvoergebied",
  #     group = "Afvoergebied en Aanvoergebied",
  #     options = WMSTileOptions(
  #       format = "image/png",
  #       transparent = TRUE ,
  #       minZoom = 7,
  #       maxZoom = 13
  #     )
  #
  #   
  
  
  
  output$tabel <- DT::renderDataTable({
    DT::datatable(
      analyse_file_data(),
      filter = 'top',
      rownames = FALSE,
      options = list(searchHighlight = TRUE, dom = 'ltipr') #dom needed to remove search bar (redundant with column search)
    )
    #laat user kolommen selecteren [,input$gewenste_kolommen,drop = FALSE],options = list())
  })
  output$grafiek_tabel <- DT::renderDataTable({
    #render 1 tabel rij onder grafiek die de waardes toont van het punt in de grafiek waar de user op/nabij klikt
    DT::datatable(nearPoints(analyse_file_data(), input$grafiek_klik)) #xvar = plot(), yvar = plot())#xvar en yxar moeten x en y as namen uit grafiek ophalen
  })
  
  
  output$tabel_samenvatting <- DT::renderDataTable({
    table_summary_selection()
  })
  
  
  output$grafiek <- renderPlot({
    plot_data <- analyse_file_data()
    plot_user_choices <- plot_user_selection()
    
    #plotting only makes sense if there is a x-axis and y-axis
    if (req(!is.null(plot_user_choices$x_axis) &
            !is.null(plot_user_choices$y_axis))) {
      plot <- ggplot(plot_data,
                     mapping = aes(x = .data[[plot_user_choices$x_axis]], y = .data[[plot_user_choices$y_axis]])) +
        geom_point(size = 1.5,
                   alpha = 0.8,
                   aes(colour = .data[[plot_user_choices$colour]])) +
        #geom_smooth(method="loess", fullrange = TRUE, span = 0.75, linewidth = 2) +
        labs(title = plot_user_choices$title)
      
      if (plot_user_choices$wrap_choice == TRUE)
      {
        plot <-
          plot + facet_wrap(plot_user_choices$wrap_category, scales = 'free_y')
      }
    }
    return(plot)
    
    
  })
  # output$grafiek_klik <-
  # output$grafiek_dblklik <-
  # output$grafiek_zweef <-
  # output$grafiek_gebied <-
  
  output$kaart <- renderLeaflet({
    map
  })# %>% bindCache(input$analyse_input_file) 
  

shinyApp(ui = ui,
         server = server,
         options = list())