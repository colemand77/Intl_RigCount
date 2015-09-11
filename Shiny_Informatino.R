Intl_Rig_Chart <- function(dataset){
  require(shiny)
  require(data.table)
  require(dygraphs)
  source("DyGraph_Functions.R")
  dataset <- as.data.table(dataset)
  shinyApp(
    ui = fluidPage(
      fluidRow(
        column(3, checkboxGroupInput('Region', 'Region',
                                     unique(dataset[,Region]),
                                     selected = unique(dataset[,Region]))),
        column(3, checkboxGroupInput('Land_Off','Land / Offshore', 
                                     unique(dataset[,Land_Off]),
                                     selected = unique(dataset[,Land_Off]))),
        column(3, checkboxGroupInput('OPEC',"OPEC Members?", 
                                     unique(dataset[,OPEC]),
                                     selected = unique(dataset[,OPEC]))),
        column(3, selectizeInput('Countries',"Countries",
                                 unique(dataset[,Country]),
                                 selected = unique(dataset[,Country]),
                                 multiple = TRUE))
      ),
      fluidRow(style = "padding-bottom: 10px;",
               column(3, selectInput('Stack', "Stacked Chart",
                                     choices = c(TRUE, FALSE))),
               column(3, selectInput('Group', "Group by:",
                                     choices = c("Region", "Land_Off","OPEC","Country","none")))
      ),
      
      fluidRow(column(3, div(strong("From:"), textOutput("SD", inline = TRUE)))
      ),
      
      fluidRow(column(3, div(strong("To:"), textOutput("ED", inline = TRUE)))
      ),
      
      fluidRow(style = "padding-bottom: 30px;",
               dygraphOutput("rigs")
      ),
      
      fluidRow(style = "padding-bottom: 30px;",
               dygraphOutput("rigs_index")
               )
      
    ), 
    server = function(input, output, session){
      Region <- reactive({input$Region})
      Land_Off <- reactive({input$Land_Off})
      OPEC <- reactive({input$OPEC})
      graph_stack <- reactive({if(input$Stack=="TRUE") TRUE else FALSE})
      graph_group <- reactive({input$Group})
      
      observe({
        countries_options <- countrylist(data = dataset, Region = Region(),
                                         Land_Off = Land_Off(),
                                         OPEC = OPEC())
        updateSelectizeInput(session, "Countries",
                             choices = countries_options,
                             selected = NULL)
      })
      Countries <- reactive({input$Countries})
      
    
      output$rigs <- renderDygraph({
        graph_rigcount(data = dataset, Country = Countries(),
                       Land_Off = Land_Off(),
                       Region = Region(),
                       OPEC = OPEC(),
                       group = graph_group(),
                       stacked = graph_stack())
      })
      
      output$daterangetest <- renderText({
        if (!is.null(input$rigs_date_window)){
          strftime(input$rigs_date_window[[1]], format = "%Y-%m%-%d")
        } else {
          strftime(max(dataset[,Date],na.rm = TRUE), format = "%Y-%m-%d")
        }
      })
      
      
      startDate <- reactive({
        if(!is.null(input$rigs_date_window)){
          strftime(input$rigs_date_window[[1]], format = "%Y-%m-%d")
        } else {
          strftime(min(dataset[,Date], na.rm = TRUE), format = "%Y-%m-%d")
        }
      })

      output$SD <- renderText({startDate()})
      
      endDate <- reactive({
        if(!is.null(input$rigs_date_window)){
          strftime(input$rigs_date_window[[2]], format = "%Y-%m-%d")
        } else {
          strftime(max(dataset[,Date], na.rm = TRUE), format = "%Y-%m-%d")
        }
      })
      
      output$ED <- renderText({endDate()}) 
      
      output$rigs_index <- renderDygraph({
        dygraph(
          ts_rigcount_indexed(data = dataset, group = graph_group(), 
                              indexed = TRUE,
                              start_date = startDate(), 
                              end_date = endDate(),
                              Country = Countries(), Land_Off = Land_Off(), Region = Region(), OPEC = OPEC()) 
        ) %>%
          dyAxis(name = 'y', label = "Rig Count Index")
          
    
      })     
      
    },
    options = list(height = 1300)
  )    
}

