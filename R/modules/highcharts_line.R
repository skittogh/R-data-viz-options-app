highcharts_line_ui <- function(id, label = "highcharts_line_ui") {
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      br(),
      br(),
      
      column(
        width = 2,
        wellPanel(
          id = "well1",
          selectInput(
            inputId = ns("select_numerical"),
            label = "Select value to plot",
            choices = select_numerical,
            selected = "Rate"
          ),
          uiOutput(ns("show_error")),
          selectInput(ns("primary_var"),
                      label = "Select primary variable" ,
                      choices = c("none",select_primary_var))
        ),
        wellPanel(
          id = "well2",
          uiOutput(ns("select_colours"))
        )
      ),
      
      
      column(width = 9, offset = 1,
             
             h2("Highcharts Line Chart"),
             br(),
             
             fluidRow(
               tabBox(
                 width = 10,
                 id = 'secondary_box',
                 tabPanel(value = 'plot1',
                          h4(id = "nav_title2", "View plot"),
                          highchartOutput(ns("hc_plot_out"), height = "600px") %>% withSpinner(),
                          uiOutput(ns("load_time")),
                          br(),
                          hr(),
                          br(),
                          uiOutput(ns("plot_notes"))
                 ),
                 tabPanel(
                   value = 'code',
                   h4(id = "nav_title2", "View code"),
                   rclipboardSetup(), # set up ability to copy to clipboard
                   uiOutput(outputId = ns("clip"), class = "clipb"), 
                   verbatimTextOutput(ns("hc_code_out"))
                 )
               )
             )
      ) 
    )
  )
}





highcharts_line_server <- function(id, label = "highcharts_line_server", parent) {
  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      
      # output$select_colours <- renderUI({
      #   options_filtered <-  if (input$primary_var == "none") {
      #     names(Palette_Options)
      #   } else if (input$primary_var != "none") {
      #     names(Palette_Options[as.numeric(Palette_Options[2,]) >= length(unique(dataset_filtered()$Var))]) # only return the names of palette options that contain the same or more options than variables to be displayed
      #   }
      #   
      # selectInput(
      #   inputId = session$ns("change_colours"),
      #   label = "Change colour palette",
      #   choices = options_filtered,
      #   selected = options_filtered[1]
      # )
      # })
      # 
      # palette <- reactive({
      #   palette <-
      #     eval(parse(text =   Palette_Options[names(Palette_Options) == input$change_colours][1, ]))
      #   palette
      # })

      
      pal_list <- eventReactive(input$change_colours, {
        readRDS("pal_list.RDS")
      })
      
      output$select_colours <- renderUI({
        Palette_Options <- readRDS("Palette_Options.RDS")
        
        options_filtered <-  if (input$primary_var == "none") {
          names(Palette_Options)
        } else if (input$primary_var != "none") {
          names(Palette_Options[as.numeric(Palette_Options[2,]) >= length(unique(dataset_filtered()$Var))]) # only return the names of palette options that contain the same or more options than variables to be displayed
        }
        
        selectInput(
          inputId = session$ns("change_colours"),
          label = "Change colour palette",
          choices = options_filtered,
          selected = options_filtered[1]
        )
      })
      
      palette <- reactive({
        Palette_Options <- readRDS("Palette_Options.RDS")
        palette <-
          eval(parse(text =   Palette_Options[names(Palette_Options) == input$change_colours][1, ]))
        palette
      })
      
      output$show_error <- renderUI({
        if(input$select_numerical == "Rate"){
          checkboxInput(session$ns("error_bars"),
                        label = "Show error bars?",
                        value = FALSE)
        } else
        {NULL}
      })
      
    
      primary_factor_levels <- reactive({
        if(input$primary_var == "Variable_1") {c("up", "down", "left", "right", "middle")} else
          if(input$primary_var == "Variable_2") {c("small", "medium", "large")} else
            if(input$primary_var == "Variable_3") {c("Location_1", "Location_2", "Location_3", "Location_4", "Location_5", "Location_6", "Location_7", "Location_8", "Location_9", "Location_10", "Location_11", "Location_12")}
      }) 
      
      
      dataset_filtered <- reactive({
        req(input$primary_var)
        
        dataset <-
          if (input$primary_var == "none") {
            dataset %>%
              group_by(year) %>%
              summarise(
                Number = sum(!!sym(input$select_numerical)),
                upper_ci = sum(upper_ci),
                lower_ci = sum(lower_ci),
                .groups = 'drop'
              )
          } else if (input$primary_var != "none") {
            dataset %>%
              group_by(!!sym(input$primary_var), year) %>%
              summarise(
                Number = sum(!!sym(input$select_numerical)),
                upper_ci = sum(upper_ci),
                lower_ci = sum(lower_ci),
                .groups = 'drop'
              ) %>% 
              rename (Var = 1)
          }
         
        dataset
      })
      
      
      
      # ----------- HIGHCHARTS -------------------------------------------------
      # generate  highchart plot code
      hc_plot_code <- reactive({
        req(palette(), input$change_colours, input$primary_var, input$select_numerical)
        
        
        # palette further refined - plotly likes to have the correct number of options in the colour vector otherwise it does some odd things
        if (input$primary_var == "none") {
          palette <- palette()[1]
        } else if (input$primary_var != "none") {
          palette <- palette()[1:nrow(unique(dataset_filtered()[1]))]
        }
        
        
        
        if(input$select_numerical == "Number" | input$error_bars == F) {
          
          if(input$primary_var == "none") {
            dataset_filtered() %>%
              hchart(.,
                     type = 'line',
                     hcaes(x = year,      
                           y = Number),
                     color = palette) 
            
          } else if(input$primary_var != "none") {
            
            dataset_filtered() %>%
              mutate(Var = factor(Var, levels = primary_factor_levels())) %>% 
              hchart(.,
                     type = 'line',
                     hcaes(x = year,
                           group = Var,
                           y = Number),
                     color = palette) 
          }
        } else
          
          
          
          
          if(input$select_numerical == "Rate" & input$error_bars == T) {
            
            if(input$primary_var == "none") {
              
              dataset_filtered() %>%
                hchart(.,
                       type = 'line',
                       hcaes(x = year,    
                             y = Number),
                       color = palette) %>% 
                
                hc_add_series(data = list_parse(mutate(dataset_filtered(), low = lower_ci, high = upper_ci)),
                              type = "errorbar") %>%
                
                
                hc_plotOptions(
                  errorbar = list(
                    color = "black",
                    stemWidth = 1
                  )
                )
              
            } else if(input$primary_var != "none") {
              
              var_num <- length(unique(dataset_filtered()[["Var"]])) # return number of variables for linking id's below (this is to get the error bars matching up well, bit tricky with highcharts)
              
              dataset_filtered() %>%
                mutate(Var = factor(Var, levels = primary_factor_levels())) %>% 
                hchart(.,
                       type =  "line",
                       hcaes(x = year, 
                             y = Number, 
                             group = Var),
                       id = letters[1:var_num],
                       color = palette
                ) %>%
                
                hc_add_series(
                  dataset_filtered(),
                  "errorbar",
                  hcaes(y = Number, x = year, low = lower_ci, high = upper_ci, group = Var),
                  linkedTo = letters[1:var_num],
                  enableMouseTracking = TRUE,
                  showInLegend = FALSE
                ) %>%
                
                hc_plotOptions(
                  errorbar = list(
                    color = "black",
                    stemWidth = 1
                  )
                )
              
            }
          }
        
        
      })
      
      
      # generate  highchart plot code for rendering code in app
      hc_plot_code_render <- reactive({
        req(input$primary_var, input$select_numerical)
        
        if(input$primary_var == "none" & input$error_bars == F) {
          paste0("
          library(highcharter)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1] 
          
          ",
                 
                 single_line,
                 
                 "
          
          dataset_filtered %>%
              hchart(.,
                     type = 'line',
                     hcaes(x = year,      
                           y = Number),
                     color = palette)  "
          )
          
          
        } else if(input$primary_var != "none" & input$error_bars == F) {
          paste0("
          library(highcharter)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1:5] 
          
          ",
                 
                 multi_line,
                 
                 "
          
          dataset_filtered %>%
              mutate(Var = factor(Var, levels = c('up', 'down', 'left', 'right', 'middle'))) %>% 
              hchart(.,
                     type = 'line',
                     hcaes(x = year,
                           group = Var,
                           y = Number),
                     color = palette) "
          )
          
        } else if(input$primary_var == "none" & input$error_bars == T) {
          paste0("
          library(highcharter)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1] 
          
          ",
                 
                 single_line,
                 
                 "
          
          dataset_filtered %>%
                hchart(.,
                       type = 'line',
                       hcaes(x = year,    
                             y = Number),
                       color = palette) %>% 
                
                hc_add_series(data = list_parse(mutate(dataset_filtered, low = lower_ci, high = upper_ci)),
                              type = 'errorbar') %>%
                
                
                hc_plotOptions(
                      errorbar = list(
                      color = 'black',
                      stemWidth = 1
                  )
                )"
          )
          
        } else if(input$primary_var != "none" & input$error_bars == T) {
          paste0("
          library(highcharter)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1:5] 
          
          ",
                 
                 multi_line,
                 
                 "
          
          dataset_filtered %>%
                mutate(Var = factor(Var, levels = c('up', 'down', 'left', 'right', 'middle'))) %>% 
                hchart(.,
                       type =  'line',
                       hcaes(x = year, 
                             y = Number, 
                             group = Var),
                       id = letters[1:5],
                       color = palette
                ) %>%
                
                hc_add_series(
                        dataset_filtered,
                        'errorbar',
                        hcaes(y = Number, x = year, low = lower_ci, high = upper_ci, group = Var),
                        linkedTo = letters[1:5],
                        enableMouseTracking = TRUE,
                        showInLegend = FALSE
                ) %>%
                
                hc_plotOptions(
                  errorbar = list(
                    color = 'black',
                    stemWidth = 1
                  )
                )"
          )
        } else {NULL}
      })
      
      
      
      output$hc_plot_out <- renderHighchart({
        start_time <- Sys.time()
        h <- hc_plot_code()
        # load time return
        end_time <- Sys.time()
        time <- round(as.numeric(as.character(end_time - start_time)) * 1000, 2)
        output$load_time <- renderUI({
          HTML(paste0("<span style = 'color: #23395d; font-size: 19px'>", 
                      "Time to generate plot: ", "<span style = 'color: #23395d; font-size: 19px; font-weight: bold;'>", as.character(time), " milliseconds",
                      "</span>", "</span>"))
        })
        h
      })
      
      
      output$hc_code_out <- renderPrint({
        cat(hc_plot_code_render(),sep = "\n")
      })
      
      
      output$plot_notes <- renderUI({
        text <-  
          HTML(paste0(
            "<p>",
            "<span style = 'color: #23395d; font-size: 16px; font-weight: bold;'>",
            "Comments:",
            "</span>",
            "</p>",
            "<span style = 'color: #23395d; font-size: 16px'>",
            tagList(
              tags$ul(
                tags$li(tags$p("Uses highcharts.js library which is written in pure javascript.")),
                tags$li(tags$p("highcharter r package makes it easy for R users to create plots using this extensive javascript library.")),
                tags$li(tags$p("'Out of the box', plots are incredible to view and interact with. Transitioning plots is defualt behaviour.")),
                tags$li(tags$p("The code layout and syntax within R is simple.")),
                tags$li(tags$p("Highcharts is free to use for non commercial use."))
              )
            ),
            "</span>"
          ))
        text
      })
      
      
      
      # Add clipboard buttons
      output$clip <- renderUI({
        # text to copy
        text <- hc_plot_code_render()
        # button
        rclipButton(
          inputId = "clipbtn",
          label = "Copy to clip",
          clipText = text, 
          tooltip = "Click to copy palette to clipboard",
          placement = "top",
          options = list(delay = list(show = 800, hide = 100), trigger = "hover")
        )
      })
      
      
      
    }
  )
}
