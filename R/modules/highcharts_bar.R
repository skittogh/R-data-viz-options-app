highcharts_bar_ui <- function(id, label = "highcharts_bar_ui") {
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
            inputId = ns("select_year"),
            label = "Select year",
            choices = select_years,
            selected = "2021"
          ),
          selectInput(
            inputId = ns("select_numerical"),
            label = "Select value to plot",
            choices = select_numerical,
            selected = "Rate"
          ),
          uiOutput(ns("show_error")),
          uiOutput(ns("select_primary_variable")),
          uiOutput(ns("select_secondary_variable"))
        ),
        wellPanel(
          id = "well2",
          uiOutput(ns("select_colours"))
        )
      ),
      
      
      column(width = 9, offset = 1,
             
             h2("Highcharts Bar Chart"),
             br(),
             
             fluidRow(
               tabBox(
                 width = 10,
                 id = 'secondary_box',
                 tabPanel(value = 'barplot1',
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





highcharts_bar_server <- function(id, label = "highcharts_bar_server", parent) {
  
  
  moduleServer(
    id,
    function(input, output, session) {
      # 
      # output$select_colours <- renderUI({
      #   req(group(), input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
      #   options_filtered <-  if (length(group()) == 1) {
      #     names(Palette_Options)
      #   } else if (length(group()) != 1) {
      #     names(Palette_Options[as.numeric(Palette_Options[2,]) >= nrow(unique(dataset_filtered()[2]))]) # only return the names of palette options that contain the same or more options than variables to be displayed
      #   }
      #   
      #   selectInput(
      #     inputId = session$ns("change_colours"),
      #     label = "Change colour palette",
      #     choices = options_filtered,
      #     selected = options_filtered[1]
      #   )
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
        req(group(), input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        Palette_Options <- readRDS("Palette_Options.RDS")
        
        options_filtered <-  if (length(group()) == 1) {
          names(Palette_Options)
        } else if (length(group()) != 1) {
          names(Palette_Options[as.numeric(Palette_Options[2,]) >= nrow(unique(dataset_filtered()[2]))]) # only return the names of palette options that contain the same or more options than variables to be displayed
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
      
      output$select_primary_variable <- renderUI({
        selectInput(session$ns("primary_var"),
                    label = "Select primary variable" ,
                    choices = select_primary_var)
      })
      
      
      output$select_secondary_variable <- renderUI({
        req(input$primary_var)
        opts <- c("none", select_primary_var[select_primary_var != input$primary_var])
        selectInput(session$ns("secondary_var"),
                    label = "Select grouping variable" ,
                    choices = opts)
      })
      
      
      group <- reactive({
        req(input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        if(input$secondary_var == "none") {
          input$primary_var
        } else {
          c(input$primary_var, input$secondary_var)
        }
      })
      
      
      primary_factor_levels <- reactive({
        if(input$primary_var == "Variable_1") {c("up", "down", "left", "right", "middle")} else
          if(input$primary_var == "Variable_2") {c("small", "medium", "large")} else
            if(input$primary_var == "Variable_3") {c("Location_1", "Location_2", "Location_3", "Location_4", "Location_5", "Location_6", "Location_7", "Location_8", "Location_9", "Location_10", "Location_11", "Location_12")}
      }) 
      
      
      secondary_factor_levels <- reactive({
        if(input$secondary_var != "none") {
          if(input$secondary_var == "Variable_1") {c("up", "down", "left", "right", "middle")} else
            if(input$secondary_var == "Variable_2") {c("small", "medium", "large")} else
              if(input$secondary_var == "Variable_3") {c("Location_1", "Location_2", "Location_3", "Location_4", "Location_5", "Location_6", "Location_7", "Location_8", "Location_9", "Location_10", "Location_11", "Location_12")}
        }
      })
      
      
      dataset_filtered <- reactive({
        req(input$primary_var, input$secondary_var, group())
        
        dataset <- dataset %>%
          filter(year == input$select_year) 
        
        dataset <- 
          if(length(group()) == 1) {
            dataset %>%
              group_by(!!sym(input$primary_var)) %>%
              summarise(Number = sum(!!sym(input$select_numerical)), upper_ci = sum(upper_ci), lower_ci = sum(lower_ci),
                        .groups = 'drop')
          } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
            dataset %>%
              group_by(!!sym(input$primary_var), !!sym(input$secondary_var)) %>%
              summarise(Number = sum(!!sym(input$select_numerical)), upper_ci = sum(upper_ci), lower_ci = sum(lower_ci),
                        .groups = 'drop')
          }
        dataset
        # print(dataset)
      })
      
      

      # ----------- HIGHCHARTS -------------------------------------------------
      # generate  highchart plot code
      hc_plot_code <- reactive({
        req(palette(), input$change_colours, input$primary_var, input$secondary_var, input$select_numerical, input$select_year)

        if(input$select_numerical == "Number" | input$error_bars == F) {
        
        if(length(group()) == 1) {
          dataset_filtered() %>%
            hchart(.,
                   type = 'column',
                   hcaes(x = !!sym(input$primary_var),      
                         y = Number),
                   color = palette()[1]) %>% 
            hc_xAxis(categories = primary_factor_levels()) # factorising this doesn't work, so need to specify order of xaxis here
          
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
          
          var_num <- length(unique(dataset_filtered()[[input$secondary_var]])) # return number of variables for linking id's below (this is to get the error bars matching up well, bit tricky with highcharts)
          
          dataset_filtered <- dataset_filtered()
          dataset_filtered[[input$secondary_var]] <- factor(dataset_filtered[[input$secondary_var]], levels = secondary_factor_levels()) # factorise so that groups are in given order
          
          dataset_filtered %>%
            hchart(.,
                   type = 'column',
                   hcaes(x = !!sym(input$primary_var),
                         group = !!sym(input$secondary_var),
                         y = Number),
                   color = palette()[1:var_num]) %>% # needs same number of colours as categories so uses var_num
            hc_xAxis(categories = primary_factor_levels()) # factorising this doesn't work, so need to specify order of xaxis here
        }
        } else
          
          
          
          
        if(input$select_numerical == "Rate" & input$error_bars == T) {
          
          if(length(group()) == 1) {
            
            dataset_filtered() %>%
              hchart(.,
                     type = 'column',
                     # color = palette(),
                     hcaes(x = !!sym(input$primary_var),    
                           y = Number),
                     color = palette()[1]) %>% 
              
              hc_add_series(data = list_parse(mutate(dataset_filtered(), low = lower_ci, high = upper_ci)),
                            type = "errorbar") %>%
              
              hc_xAxis(categories = primary_factor_levels()) %>% # factorising this doesn't work, so need to specify order of xaxis here
              
              hc_plotOptions(
                errorbar = list(
                  color = "black",
                  # whiskerLength = 1,
                  stemWidth = 1
                )
              )
            
          } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
            
            var_num <- length(unique(dataset_filtered()[[input$secondary_var]])) # return number of variables for linking id's below (this is to get the error bars matching up well, bit tricky with highcharts)
            
            dataset_filtered <- dataset_filtered()
            dataset_filtered[[input$secondary_var]] <- factor(dataset_filtered[[input$secondary_var]], levels = secondary_factor_levels()) # factorise so that groups are in given order
            
            dataset_filtered %>%
              hchart(.,
                     type =  "column",
              hcaes(x = !!sym(input$primary_var), y = Number, group = !!sym(input$secondary_var)),
              id = letters[1:var_num],
              color = palette()[1:var_num]
            ) %>%

              hc_add_series(
                dataset_filtered,
                "errorbar",
                hcaes(y = Number, x = !!sym(input$primary_var), low = lower_ci, high = upper_ci, group = !!sym(input$secondary_var)),
                linkedTo = letters[1:var_num],
                enableMouseTracking = TRUE,
                showInLegend = FALSE
              ) %>%

              hc_xAxis(categories = primary_factor_levels()) %>% # factorising this doesn't work, so need to specify order of xaxis here
              
              hc_plotOptions(
                errorbar = list(
                  color = "black",
                  # whiskerLength = 1,
                  stemWidth = 1
                )
              )
            
            }
        }
          
          
      })

      
      # generate  highchart plot code for rendering code in app
      hc_plot_code_render <- reactive({
        req(group(), input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        
        if(length(group()) == 1 & input$error_bars == F) {
          paste0("
          library(highcharter)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1] 
          
          ",
                 
          single_bar,
                 
          "
          
          dataset_filtered %>%
            hchart(.,
                   type = 'column',
                   hcaes(x = Variable_1,      
                         y = Number),
                   color = palette) %>% 
            hc_xAxis(categories = c('up', 'down', 'left', 'right', 'middle')) # factorising this doesn't work, so need to specify order of xaxis here  "
          )
          
          
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var & input$error_bars == F) {
          paste0("
          library(highcharter)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1:3] 
          
          ",
                 
          multi_bar,
                 
          "
          
          dataset_filtered %>%
            mutate(Variable_2 = factor(Variable_2, levels = c('small', 'medium', 'large'))) %>% 
            hchart(.,
                   type = 'column',
                   hcaes(x = Variable_1,
                         group = Variable_2,
                         y = Number),
                   color = palette) %>% # needs same number of colours as categories so uses var_num
            hc_xAxis(categories = c('up', 'down', 'left', 'right', 'middle')) # factorising this doesn't work, so need to specify order of xaxis here "
          )
          
        } else if(length(group()) == 1 & input$select_numerical == "Rate" & input$error_bars == T) {
          paste0("
          library(highcharter)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1] 
          
          ",
                 
          single_bar,
                 
          "
          
          dataset_filtered %>%
              hchart(.,
                     type = 'column',
                     # color = palette,
                     hcaes(x = Variable_1,    
                           y = Number),
                     color = palette) %>% 
              
              hc_add_series(data = list_parse(mutate(dataset_filtered, low = lower_ci, high = upper_ci)),
                            type = 'errorbar') %>%
              
              hc_xAxis(categories = c('up', 'down', 'left', 'right', 'middle')) %>% # factorising this doesn't work, so need to specify order of xaxis here
              
              hc_plotOptions(
                errorbar = list(
                  color = 'black',
                  # whiskerLength = 1,
                  stemWidth = 1
                )
              )"
          )
          
        } else if(length(group()) == 2 & input$select_numerical == "Rate" & input$error_bars == T) {
          paste0("
          library(highcharter)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1:3] 
          
          ",
                 
          multi_bar,
                 
          "
          
           dataset_filtered %>%
              mutate(Variable_2 = factor(Variable_2, levels = c('small', 'medium', 'large'))) %>% 
              hchart(.,
                     type =  'column',
              hcaes(x = Variable_1, y = Number, group = Variable_2),
              id = letters[1:3],
              color = palette
            ) %>%

              hc_add_series(
                dataset_filtered,
                'errorbar',
                hcaes(y = Number, x = Variable_1, low = lower_ci, high = upper_ci, group = Variable_2),
                linkedTo = letters[1:3],
                enableMouseTracking = TRUE,
                showInLegend = FALSE
              ) %>%

              hc_xAxis(categories = c('up', 'down', 'left', 'right', 'middle')) %>% # factorising this doesn't work, so need to specify order of xaxis here
              
              hc_plotOptions(
                errorbar = list(
                  color = 'black',
                  # whiskerLength = 1,
                  stemWidth = 1
                )
              )"
          )
        } 
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
