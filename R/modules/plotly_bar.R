plotly_bar_ui <- function(id, label = "plotly_bar_ui") {
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
             
             h2("Plotly Bar Chart"),
             br(),
             
             fluidRow(
               tabBox(
                 width = 10,
                 id = 'secondary_box',
                 tabPanel(value = 'barplot1',
                          h4(id = "nav_title2", "View plot"),
                          plotlyOutput(ns("plotly_plot_out"), height = "600px") %>% withSpinner(),
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
                   verbatimTextOutput(ns("plotly_code_out"))
                 )
               ),
               
             )
      ) 
    )
  )
}





plotly_bar_server <- function(id, label = "plotly_bar_server", parent) {
  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # pal_list <- eventReactive(input$change_colours, {
      #   readRDS("pal_list.RDS")
      # })
      # 
      # output$select_colours <- renderUI({
      #   req(group(), input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
      #   Palette_Options <- readRDS("Palette_Options.RDS")
      #   
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
      #   Palette_Options <- readRDS("Palette_Options.RDS")
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
          if (length(group()) == 1) {
            dataset %>%
              group_by(!!sym(input$primary_var)) %>%
              summarise(
                Number = sum(!!sym(input$select_numerical)),
                upper_ci = sum(upper_ci),
                lower_ci = sum(lower_ci),
                .groups = 'drop'
              )
          } else if (length(group()) == 2 &
                     input$primary_var != input$secondary_var) {
            dataset %>%
              group_by(!!sym(input$primary_var),!!sym(input$secondary_var)) %>%
              summarise(
                Number = sum(!!sym(input$select_numerical)),
                upper_ci = sum(upper_ci),
                lower_ci = sum(lower_ci),
                .groups = 'drop'
              )
          }
        dataset
        # print(dataset)
      })
      
      
      
      # ----------- PLOTLY -----------------------------------------------------  
      # generate  plotly plot code
      plotly_plot_code <- reactive({
        req(palette(), input$change_colours, input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        
        # palette further refined - plotly likes to have the correct number of options in the colour vector otherwise it does some odd things
        if (input$secondary_var == "none") {
          palette <- palette()[1]
        } else if (input$secondary_var != "none") {
          palette <- palette()[1:nrow(unique(dataset_filtered()[2]))]
        }
        
        
        if(input$select_numerical == "Number" | input$error_bars == F) {
          
          if(length(group()) == 1) {
            
            dataset_filtered() %>%
              plot_ly(
                x = factor(dataset_filtered()[[input$primary_var]], levels = primary_factor_levels()),
                y = ~Number,
                type = "bar",
                marker =list(color = palette[1])
              ) %>% 
              layout(xaxis = list(titlefont = list(size = 16), tickfont = list(size = 14), title = list(text = input$primary_var)), 
                     yaxis = list(titlefont = list(size = 16), tickfont = list(size = 14), title = list(text = input$select_numerical)))
            
          } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
            dataset_filtered() %>%
              plot_ly(x = factor(dataset_filtered()[[input$primary_var]], levels = primary_factor_levels()),
                      y = ~Number,
                      type = "bar",
                      color = factor(dataset_filtered()[[input$secondary_var]], levels = secondary_factor_levels()),
                      colors = palette
              ) %>% 
              layout(xaxis = list(titlefont = list(size = 16), tickfont = list(size = 14), title = list(text = input$primary_var)), 
                     yaxis = list(titlefont = list(size = 16), tickfont = list(size = 14), title = list(text = input$select_numerical)), 
                     legend = legend_format)
          }
          
        } else
          
          
          
          
          
          if(input$select_numerical == "Rate" & input$error_bars == T) {
            
            if(length(group()) == 1) {
              
              dataset_filtered() %>%
                plot_ly(
                  x = factor(dataset_filtered()[[input$primary_var]], levels = primary_factor_levels()),
                  y = ~Number,
                  type = "bar",
                  marker =list(color = palette[1]),
                  error_y = ~list(
                    type = "data",
                    symmetric = "FALSE",
                    array = dataset_filtered()$upper_ci  - dataset_filtered()$Number, 
                    arrayminus = dataset_filtered()$Number - dataset_filtered()$lower_ci,
                    opacity = 1,
                    width = 2.5,
                    thickness = 1,
                    color = 'black'
                  ),
                  hoverinfo = "text",
                  hovertext =  ~paste0(dataset_filtered()[[input$primary_var]],
                                       ": ",
                                       round(Number,1),
                                       " (",
                                       round(dataset_filtered()$lower_ci,1),
                                       ",",
                                       round(dataset_filtered()$upper_ci,1),
                                       ")"
                  )
                  
                ) %>% 
                layout(xaxis = list(titlefont = list(size = 16), tickfont = list(size = 14), title = list(text = input$primary_var)), 
                       yaxis = list(titlefont = list(size = 16), tickfont = list(size = 14), title = list(text = input$select_numerical)))
              
            } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
              dataset_filtered() %>%
                plot_ly(x = factor(dataset_filtered()[[input$primary_var]], levels = primary_factor_levels()),
                        y = ~Number,
                        type = "bar",
                        color = factor(dataset_filtered()[[input$secondary_var]], levels = secondary_factor_levels()),
                        colors = palette,
                        error_y = ~list(
                          type = "data",
                          symmetric = "FALSE",
                          array = dataset_filtered()$upper_ci  - dataset_filtered()$Number,
                          arrayminus = dataset_filtered()$Number - dataset_filtered()$lower_ci,
                          opacity = 1,
                          width = 2.5,
                          thickness = 1,
                          color = 'black'
                        ),
                        hoverinfo = "text",
                        hovertext =  ~paste0(dataset_filtered()[[input$primary_var]],
                                             " & ",
                                             dataset_filtered()[[input$secondary_var]],
                                             ": ",
                                             round(Number,1),
                                             " (",
                                             round(dataset_filtered()$lower_ci,1),
                                             ",",
                                             round(dataset_filtered()$upper_ci,1),
                                             ")"
                        )
                ) %>% 
                layout(xaxis = list(titlefont = list(size = 16), tickfont = list(size = 14), title = list(text = input$primary_var)), 
                       yaxis = list(titlefont = list(size = 16), tickfont = list(size = 14), title = list(text = input$select_numerical)), 
                       legend = legend_format)
            }
          }
        
      })
      
      
      # generate  plotly plot code for rendering code in app
      plotly_plot_code_render <- reactive({
        req(group(), input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        
        if(length(group()) == 1 & input$error_bars == F) {
          paste0("
          library(plotly)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1] 
          
          ",
                 
                 single_bar,
                 
                 "
          
          dataset_filtered %>%
              plot_ly(
                x = ~factor(Variable_1, levels = c('up', 'down', 'left', 'right', 'middle')),
                y = ~Number,
                type = 'bar',
                marker =list(color = palette)
              ) %>% 
              layout(xaxis = list(titlefont = list(size = 16), tickfont = list(size = 14), title = list(text = 'x_var')), 
                     yaxis = list(titlefont = list(size = 16), tickfont = list(size = 14), title = list(text = 'y_var')))  "
          )
          
          
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var & input$error_bars == F) {
          paste0("
          library(plotly)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1:3] 
          
          ",
                 
                 multi_bar,
                 
                 "
          
          dataset_filtered %>%
              plot_ly(x = ~factor(Variable_1, levels = c('up', 'down', 'left', 'right', 'middle')),
                      y = ~Number,
                      type = 'bar',
                      color = ~factor(Variable_2, levels = c('small', 'medium', 'large')),
                      colors = palette
              ) %>% 
              layout(xaxis = list(titlefont = list(size = 16), tickfont = list(size = 14), title = list(text = 'x_var')), 
                     yaxis = list(titlefont = list(size = 16), tickfont = list(size = 14), title = list(text = 'y_var')), 
                     legend = list(
                                 title = '',
                                 font = list(
                                           family = 'arial',
                                           size = 12,
                                           color = '#000'),
                                 bgcolor = '#f5f5f5',
                                 bordercolor = '#f5f5f5',
                                 borderwidth = 2,
                                 orientation = 'h', 
                                 xanchor = 'center', 
                                 x = .5, 
                                 y = -.2, 
                                 entrywidth = 100
                      )
                 ) "
          )
          
        } else if(length(group()) == 1 & input$select_numerical == "Rate" & input$error_bars == T) {
          paste0("
          library(plotly)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1] 
          
          ",
                 
                 single_bar,
                 
                 "
          
          dataset_filtered %>%
                plot_ly(
                  x = ~factor(Variable_1, levels = c('up', 'down', 'left', 'right', 'middle')),
                  y = ~Number,
                  type = 'bar',
                  marker =list(color = palette),
                  error_y = ~list(
                    type = 'data',
                    symmetric = 'FALSE',
                    array = upper_ci - Number, 
                    arrayminus = Number - lower_ci,
                    opacity = 1,
                    width = 2.5,
                    thickness = 1,
                    color = 'black'
                  ),
                  hoverinfo = 'text',
                  hovertext =  ~paste0(Variable_1,
                                       ': ',
                                       round(Number,1),
                                       ' (',
                                       round(lower_ci,1),
                                       ',',
                                       round(upper_ci,1),
                                       ')'
                  )
                  
                ) %>% 
                layout(xaxis = list(titlefont = list(size = 16), tickfont = list(size = 14), title = list(text = 'x_var')), 
                       yaxis = list(titlefont = list(size = 16), tickfont = list(size = 14), title = list(text = 'y_var')))"
          )
          
        } else if(length(group()) == 2 & input$select_numerical == "Rate" & input$error_bars == T) {
          paste0("
          library(plotly)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1:3] 
          
          ",
                 
                 multi_bar,
                 
                 "
          
            dataset_filtered %>%
                plot_ly(x = ~factor(Variable_1, levels = c('up', 'down', 'left', 'right', 'middle')),
                        y = ~Number,
                        type = 'bar',
                        color = ~factor(Variable_2, levels = c('small', 'medium', 'large')),
                        colors = palette,
                        error_y = ~list(
                            type = 'data',
                            symmetric = 'FALSE',
                            array = upper_ci - Number,
                            arrayminus = Number - lower_ci,
                            opacity = 1,
                            width = 2.5,
                            thickness = 1,
                            color = 'black'
                        ),
                        hoverinfo = 'text',
                        hovertext =  ~paste0(Variable_1,
                                             ' & ',
                                             Variable_2,
                                             ': ',
                                             round(Number,1),
                                             ' (',
                                             round(lower_ci,1),
                                             ',',
                                             round(upper_ci,1),
                                             ')'
                        )
                ) %>% 
                layout(xaxis = list(titlefont = list(size = 16), tickfont = list(size = 14), title = list(text = 'x_var')), 
                       yaxis = list(titlefont = list(size = 16), tickfont = list(size = 14), title = list(text = 'y_var')), 
                       legend = list(
                                 title = '',
                                 font = list(
                                           family = 'arial',
                                           size = 12,
                                           color = '#000'),
                                 bgcolor = '#f5f5f5',
                                 bordercolor = '#f5f5f5',
                                 borderwidth = 2,
                                 orientation = 'h', 
                                 xanchor = 'center', 
                                 x = .5, 
                                 y = -.2, 
                                 entrywidth = 100
                      ))"
          )
        } 
      })
      
      
      output$plotly_plot_out <- renderPlotly({
        start_time <- Sys.time()
        h <- plotly_plot_code()
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
      
      
      output$plotly_code_out <- renderPrint({
        cat(plotly_plot_code_render(),sep = "\n")
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
                tags$li(tags$p("Uses plotly.js library which is built on top of D3.js (like many other plotting packages) as well as stack.gl.")),
                tags$li(tags$p("Feature rich and very user friendly.")),
                tags$li(tags$p(
                  "Interestingly plotly seems to treat colour palettes a bit differently. instead of using the first x colours in palette based on the number of variables,
                         it will try its best to span the whole colour palette first to last in colour vector. One way to avoid this would be to subset the vector the exact number of variables for the particular dataset,
                         for example 'palette[1:length(unique(dataset$variable))]'."))
              )
            ),
            "</span>"
          ))
        
        text
      })
      
      
      
      
      # Add clipboard buttons
      output$clip <- renderUI({
        # text to copy
        text <- plotly_plot_code_render()
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
