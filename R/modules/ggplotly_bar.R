ggplotly_bar_ui <- function(id, label = "ggplotly_bar_ui") {
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
             
             h2("GGPlotly Bar Chart"),
             br(),
             
             fluidRow(
               tabBox(
                 width = 10,
                 id = 'secondary_box',
                 tabPanel(value = 'barplot1',
                          h4(id = "nav_title2", "View plot"),
                          plotlyOutput(ns("ggplotly_plot_out"), height = "600px") %>% withSpinner(hide.ui = FALSE),
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
                   verbatimTextOutput(ns("ggplotly_code_out"))
                 )
               )
             )
      ) 
    )
  )
}





ggplotly_bar_server <- function(id, label = "ggplotly_bar_server", parent) {
  
  
  moduleServer(
    id,
    function(input, output, session) {
      
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
      })
      
      
      
      # ----------- GGPLOTLY ---------------------------------------------------
      # generate  ggplotly plot code
      ggplotly_plot_code <- reactive({
        req(palette(), input$change_colours, input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        
        if(input$select_numerical == "Number" | input$error_bars == F) {
          
          if(length(group()) == 1) {
            a <- dataset_filtered() %>%
              
              ggplot(aes(y = Number, x = factor(dataset_filtered()[[input$primary_var]], levels = primary_factor_levels()),
                         text = paste(dataset_filtered()[[input$primary_var]],
                                      ": ", 
                                      round(Number,2)
                                      )
                         )) +
              geom_bar(stat="identity", fill =  palette()[1]) +
              xlab(input$primary_var) +
              ggplot_theme 
            ggplotly(a, tooltip = c("text")) %>%
              layout(legend = legend_format, yaxis = list(title = list(text = input$select_numerical)))
            
          } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
            a <- dataset_filtered() %>%
              
              ggplot(aes(fill = factor(dataset_filtered()[[input$secondary_var]],levels = secondary_factor_levels()), y = Number, x = factor(dataset_filtered()[[input$primary_var]], levels = primary_factor_levels()),
                         text = paste(dataset_filtered()[[input$primary_var]], 
                                      " & ", 
                                      dataset_filtered()[[input$secondary_var]], 
                                      ": ", 
                                      round(Number,2)
                                      )
                         )) +
              geom_bar(position="dodge", stat="identity") +
              xlab(input$primary_var) +
              scale_fill_manual(values = palette()) +
              ggplot_theme 
            ggplotly(a, tooltip = c("text")) %>%
              layout(legend = legend_format, yaxis = list(title = list(text = input$select_numerical)))
            
          }
        } else
        
        
        
        if(input$select_numerical == "Rate" & input$error_bars == T) {
          
        if(length(group()) == 1) {
          a <- dataset_filtered() %>%
            
            ggplot(aes(y = Number, x = factor(dataset_filtered()[[input$primary_var]], levels = primary_factor_levels()),
                       text = paste(dataset_filtered()[[input$primary_var]],
                                    ": ", 
                                    round(Number,2),
                                    " (",
                                    round(dataset_filtered()$lower_ci,1),
                                    ",",
                                    round(dataset_filtered()$upper_ci,1),
                                    ")"
                                    )
                       )) +
            geom_bar(stat="identity", fill =  palette()[1]) +
            xlab(input$primary_var) +
            ggplot_theme +
            geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(.9)) 
          ggplotly(a, tooltip = c("text")) %>%
            layout(legend = legend_format, yaxis = list(title = list(text = input$select_numerical)))
          
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
          a <- dataset_filtered() %>%
            
            ggplot(aes(fill = factor(dataset_filtered()[[input$secondary_var]],levels = secondary_factor_levels()), y = Number, x = factor(dataset_filtered()[[input$primary_var]], levels = primary_factor_levels()),
                       text = paste(dataset_filtered()[[input$primary_var]], 
                                    " & ", 
                                    dataset_filtered()[[input$secondary_var]], 
                                    ": ", 
                                    round(Number,2),
                                    " (",
                                    round(dataset_filtered()$lower_ci,1),
                                    ",",
                                    round(dataset_filtered()$upper_ci,1),
                                    ")"
                                    )
                       )) +
            geom_bar(position="dodge", stat="identity") +
            xlab(input$primary_var) +
            scale_fill_manual(values = palette()) +
            ggplot_theme +
            geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(.9)) 
          ggplotly(a, tooltip = c("text")) %>%
            layout(legend = legend_format, yaxis = list(title = list(text = input$select_numerical)))
          
        }
        }
      })
      
      # generate  ggplotly plot code for rendering code in app
      ggplotly_plot_code_render <- reactive({
        req(group(), input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        
        if(length(group()) == 1 & input$error_bars == F) {
          paste0("
          library(plotly)
          library(ggplot2)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1] 
          
          ",
                 
          single_bar,
                 
          "
          
          a <- dataset_filtered %>%
                ggplot(aes(y = Number, 
                           x = factor(Variable_1, levels = c('up', 'down', 'left', 'right', 'middle')), 
                           text = paste(Variable_1,
                                        ': ', 
                                        round(Number,2)
                                        )
                           )) +
                geom_bar(stat = 'identity', fill =  palette) +
                xlab('x_var') +
                ggplot2::theme(panel.background = ggplot2::element_blank(), 
                           legend.title = ggplot2::element_blank(), 
                           axis.ticks = ggplot2::element_blank(), 
                           panel.grid.major.y = ggplot2::element_line(color = 'grey'), 
                           axis.text = ggplot2::element_text(size = 10), 
                           axis.title = ggplot2::element_text(size = 12), 
                           legend.text = ggplot2::element_text(size = 10), 
                           strip.text = ggplot2::element_text(size = 10), 
                           legend.key = ggplot2::element_blank(), 
                           plot.margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 20, unit = 'pt'))
            
            ggplotly(a, tooltip = c('text')) %>%
              layout(yaxis = list(title = list(text = 'y_var')),
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
                )  "
          )
          
          
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var & input$error_bars == F) {
          paste0("
          library(plotly)
          library(ggplot2)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1:3] 
          
          ",
                 
          multi_bar,
                 
          "
          
          a <- dataset_filtered %>%
              ggplot(aes(fill = factor(Variable_2, levels = c('small', 'medium', 'large')), 
                          y = Number, 
                          x = factor(Variable_1, levels = c('up', 'down', 'left', 'right', 'middle')),
                          text = paste(Variable_1, 
                                      ' & ', 
                                      Variable_2, 
                                      ': ', 
                                      round(Number,2)
                                      )
                         )) +
              geom_bar(position='dodge', stat='identity') +
              xlab('x_var') +
              scale_fill_manual(values = palette) +
              ggplot2::theme(panel.background = ggplot2::element_blank(), 
                           legend.title = ggplot2::element_blank(), 
                           axis.ticks = ggplot2::element_blank(), 
                           panel.grid.major.y = ggplot2::element_line(color = 'grey'), 
                           axis.text = ggplot2::element_text(size = 10), 
                           axis.title = ggplot2::element_text(size = 12), 
                           legend.text = ggplot2::element_text(size = 10), 
                           strip.text = ggplot2::element_text(size = 10), 
                           legend.key = ggplot2::element_blank(), 
                           plot.margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 20, unit = 'pt')) 
            
            ggplotly(a, tooltip = c('text')) %>%
              layout(yaxis = list(title = list(text = 'y_var')),
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
          library(ggplot2)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1] 
          
          ",
                 
          single_bar,
                
          "
          
          a <- dataset_filtered %>%
              ggplot(aes(y = Number, 
                          x = factor(Variable_1, levels = c('up', 'down', 'left', 'right', 'middle')),
                          text = paste(Variable_1,
                                      ': ', 
                                      round(Number,2),
                                      ' (',
                                      round(lower_ci,1),
                                      ',',
                                      round(upper_ci,1),
                                      ')'
                                      )
                         )) +
              geom_bar(stat='identity', fill =  palette) +
              xlab('x_var') +
              geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(.9)) +
              ggplot2::theme(panel.background = ggplot2::element_blank(), 
                           legend.title = ggplot2::element_blank(), 
                           axis.ticks = ggplot2::element_blank(), 
                           panel.grid.major.y = ggplot2::element_line(color = 'grey'), 
                           axis.text = ggplot2::element_text(size = 10), 
                           axis.title = ggplot2::element_text(size = 12), 
                           legend.text = ggplot2::element_text(size = 10), 
                           strip.text = ggplot2::element_text(size = 10), 
                           legend.key = ggplot2::element_blank(), 
                           plot.margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 20, unit = 'pt')) 
              
            
            ggplotly(a, tooltip = c('text')) %>%
              layout(yaxis = list(title = list(text = 'y_var')),
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
                )"
          )
          
        } else if(length(group()) == 2 & input$select_numerical == "Rate" & input$error_bars == T) {
          paste0("
          library(plotly)
          library(ggplot2)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1:3] 
          
          ",
                 
          multi_bar,
                 
          "
          
           a <- dataset_filtered %>%
              ggplot(aes(fill = factor(Variable_2, levels = c('small', 'medium', 'large')), 
                          y = Number, 
                          x = factor(Variable_1, levels = c('up', 'down', 'left', 'right', 'middle')),
                          text = paste(Variable_1, 
                                    ' & ', 
                                    Variable_2, 
                                    ': ', 
                                    round(Number,2),
                                    ' (',
                                    round(lower_ci,1),
                                    ',',
                                    round(upper_ci,1),
                                    ')'
                                    )
                       )) +
            geom_bar(position='dodge', stat='identity') +
            xlab('x_var') +
            scale_fill_manual(values = palette) +
            geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(.9)) +
            ggplot2::theme(panel.background = ggplot2::element_blank(), 
                           legend.title = ggplot2::element_blank(), 
                           axis.ticks = ggplot2::element_blank(), 
                           panel.grid.major.y = ggplot2::element_line(color = 'grey'), 
                           axis.text = ggplot2::element_text(size = 10), 
                           axis.title = ggplot2::element_text(size = 12), 
                           legend.text = ggplot2::element_text(size = 10), 
                           strip.text = ggplot2::element_text(size = 10), 
                           legend.key = ggplot2::element_blank(), 
                           plot.margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 20, unit = 'pt')) 
             
          
          
          ggplotly(a, tooltip = c('text')) %>%
            layout(yaxis = list(title = list(text = 'y_var')), 
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
                 )"
          )
        } 
      })
      
      
      output$ggplotly_plot_out <- renderPlotly({
        start_time <- Sys.time()
        h <- ggplotly_plot_code()
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
      
      
      output$ggplotly_code_out <- renderPrint({
        cat(ggplotly_plot_code_render(),sep = "\n")
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
                tags$li(tags$p("ggplot2 code is converted to interactive plot code powered by plotly.js using ggplotly() function. ggplot2 R code is converted into a plotly object then into plotly js code")),
                tags$li(tags$p("clear code structure using principles of grammer of graphics.")),
                tags$li(tags$p("May have to use plotly functions for some elements since ggplot2 API may not describe all aspects of plot"))
              )
            ),
            "</span>"
          ))
        text
      })
      
      
      
      # Add clipboard buttons
      output$clip <- renderUI({
        # text to copy
        text <- ggplotly_plot_code_render()
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
