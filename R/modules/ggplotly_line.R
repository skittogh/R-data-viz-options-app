ggplotly_line_ui <- function(id, label = "ggplotly_line_ui") {
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
             
             h2("ggplotly Line Chart"),
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





ggplotly_line_server <- function(id, label = "ggplotly_line_server", parent) {
  
  
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
      
      
      
      # ----------- GGPLOTLY -----------------------------------------------------  
      # generate  ggplotly plot code
      ggplotly_plot_code <- reactive({
        req(palette(), input$change_colours, input$primary_var, input$select_numerical)
       
        
        # palette further refined - plotly likes to have the correct number of options in the colour vector otherwise it does some odd things
        if (input$primary_var == "none") {
          palette <- palette()[1]
        } else if (input$primary_var != "none") {
          palette <- palette()[1:nrow(unique(dataset_filtered()[1]))]
        }
        
        
        
        if(input$select_numerical == "Number" | input$error_bars == F) {
          
          if(input$primary_var == "none") {
            
            a <- dataset_filtered() %>%
              
              ggplot(aes(y = Number, 
                         x = year,
                         group = 1,
                         text = paste(year,
                                      ": ", 
                                      round(Number,2)
                         )
              )) +
              geom_point(size = 2, aes(color=palette)) +
              geom_line(aes(color=palette)) +
              xlab(input$primary_var) +
              ggplot_theme 
            
            ggplotly(a, tooltip = c("text")) %>%
              layout(showlegend = FALSE, yaxis = list(title = list(text = input$select_numerical)))
            
            
          } else if(input$primary_var != "none") {
            a <- dataset_filtered() %>%
              
              ggplot(aes(y = Number, 
                         x = year,
                         group = factor(Var, levels = primary_factor_levels()),
                         color = factor(Var, levels = primary_factor_levels()),
                         text = paste(year,
                                      " & ", 
                                      Var, 
                                      ": ", 
                                      round(Number,2)
                         )
              )) +
              geom_point(size = 2) +
              geom_line() +
              scale_color_manual(values = palette) +
              xlab(input$primary_var) +
              ggplot_theme 
            
            ggplotly(a, tooltip = c("text")) %>%
              layout(legend = legend_format, yaxis = list(title = list(text = input$select_numerical)))
          }
          
        } else
          
          
          
          
          
          if(input$select_numerical == "Rate" & input$error_bars == T) {
            
            if(input$primary_var == "none") {
              
              a <- dataset_filtered() %>%
                
                ggplot(aes(y = Number, 
                           x = year,
                           group = 1,
                           text = paste(year,
                                        ": ", 
                                        round(Number,2),
                                        " (",
                                        round(lower_ci,1),
                                        ",",
                                        round(upper_ci,1),
                                        ")"
                           )
                )) +
                geom_point(size = 2, aes(color=palette)) +
                geom_line(aes(color=palette)) +
                xlab("Year") +
                ylab(input$select_numerical) +
                ggplot_theme 
              
              ggplotly(a + geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), 
                                         width = 0.2, 
                                         size = 0.5,  
                                         col = "black"), 
                       tooltip = c("text")) %>%
                layout(showlegend = FALSE)
              
              
            } else if(input$primary_var != "none") {
              a <- dataset_filtered() %>%
                
                ggplot(aes(y = Number, 
                           x = year,
                           group = factor(Var, levels = primary_factor_levels()),
                           color = factor(Var, levels = primary_factor_levels()),
                           text = paste(year,
                                        " & ", 
                                        Var,
                                        ": ", 
                                        round(Number,2),
                                        " (",
                                        round(lower_ci,1),
                                        ",",
                                        round(upper_ci,1),
                                        ")"
                           )
                )) +
                geom_point(size = 2) +
                geom_line() +
                scale_color_manual(values = palette) +
                xlab("Year") +
                ylab(input$select_numerical) +
                ggplot_theme 
              
                ggplotly(a + geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), 
                                           width = 0.2,
                                           size = 0.5, 
                                           col = "black"), 
                         tooltip = c("text")) %>% 
                layout(legend = legend_format)
              
              }
          }
        
      })
      
      
      # generate  plotly plot code for rendering code in app
      ggplotly_plot_code_render <- reactive({
        req(input$primary_var, input$select_numerical)
        
        
        if(input$primary_var == "none" & input$error_bars == F) {
          paste0("
          library(plotly)
          library(ggplot2)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1] 
          
          ",
                 
          single_line,
                 
          "
          
          a <- dataset_filtered %>%
              ggplot(aes(y = Number, 
                         x = year,
                         group = 1,
                         text = paste(year,
                                      ': ', 
                                      round(Number,2)
                         )
              )) +
              geom_point(size = 2, aes(color=palette)) +
              geom_line(aes(color=palette)) +
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
              layout(yaxis = list(title = list(text = 'y_var')), showlegend = FALSE) "
          )
          
        } else if(input$primary_var != "none" & input$error_bars == F) {
          paste0("
          library(plotly)
          library(ggplot2)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1:5] 
          
          ",
                 
          multi_line,
                
          "
          
          a <- dataset_filtered %>%
              ggplot(aes(y = Number, 
                         x = year,
                         group = factor(Var, levels = c('up', 'down', 'left', 'right', 'middle')),
                         color = factor(Var, levels = c('up', 'down', 'left', 'right', 'middle')),
                         text = paste(year,
                                      ' & ', 
                                      Var, 
                                      ': ', 
                                      round(Number,2)
                         )
              )) +
              geom_point(size = 2) +
              geom_line() +
              scale_color_manual(values = palette) +
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
                        )
            "
          )
          
        } else if(input$primary_var == "none" & input$error_bars == T) {
          paste0("
          library(plotly)
          library(ggplot2)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1] 
          
          ",
                 
          single_line,
              
          "
          
           a <- dataset_filtered %>%
                ggplot(aes(y = Number, 
                           x = year,
                           group = 1,
                           text = paste(year,
                                        ': ', 
                                        round(Number,2),
                                        ' (',
                                        round(lower_ci,1),
                                        ',',
                                        round(upper_ci,1),
                                        ')'
                           )
                )) +
                geom_point(size = 2, aes(color=palette)) +
                geom_line(aes(color=palette)) +
                xlab('Year') +
                ylab('y_var') +
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
              
              ggplotly(a + geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), 
                                         width = 0.2, 
                                         size = 0.5,  
                                         col = 'black'), 
                       tooltip = c('text')) %>%
                layout(showlegend = FALSE)"
          )
          
        } else if(input$primary_var != "none" & input$error_bars == T) {
          paste0("
          library(plotly)
          library(ggplot2)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1:5] 
          
          ",
                 
                 multi_line,
                 
                 "
          
          a <- dataset_filtered %>%
              ggplot(aes(y = Number, 
                           x = year,
                           group = factor(Var, levels = c('up', 'down', 'left', 'right', 'middle')),
                           color = factor(Var, levels = c('up', 'down', 'left', 'right', 'middle')),
                           text = paste(year,
                                        ' & ', 
                                        Var,
                                        ': ', 
                                        round(Number,2),
                                        ' (',
                                        round(lower_ci,1),
                                        ',',
                                        round(upper_ci,1),
                                        ')'
                           )
                )) +
                geom_point(size = 2) +
                geom_line() +
                scale_color_manual(values = palette) +
                xlab('Year') +
                ylab('y_var') +
                ggplot_theme 
              
                ggplotly(a + geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), 
                                           width = 0.2,
                                           size = 0.5, 
                                           col = 'black'), 
                         tooltip = c('text')) %>% 
                layout(legend = list(
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
          
        } else {NULL}
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
