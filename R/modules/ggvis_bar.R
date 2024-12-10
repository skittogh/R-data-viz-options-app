GGvis_bar_ui <- function(id, label = "GGvis_bar_ui") {
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
             
             h2("GGvis Bar Chart"),
             br(),
             uiOutput(ns("note")),
             br(),
             
             fluidRow(
               tabBox(
                 width = 10,
                 id = 'secondary_box',
                 tabPanel(value = 'barplot1',
                          h4(id = "nav_title2", "View plot"),
                          
                          ggvisOutput("ggvis_plot_out"), #doesn't work in NS ??
                          uiOutput("ggvis_plot_out_ui"), #doesn't work in NS ??
                          uiOutput(ns("load_time")),
                          br(),
                          hr(),
                          br(),
                          uiOutput(ns("plot_notes"))
                 ),
                 tabPanel(
                   value = 'code',
                   h4(id = "nav_title2", "View code"),
                   verbatimTextOutput(ns("plotly_code_out"))
                 )
               )
             )
      ) 
    )
  )
}





GGvis_bar_server <- function(id, label = "GGvis_bar_server", parent) {
  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      
      output$note <- renderUI({
        HTML(paste0("<span style = 'color: red; font-size: 16px; font-weight: bold'>", 
                    "Note: The ggvis package is not fully developed and further development has been postponed. 
                    There are many features not available, so has not been fully tested here.",
                    "</span>"))
      })
      
      
      output$select_colours <- renderUI({
        req(group(), input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
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
      
   
      # ----------- GGVIS ------------------------------------------------------
      # generate  ggvis plot code - not that this does not need to be in a render... element since bind_shiny() fulfils this
      observe({
        
        start_time <- Sys.time()
        
        if(length(group()) == 1) {
          
          p <- dataset_filtered() %>%
            ggvis(
              ~dataset_filtered()[[input$primary_var]],
              ~Number) %>%
            layer_bars() %>%
            add_axis("x", title = input$primary_var) %>%
            add_tooltip(all_values, "hover") %>%
            # set_options(width = 1000, height = 600, padding = padding(10, 10, 10, 10)) %>% 
            bind_shiny("ggvis_plot_out", "ggvis_plot_out_ui")
          
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
          
          if(input$secondary_var == "Variable_2") {
            p <- dataset_filtered() %>%
              # mutate(var1_var2 = factor(paste(!!sym(input$primary_var), !!sym(input$secondary_var)))) %>%
              mutate(var1_var2 = factor(paste0(substr(!!sym(input$primary_var),1,1), substr(!!sym(input$secondary_var),1,1)))) %>%
              ggvis(
                ~var1_var2,
                ~Number,
                fill = ~Variable_2
              ) %>%
              layer_bars(stack = FALSE) %>%
              add_axis("x", title = input$primary_var, properties = axis_props(labels = list(angle = 90, align = "left"))) %>%
              bind_shiny("ggvis_plot_out", "ggvis_plot_out_ui")
          }
          
          if(input$secondary_var == "Variable_3") {
            p <- dataset_filtered() %>%
              # mutate(var1_var2 = factor(paste(!!sym(input$primary_var), !!sym(input$secondary_var)))) %>%
              mutate(var1_var2 = factor(paste0(substr(!!sym(input$primary_var),1,1), substr(!!sym(input$secondary_var),1,1)))) %>%
              ggvis(
                ~var1_var2,
                ~Number,
                fill = ~Variable_3
              ) %>%
              layer_bars(stack = FALSE) %>%
              add_axis("x", title = input$primary_var, properties = axis_props(labels = list(angle = 90, align = "left"))) %>%
              bind_shiny("ggvis_plot_out", "ggvis_plot_out_ui")
          }
        }
        
        # load time return
        end_time <- Sys.time()
        time <- round(as.numeric(as.character(end_time - start_time)) * 1000, 2)
        output$load_time <- renderUI({
          HTML(paste0("<span style = 'color: #23395d; font-size: 19px'>", 
                      "Time to generate plot: ", "<span style = 'color: #23395d; font-size: 19px; font-weight: bold;'>", as.character(time), " milliseconds",
                      "</span>", "</span>"))
        })
        
        
        p
      })
      
      # generate  ggplotly plot code for rendering code in app
      ggvis_plot_code_render <- reactive({
        req(input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        
        if(length(group()) == 1) {
          "a <- dataset %>%
            plot_ly(
                    x = ~x,
                    y = ~y,
                    type = 'bar'
            )
          ggplotly(a) %>%
              layout(legend = list(title=''))"
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
          "a <- dataset %>%
            plot_ly(x = ~x,
                    y = ~y,
                    type = 'bar',
                    color = group_var
            )
          ggplotly(a) %>%
              layout(legend = list(title=''))"
        }
      })
      
      
      # output$ggvis_plot_out <- renderGGvis({
      #   ggplotly_plot_code()
      # })
      
      
      output$ggvis_code_out <- renderPrint({
        cat(ggvis_plot_code_render(),sep = "\n")
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
                tags$li(tags$p("Graphics are rendered in a web browser, using Vega.")),
                tags$li(tags$p("Similar naming and components to ggplot2 so simple to use for ggplot2 users.")),
                tags$li(tags$p("Unfortunaly development has ceased and functionality is very limited"))
              )
            ),
            "</span>"
          ))
        text
      })
      
      
    }
  )
}
