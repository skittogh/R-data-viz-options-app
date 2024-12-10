echarts_bar_ui <- function(id, label = "echarts_bar_ui") {
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
             
             h2("Echarts Bar Chart"),
             br(),
             
             fluidRow(
               tabBox(
                 width = 10,
                 id = 'secondary_box',
                 tabPanel(value = 'barplot1',
                          h4(id = "nav_title2", "View plot"),
                          echarts4rOutput(ns("echarts_plot_out"), height = "600px") %>% withSpinner(),
                          br(),
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
                   verbatimTextOutput(ns("echarts_code_out"))
                 )
               ),
               
             )
      ) 
    )
  )
}





echarts_bar_server <- function(id, label = "echarts_bar_server", parent) {
  
  
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
      
   
      
      # ----------- echarts -----------------------------------------------------  
      # generate  echarts plot code
      echarts_plot_code <- reactive({
        req(palette(), input$change_colours, input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        
        # palette further refined 
        if (input$secondary_var == "none") {
          palette <- palette()[1] 
        } else if (input$secondary_var != "none") {
          palette <- palette()[1:nrow(unique(dataset_filtered()[2]))]
        }
        
        
        
        if(input$select_numerical == "Number" | input$error_bars == F) {
        
          if(length(group()) == 1) {
            
            dataset_filtered() |> 
              e_charts_(input$primary_var) |> 
              e_bar(Number, name = "") |> 
              e_axis_labels(x = input$primary_var, y = "Number") |> # axis labels
              e_tooltip() |>
              e_color(palette)
            
          
          } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
            
            dataset_filtered() |> 
              group_by_(input$secondary_var) |> 
              e_charts_(input$primary_var) |> 
              e_bar(Number) |> 
              e_axis_labels(x = input$primary_var, y = input$select_numerical) |> # axis labels
              e_tooltip(trigger = "axis") |> # tooltip\
              e_legend(bottom = 0) |>  # move legend to the bottom
              e_color(palette)
            
          }
          
          
          
          
        } else if(input$select_numerical == "Rate" & input$error_bars == T) {
        
        if(length(group()) == 1) {
          
          dataset_filtered() |> 
            e_charts_(input$primary_var) |> 
            e_bar(Number, name = "") |> 
            e_axis_labels(x = input$primary_var, y = "Number") |> # axis labels 
            e_error_bar(lower_ci, upper_ci, itemStyle = list(borderWidth = 0.5)) |>
            e_color(palette) |>
            e_tooltip(trigger = "axis", formatter=htmlwidgets::JS("
             	function(x) {        // console.log(x);  // use it to see contents of 'x'
             	  let d=x[1].value; 
             	  let str=     '<b>'    +   d[0]      +        ': '     +   x[0].value[1]  +  '</b>'     +    ' ('   +     d[1]    +    ', '    +    d[2]     +     ')'; 
             	  return str;}"))
      
       
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
          
          dataset_filtered() |> 
            group_by_(input$secondary_var) |> 
            e_charts_(input$primary_var) |> 
            e_bar(Number) |> 
            e_axis_labels(x = input$primary_var, y = input$select_numerical) |> # axis labels
            e_error_bar(lower_ci, upper_ci, itemStyle = list(borderWidth = 0.5)) |>
            e_legend(bottom = 0) |>  # move legend to the bottom
            e_color(palette) |>
            e_tooltip(formatter = htmlwidgets::JS("
              function(params){
                return`  ${params.value[0]} :  ${params.value[1]}   ,    ${params.value[2]  }   `
              }"))
         
          
          }
        }
        
      })
      
      
      # generate  echarts plot code for rendering code in app
      echarts_plot_code_render <- reactive({
        req(input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        
        if(length(group()) == 1 & input$error_bars == F) {
          paste0("
          library(echarts4r)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey() 
          
          ",
                 
          single_bar,
                 
          "
        
          dataset_filtered |> 
              e_charts(Variable_1) |> 
              e_bar(Number, name = '') |> 
              e_axis_labels(x = 'x_var', y = 'y_var') |> # axis labels
              e_tooltip() |>
              e_color(palette)"
          )
          
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var & input$error_bars == F) {
          paste0("
          library(echarts4r)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey() 
          
          ",
                 
          multi_bar,
                 
          "
        
           dataset_filtered |> 
              group_by(Variable_2) |> 
              e_charts(Variable_1) |> 
              e_bar(Number) |> 
              e_axis_labels(x = 'x_var', y = 'y_var') |> # axis labels
              e_tooltip(trigger = 'axis') |> # tooltip\
              e_legend(bottom = 0) |>  # move legend to the bottom
              e_color(palette)"
          )
       
        } else if(length(group()) == 1 & input$error_bars == T) {
          paste0("
          library(echarts4r)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey() 
          
          ",
                 
          single_bar,
                 
          "
        
          dataset_filtered |> 
            e_charts(Variable_1) |> 
            e_bar(Number, name = '') |> 
            e_axis_labels(x = 'x_var', y = 'y_var') |> # axis labels 
            e_error_bar(lower_ci, upper_ci, itemStyle = list(borderWidth = 0.5)) |>
            e_color(palette) |>
            e_tooltip(trigger = 'axis', formatter=htmlwidgets::JS(\"
                 function(x) {        // console.log(x);  // use it to see contents of 'x'
                   let d=x[1].value; 
                   let str =  '<b>' + d[0] + ': ' + x[0].value[1]  + '</b>' + ' ('  + d[1] + ', ' + d[2]  + ')'; 
                   return str;}\")) "
          )
          
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var & input$error_bars == T) {
          paste0("
          library(echarts4r)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey() 
          
          ",
                 
          multi_bar,
                
          "
        
          dataset_filtered |> 
            group_by(Variable_2) |> 
            e_charts(Variable_1) |> 
            e_bar(Number) |> 
            e_axis_labels(x = 'x_var', y = 'y_var') |> # axis labels
            e_error_bar(lower_ci, upper_ci, itemStyle = list(borderWidth = 0.5)) |>
            e_legend(bottom = 0) |>  # move legend to the bottom
            e_color(palette) |>
            e_tooltip(formatter = htmlwidgets::JS(\"
          function(params){
            return`  ${params.value[0]} :  ${params.value[1]},  ${params.value[2]  }   `
          }\"))"
          )
        }
        
      })
      
      
      output$echarts_plot_out <- renderEcharts4r({
        start_time <- Sys.time()
        h <- echarts_plot_code()
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
      
      
      output$echarts_code_out <- renderPrint({
        cat(echarts_plot_code_render(),sep = "\n")
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
                     tags$li(tags$p("echarts4r package is a wrapper for the echarts.js library which is an Apache project: https://echarts.apache.org/en/index.html.")),
                     tags$li(tags$p("There is some knowledge of Javascript needed if tooltips are to be edited. Tooltips shown here need some work.")),
                     tags$li(tags$p("Error bars for grouped line plots render horizontally offset from one another. 
                                    This isn't a bad thing in my opionion as it allows for between distinction between the bars than having them overlapping. 
                                    It would be nice though to be able to match the colour of the error bars with the colour of the line/points")),
                     tags$li(tags$p("Plots can be rendered within svg or canvas for plotting large datasets.")),
                     tags$li(tags$p("A huge range of plot types can be created, as well as maps."))
                   )
                ),
           "</span>"
         ))
       
       text
      })
      
      
      # Add clipboard buttons
      output$clip <- renderUI({
        # text to copy
        text <- echarts_plot_code_render()
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
