c3_bar_ui <- function(id, label = "c3_bar_ui") {
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
             
             h2("C3 Bar Chart"),
             br(),
             
             fluidRow(
               tabBox(
                 width = 10,
                 id = 'secondary_box',
                 tabPanel(value = 'barplot1',
                          h4(id = "nav_title2", "View plot"),
                          c3Output(ns("c3_plot_out"), height = "600px") %>% withSpinner(),
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
                   verbatimTextOutput(ns("c3_code_out"))
                 )
               ),
               
             )
      ) 
    )
  )
}





c3_bar_server <- function(id, label = "c3_bar_server", parent) {
  
  
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
      
   
      
      # ----------- C3 -----------------------------------------------------  
      # generate  c3 plot code
      c3_plot_code <- reactive({
        req(palette(), input$change_colours, input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        
        # palette further refined 
        if (input$secondary_var == "none") {
          palette <- palette() #c3  still wants an array even if one variable is plotted so using palette() instead of palette()[1] here 
        } else if (input$secondary_var != "none") {
          palette <- palette()[1:nrow(unique(dataset_filtered()[2]))]
        }
        
        
        
        if(input$select_numerical == "Number" | input$error_bars == F) {
        
          if(length(group()) == 1) {
            
            dataset_filtered() %>% select(1, Number) %>% rename(Var = 1) %>% 
              c3(x = 'Var', y = 'Number' ) %>% 
              c3_bar() %>% 
              c3_color(palette)
          
          } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
            
            dataset_filtered() %>% select(1, 2, Number) %>% pivot_wider(names_from = all_of(input$secondary_var), values_from = Number) %>% 
              c3(x = input$primary_var) |> 
              c3_bar(stacked = F) %>% 
              c3_color(palette)
          }
          
          
          
          
        } else if(input$select_numerical == "Rate" & input$error_bars == T) {
        
        if(length(group()) == 1) {
          
          dataset_filtered() %>% select(1, Number) %>% rename(Var = 1) %>% 
            c3(x = 'Var', y = 'Number' ) %>% 
            c3_bar() %>% 
            c3_color(palette)
       
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
          
          dataset_filtered() %>% select(1, 2, Number) %>% pivot_wider(names_from = all_of(input$secondary_var), values_from = Number) %>% 
            c3(x = input$primary_var) |> 
            c3_bar(stacked = F) %>% 
            c3_color(palette)
          
          }
        }
        
      })
      
      
      # generate  c3 plot code for rendering code in app
      c3_plot_code_render <- reactive({
        req(input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        
        if(length(group()) == 1) {
          paste0("
          library(c3)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey() 
          
          ",
                 
          single_bar,
                 
          "
                 
          dataset_filtered %>% select(1, Number) %>% rename(Var = 1) %>% 
              c3(x = 'Var', y = 'Number' ) %>% 
              c3_bar() %>% 
              c3_color(palette)"
          )
          
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
          paste0("
          library(c3)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey() 
          
          ",
                 
          multi_bar,
                 
          "
        
          dataset_filtered %>% select(1, 2, Number) %>% pivot_wider(names_from = all_of('Variable_2'), values_from = 'Number') %>% 
              c3(x = 'Variable_1') |> 
              c3_bar(stacked = F) %>% 
              c3_color(palette)"
          )
          
        } else if(length(group()) == 1 & input$error_bars == T) {
          paste0("
          library(c3)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey() 
          
          ",
                 
          single_bar,
                 
          "
        
          dataset_filtered %>% select(1, Number) %>% rename(Var = 1) %>% 
            c3(x = 'Var', y = 'Number' ) %>% 
            c3_bar() %>% 
            c3_color(palette)"
          )
          
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var & input$error_bars == T) {
          paste0("
          library(c3)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey() 
          
          ",
                 
          multi_bar,
                 
          "
                 
          dataset_filtered %>% select(1, 2, Number) %>% pivot_wider(names_from = all_of('Variable_2'), values_from = 'Number') %>% 
            c3(x = 'Variable_1') |> 
            c3_bar(stacked = F) %>% 
            c3_color(palette)"
          )
        }
      })
      
      
      output$c3_plot_out <- renderC3({
        start_time <- Sys.time()
        h <- c3_plot_code()
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
      
      
      output$c3_code_out <- renderPrint({
        cat(c3_plot_code_render(),sep = "\n")
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
                     tags$li(tags$p("Interestingly this will not render if D3 has been rendered prior. The .JS files used for D3 must overwrite some .JS needed for these C3 plots.")),
                     tags$li(tags$p("Uses C3.js library which is built on top of D3.js like many other plotting packages.")),
                     tags$li(tags$p("The R wrapper does not provide options for error bars. These may be added with additional .js but in my opinion if more than minor javascript is needed then benifits over D3 become negligible")),
                     tags$li(tags$p("group option appears to have a bug. There may be some error in piping 'could not find function 'split_chain''. Workaround is to format data in wide format and use stacked = F")),
                     tags$li(tags$p("For most plot types, the R code required is incredibly concise."))
                   )
                ),
           "</span>"
         ))
       
       text
      })
      
      
      
      
      # Add clipboard buttons
      output$clip <- renderUI({
        # text to copy
        text <- c3_plot_code_render()
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
