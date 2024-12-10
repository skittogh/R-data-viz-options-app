colourpicker_ui <- function(id, label = "colourpicker_ui") {
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      br(),
      br(),
      
      column(
        width = 2,
        wellPanel(
          id = "well1",
          radioButtons(
            inputId = ns("select_man_exist"),
            label = "Select colours manually or from existing palette",
            choices = c("Manual selection", "Use existing colour palette"),
            selected = "Manual selection"
          ), 
          uiOutput(ns("rend_man_exist")),
          # selectInput(inputId = ns("number_cols"), label = "Select number of colours for palette", 
          #   choices = c(1,2,3,4,5,6,7,8,9,10,11,12),
          #   selected = 9),
          selectInput(inputId = ns("line_bar"), label = "Select plot type", 
            choices = c("Line", "Bar"),
            selected = "Line"),
        ),
        br(),
        
        wellPanel(
          id = "well2",
          radioButtons(
            inputId = ns("col_blind_opts"),
            label = "Select colour blind options",
            choices = c("Typical", "Deuteranopia", "Protanopia", "Tritanopia"),
            selected = "Typical"
          ), 
          br(),
          div(id = "col_explain",
              "The colours displayed if 'Typical' is selected are the 'true' colours. 
              The other options will display the way that a person with protanopia, 
              deuteranopia, or tritanopia could see the chosen colours, respectively")
        )
      ),
      
      
      column(width = 9, 
             
             div(id = "title_button",
                 h2("Colour picker"),
                 
                 div(id = "two_buttons",
                     rclipboardSetup(), # set up ability to copy to clipboard
                     uiOutput(ns("clip")),
                     actionBttn(inputId = ns("copy_pal"),
                                label = "Use palette")
                 )
             ),
             br(),
             
             h4("Select colours in boxes below"),
             fluidRow(
               uiOutput(ns("all_cols"))
             ),
             
             br(),
             uiOutput(ns("col_blind_cols_title")),
             fluidRow(
               uiOutput(ns("col_blind_cols"))
             ),
             
             br(),
             
             br(),
             fluidRow(
               highchartOutput(ns("hc_plot_out"), height = "600px") #%>% withSpinner()
             )
      )
      
    )
  )
}





colourpicker_server <- function(id, label = "colourpicker_server", parent) {
  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      
      
      # Add clipboard buttons
      output$clip <- renderUI({
        req(palette(), input$select_man_exist)
        rclipButton(
          inputId = session$ns("clipbtn"),
          label = "Copy to clip",
          clipText = palette(), 
          # icon = icon("clipboard"),
          tooltip = "Click to copy palette to clipboard",
          placement = "top",
          options = list(delay = list(show = 800, hide = 100), trigger = "hover")
        )
      })
      
      output$rend_man_exist <- renderUI({
        req(input$select_man_exist)
        if(input$select_man_exist == "Manual selection"){
      selectInput(inputId = session$ns("number_cols"), 
                  label = "Select number of colours for palette",
                  choices = c(1,2,3,4,5,6,7,8,9,10,11,12),
                  selected = 9)
        } else {
          selectInput(inputId = session$ns("select_palette"), 
                      label = "Select existing palette",
                      choices = names(Palette_Options),
                      selected = names(Palette_Options)[1])
        }
      }
      )
      
      
      output$all_cols <- renderUI({
        # req(input$number_cols)
        req(input$number_cols, input$select_man_exist)
        
        if(input$select_man_exist == "Manual selection"){
        lapply(1:input$number_cols, function(i) {
          output[[paste0("ui", i)]] <- renderUI({
            list(
              column(width = 1,
                     tags$div(id = "col_boxes",
                              colourInput(
                                inputId = session$ns(paste0("id", i)),
                                label = i,
                                value = "lightgrey"
                              )
                              
                     )
              )
            )
          })
        })
        } else {
          req(input$select_palette)
          lapply(1:as.numeric(Palette_Options[input$select_palette][2,]), function(i) {
            output[[paste0("ui", i)]] <- renderUI({
              list(
                column(width = 1,
                       tags$div(id = "col_boxes",
                                colourInput(
                                  inputId = session$ns(paste0("id", i)),
                                  label = i,
                                  value = eval(parse(text =   Palette_Options[names(Palette_Options) == input$select_palette][1, ]))[i]
                                )
                                
                       )
                )
              )
            })
          })
        }
        
        
      })
      
      output$col_blind_cols <- renderUI({
        req(palette(),protan_palette(), deutan_palette(),tritan_palette(), input$select_man_exist, input$col_blind_opts)
        # plot palette depending on colour blindness selection
        palette <- if(input$col_blind_opts == "Typical") {
          palette() 
        }  else if(input$col_blind_opts == "Protanopia") {
          protan_palette()
        }  else if(input$col_blind_opts == "Deuteranopia") {
          deutan_palette()
        }  else if(input$col_blind_opts == "Tritanopia") {
          tritan_palette()
        }
        
        # print(c("pal = ",palette))
        # print(c("deut = ",deutan_palette()))
        
        
        if(input$col_blind_opts != "Typical") {
          num_cols <- ifelse(input$select_man_exist == "Manual selection", input$number_cols, as.numeric(Palette_Options[input$select_palette][2,]))
          # print(num_cols)
        lapply(1:num_cols, function(i) {
          # print(palette[i])
          output[[paste0("ui2", i)]] <- renderUI({
            list(
              column(width = 1,
                     tags$div(id = "col_boxes2",
                              colourInput(
                                inputId = session$ns(paste0("idx2", i)),
                                label = i,
                                value = palette[i]
                              )
                              
                     )
              )
            )
          })
        })
      }
    })
      
      output$col_blind_cols_title <- renderUI({
        req(input$col_blind_opts)
        if(input$col_blind_opts != "Typical") {
         h4(paste0("Palette transformed to ", input$col_blind_opts)) 
        } else {NULL}
      })
      
      
      
      palette <- reactive({
        req(input$number_cols, input$select_man_exist)
        num_cols <- ifelse(input$select_man_exist == "Manual selection", input$number_cols, as.numeric(Palette_Options[input$select_palette][2,]))
        list_cols <- sapply(1:num_cols, 
                            FUN = function(x) req(input[[paste0("id", x)]])
        )
        list_cols
      })
      
      
      pal_list <- reactiveVal(list()) 
      observeEvent(input$copy_pal, {
        # print(palette())
        # print(deutan_palette())
        temp <- pal_list()
        temp[[paste0("custom_pal_", length(temp)+1)]] <- palette()
        pal_list(temp)
      })
      
      
      # colour palette transformed to estimation of what colour blind viewers may see
      deutan_palette <- reactive({
        req(palette())
        deutan(palette(), severity = 1, linear = TRUE)
      })
      protan_palette <- reactive({
        req(palette())
        protan(palette(), severity = 1, linear = TRUE)
      })
      tritan_palette <- reactive({
        req(palette())
        tritan(palette(), severity = 1, linear = TRUE)
      })
      
      
      
      observeEvent(input$copy_pal, {
        # print(pal_list())
        saveRDS(pal_list(), "pal_list.RDS")
        
        # pal_list <- readRDS("pal_list.RDS")
        pal_list <- pal_list()
        # col <- names(pal_list)
        row1 <- paste0("pal_list()$",names(pal_list))
        row2 <- lengths(pal_list)
        df <- as.data.frame(rbind(row1, row2))
        row.names(df) <- NULL
        Palette_Options2 <- cbind(Palette_Options, df)
        saveRDS(Palette_Options2, "Palette_Options.RDS")
      })
      
      
      
      
      # output$show_palette <- renderUI({
      #   req(input$number_cols)
      #   list_cols <- sapply(1:input$number_cols,
      #                       FUN=function(x) req(input[[paste0("id", x)]]))
      #   HTML(list_cols)
      # })
      # 
      
      
      
      output$hc_plot_out <- renderHighchart({
        req(palette(), input$col_blind_opts, , input$select_man_exist)
        vars <- c("Location_1", "Location_2", "Location_3", "Location_4", "Location_5", "Location_6", "Location_7", "Location_8", "Location_9", "Location_10", "Location_11", "Location_12")
        num_cols_min <- ifelse(input$select_man_exist != "Manual selection" & as.numeric(Palette_Options[input$select_palette][2,]) < 12, as.numeric(Palette_Options[input$select_palette][2,]), 12)
        num_cols <- ifelse(input$select_man_exist == "Manual selection", input$number_cols, num_cols_min)
        vars_filtered <- vars[1:num_cols]
        
        # plot palette depending on colour blindness selection
        palette <- if(input$col_blind_opts == "Typical") {
          palette()[1:num_cols]
        }  else if(input$col_blind_opts == "Protanopia") {
          protan_palette()[1:num_cols]
        }  else if(input$col_blind_opts == "Deuteranopia") {
          deutan_palette()[1:num_cols]
        }  else if(input$col_blind_opts == "Tritanopia") {
          tritan_palette()[1:num_cols]
        }
        
        
        plot <-
          
          if(input$line_bar == "Line") {
            dataset %>%
              group_by(Variable_3, year) %>%
              summarise(
                Number = sum(Rate),
                upper_ci = sum(upper_ci),
                lower_ci = sum(lower_ci),
                .groups = 'drop'
              ) %>% 
              rename (Var = 1) %>%
              filter(Var %in% vars_filtered) %>% 
              mutate(Var = factor(Var, levels = vars)) %>% 
              hchart(.,
                     type = 'line',
                     hcaes(x = year,
                           group = Var,
                           y = Number),
                     color = palette)
          } else {
            
            dataset %>%
              group_by(Variable_2, Variable_3) %>%
              summarise(Number = sum(Rate), 
                        upper_ci = sum(upper_ci), 
                        lower_ci = sum(lower_ci),
                        .groups = 'drop') %>% 
              filter(Variable_3 %in% vars_filtered) %>% 
              mutate(Variable_3 =  factor(Variable_3, levels = vars)) %>% 
              hchart(.,
                     type = 'column',
                     hcaes(x = Variable_2,
                           group = Variable_3,
                           y = Number),
                     color = palette
              ) %>% 
              hc_xAxis(categories = c("small", "medium", "large")) # factorising this doesn't work, so need to specify order of xaxis here
          }
        
        
        
        
        plot
        
      })
      
      
      
    }
  )
}
