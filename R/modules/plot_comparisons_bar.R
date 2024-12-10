plot_comparison_bar_ui <- function(id, label = "plot_comparison_bar_ui") {
  ns <- NS(id)
  
  tagList(
    
    # h2("Plot Package Comparisons - Bar"),
    
    
    fluidRow(
      br(),
      br(),
      
      
      column(width = 8, 
             # offset = 2,
             wellPanel(id = "well1",
                       column(width = 3, selectInput(inputId = ns("select_year"),
                                                     label = "Select year",
                                                     choices = select_years,
                                                     selected = "2021")),
                       
                       column(width = 3,selectInput(inputId = ns("select_numerical"),
                                                    label = "Select value to plot",
                                                    choices = select_numerical,
                                                    selected = "Variable_1")),
                       column(width = 3, uiOutput(ns("select_primary_variable"))),
                       column(width = 3, uiOutput(ns("select_secondary_variable"))),
                       br(),
                       br(),
                       br()
                       
             )
      ),
      
      column(width = 4, 
             # offset = 2,
             wellPanel(id = "well2",
                       column(width = 4, selectInput(inputId = ns("change_colours"),
                                                     label = "Change plot colours",
                                                     choices = c("red", "blue"),
                                                     selected = "red")),
                       
                       column(width = 4,checkboxInput(inputId = ns("format_plot"),
                                                      label = "Standardise plot formats")),
                       # column(width = 3,checkboxInput(inputId = ns("add_annotation"),
                       #                               label = "Add plot annotations")),
                       column(width = 4,checkboxInput(inputId = ns("facet_plot"),
                                                      label = "Facet plots w/ annotations")),
                       br(),
                       br(),
                       br()
                       
             )
      ),
      
      
      
      column(width = 12,
             br(),
             br(),
             fluidRow(
               
               tabBox(width = 4, height = 500,
                      id = 'secondary_box',
                      tabPanel(value = 'barplot1',
                               h4(id = "nav_title2", "View plot"),
                               highchartOutput(ns("hc_plot_out"))
                      ),
                      tabPanel(value = 'code',
                               h4(id = "nav_title2", "View code"),
                               verbatimTextOutput(ns("hc_code_out"))
                      )
               ),
               
               
               tabBox(width = 4, height = 500,
                      id = 'secondary_box',
                      tabPanel(id = "secondary_box",
                               value = 'barplot2',
                               h4(id = "nav_title2", "View plot"),
                               plotlyOutput(ns("plotly_plot_out"))
                      ),
                      tabPanel(id = "secondary_box",
                               value = 'code',
                               h4(id = "nav_title2", "View code"),
                               verbatimTextOutput(ns("plotly_code_out"))
                      )
               ),
               
               
               tabBox(width = 4, height = 500,
                      id = 'secondary_box',
                      tabPanel(id = "secondary_box",
                               value = 'barplot3',
                               h4(id = "nav_title2", "View plot"),
                               plotlyOutput(ns("ggplotly_plot_out"))
                      ),
                      tabPanel(id = "secondary_box",
                               value = 'code',
                               h4(id = "nav_title2", "View code"),
                               verbatimTextOutput(ns("ggplotly_code_out"))
                      )
               ),
               
               tabBox(width = 4, height = 500,
                      id = 'secondary_box',
                      tabPanel(id = "secondary_box",
                               value = 'barplot4',
                               h4(id = "nav_title2", "View plot"),
                               d3Output(ns("d3_plot_out"), width = "100%", height = "400px")
                      ),
                      tabPanel(id = "secondary_box",
                               value = 'code',
                               h4(id = "nav_title2", "View code"),
                               verbatimTextOutput(ns("d3_code_out"))
                      )
               ),
               
               tabBox(width = 4, height = 500,
                      id = 'secondary_box',
                      tabPanel(id = "secondary_box",
                               value = 'barplot5',
                               h4(id = "nav_title2", "View plot"),
                               ggvisOutput("ggvis_plot_out")
                               # uiOutput(ns("ggvis_plot_out_ui"))
                      ),
                      tabPanel(id = "secondary_box",
                               value = 'code',
                               h4(id = "nav_title2", "View code"),
                               verbatimTextOutput(ns("ggvis_code_out"))
                      )
               )
             )      
      ) 
    )
  )
}





plot_comparison_bar_server <- function(id, label = "plot_comparison_bar_server", parent) {
  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      
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
        
        if(length(group()) == 1) {
          dataset %>%
            group_by(!!sym(input$primary_var)) %>%
            summarise(Number = sum(!!sym(input$select_numerical)),
                      .groups = 'drop')
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
          dataset %>%
            group_by(!!sym(input$primary_var), !!sym(input$secondary_var)) %>%
            summarise(Number = sum(!!sym(input$select_numerical)),
                      .groups = 'drop')
        }
      })
      
      
      
      # ----------- HIGHCHARTS -------------------------------------------------
      # generate  highchart plot code
      hc_plot_code <- reactive({
        req(input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        
        if(length(group()) == 1) {
          dataset_filtered() %>%
            hchart(.,
                   type = 'column',
                   hcaes(x = !!sym(input$primary_var),
                         y = Number))
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
          dataset_filtered() %>%
            hchart(.,
                   type = 'column',
                   hcaes(x = !!sym(input$primary_var),
                         group = !!sym(input$secondary_var),
                         y = Number))
        }
      })
      
      # generate  highchart plot code for rendering code in app
      hc_plot_code_render <- reactive({
        req(input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        
        if(length(group()) == 1) {
          "dataset %>%
            hchart(.,
                   type = 'column',
                   hcaes(x = x,
                         y = x)
          )"
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
          "dataset %>%
            hchart(.,
                   type = 'column',
                   hcaes(x = x,
                         group = group_var,
                         y = y
          )"
        }
      })
      
      
      output$hc_plot_out <- renderHighchart({
        hc_plot_code()
      })
      
      
      output$hc_code_out <- renderPrint({
        cat(hc_plot_code_render(),sep = "\n")
      })
      # ------------------------------------------------------------------------
      
      
      
      
      # ----------- PLOTLY -----------------------------------------------------  
      # generate  plotly plot code
      plotly_plot_code <- reactive({
        req(input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        
        
        if(length(group()) == 1) {
          dataset_filtered() %>%
            plot_ly(
              x = dataset_filtered()[[input$primary_var]],
              y = ~Number,
              type = "bar"
            ) %>% 
            layout(xaxis = list(title = list(text = input$primary_var)))
          
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
          dataset_filtered() %>%
            plot_ly(x = dataset_filtered()[[input$primary_var]],
                    y = ~Number,
                    type = "bar",
                    color = dataset_filtered()[[input$secondary_var]]
            ) %>% 
            layout(xaxis = list(title = list(text = input$primary_var)))
        }
      })
      
      # generate  plotly plot code for rendering code in app
      plotly_plot_code_render <- reactive({
        req(input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        
        if(length(group()) == 1) {
          "dataset %>%
            plot_ly(
                    x = ~x,
                    y = ~y,
                    type = 'bar'
            ) %>% 
            layout(xaxis = list(title = list(text = 'x_var')))"
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
          "dataset %>%
            plot_ly(x = ~x,
                    y = ~y,
                    type = 'bar',
                    color = group_var
            ) %>% 
            layout(xaxis = list(title = list(text = 'x_var')))"
        }
      })
      
      
      output$plotly_plot_out <- renderPlotly({
        plotly_plot_code()
      })
      
      
      output$plotly_code_out <- renderPrint({
        cat(plotly_plot_code_render(),sep = "\n")
      })
      # ------------------------------------------------------------------------
      
      
      
      # ----------- GGPLOTLY ---------------------------------------------------  
      # generate  ggplotly plot code
      ggplotly_plot_code <- reactive({
        req(input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        
        
        if(length(group()) == 1) {
          a <- dataset_filtered() %>%
            
            ggplot(aes(y = Number, x = dataset_filtered()[[input$primary_var]],)) + 
            geom_bar(stat="identity") +
            xlab(input$primary_var) 
          ggplotly(a) %>% 
            layout(legend = list(title=''))
          
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
          a <- dataset_filtered() %>%
            
            ggplot(aes(fill = dataset_filtered()[[input$secondary_var]], y = Number, x = dataset_filtered()[[input$primary_var]],)) + 
            geom_bar(position="dodge", stat="identity") +
            xlab(input$primary_var)
          ggplotly(a) %>% 
            layout(legend = list(title=''))
          
        }
      })
      
      # generate  ggplotly plot code for rendering code in app
      ggplotly_plot_code_render <- reactive({
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
      
      
      output$ggplotly_plot_out <- renderPlotly({
        ggplotly_plot_code()
      })
      
      
      output$ggplotly_code_out <- renderPrint({
        cat(ggplotly_plot_code_render(),sep = "\n")
      })
      # ------------------------------------------------------------------------
      
      
      
      # ----------- D3 ---------------------------------------------------------  
      # generate  d3 plot code
      d3_plot_code <- reactive({
        
        req(input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        
        if(length(group()) == 1) {
          
          dataset_filtered <- dataset_filtered() %>% 
            rename(Var = 1)
          
          r2d3(data = dataset_filtered, script = "www/bar_simple.js", 
               options = list(margin = 50,
                              barPadding = 10,
                              colour = "rgba(0,0,139,1)",
                              xLabel = "x",
                              yLabel = "y",
                              chartTitle = ""))
          
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
          # dataset_filtered() %>%
          #   plot_ly(x = dataset_filtered()[[input$primary_var]],
          #           y = ~Number,
          #           type = "bar",
          #           color = dataset_filtered()[[input$secondary_var]]
          #   ) %>% 
          #   layout(xaxis = list(title = list(text = input$primary_var)))
        }
      })
      
      # generate  d3 plot code for rendering code in app
      d3_plot_code_render <- reactive({
        req(input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
        
        if(length(group()) == 1) {
          "dataset %>%
            plot_ly(
                    x = ~x,
                    y = ~y,
                    type = 'bar'
            ) %>% 
            layout(xaxis = list(title = list(text = 'x_var')))"
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
          "dataset %>%
            plot_ly(x = ~x,
                    y = ~y,
                    type = 'bar',
                    color = group_var
            ) %>% 
            layout(xaxis = list(title = list(text = 'x_var')))"
        }
      })
      
      
      output$d3_plot_out <- renderD3({
        d3_plot_code()
      })
      
      
      output$d3_code_out <- renderPrint({
        cat(d3_plot_code_render(),sep = "\n")
      })
      # ------------------------------------------------------------------------
      
      
      
      
      
      # ----------- GGVIS ------------------------------------------------------
      # generate  ggvis plot code - not that this does not need to be in a render... element since bind_shiny() forfills this
      observe(
      # output$ggvis_plot_out <- reactive({

      if(length(group()) == 1) {

        dataset_filtered() %>%
          ggvis(
            ~dataset_filtered()[[input$primary_var]],
            ~Number) %>%
          layer_bars() %>%
          add_axis("x", title = input$primary_var) %>% 
          # add_tooltip(function(x) {
          #                         if(is.null(x)) return(NULL)
          #                         paste0(names(x), ": ", format(x), collapse = "<br />")
          #                         },
          #             on = "hover") %>%
        bind_shiny("ggvis_plot_out")

      } else if(length(group()) == 2 & input$primary_var != input$secondary_var) {
        
       if(input$secondary_var == "Variable_2") {
         dataset_filtered() %>%
           # mutate(var1_var2 = factor(paste(!!sym(input$primary_var), !!sym(input$secondary_var)))) %>%
           mutate(var1_var2 = factor(paste0(substr(!!sym(input$primary_var),1,1), substr(!!sym(input$secondary_var),1,1)))) %>%
           ggvis(
             ~var1_var2,
             ~Number,
             fill = ~Variable_2
           ) %>%
           layer_bars(stack = FALSE) %>%
           add_axis("x", title = input$primary_var, properties = axis_props(labels = list(angle = 90, align = "left"))) %>% 
           bind_shiny("ggvis_plot_out")
       } 
         
        if(input$secondary_var == "Variable_3") {
          dataset_filtered() %>%
            # mutate(var1_var2 = factor(paste(!!sym(input$primary_var), !!sym(input$secondary_var)))) %>%
            mutate(var1_var2 = factor(paste0(substr(!!sym(input$primary_var),1,1), substr(!!sym(input$secondary_var),1,1)))) %>%
            ggvis(
              ~var1_var2,
              ~Number,
              fill = ~Variable_3
            ) %>%
            layer_bars(stack = FALSE) %>%
            add_axis("x", title = input$primary_var, properties = axis_props(labels = list(angle = 90, align = "left"))) %>% 
            bind_shiny("ggvis_plot_out")
        }
        
        
      }
      )

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
      # ------------------------------------------------------------------------
      
      
      
    }
  )
}
