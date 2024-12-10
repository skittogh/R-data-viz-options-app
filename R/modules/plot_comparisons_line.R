plot_comparison_line_ui <- function(id, label = "plot_comparison_line_ui") {
  ns <- NS(id)
  
  tagList(
    
    # h2("Plot Package Comparisons - Bar"),
    
    # hr(),
    # fluidRow(
    #   column(width = 8, offset = 2,
    #          wellPanel(
    #            column(width = 3, selectInput(inputId = ns("select_year"),
    #                                          label = "Select year",
    #                                          choices = select_years,
    #                                          selected = "2021")),
    #            
    #            column(width = 3,selectInput(inputId = ns("select_numerical"),
    #                                         label = "Select value to plot",
    #                                         choices = select_numerical,
    #                                         selected = "ncases")),
    # 
    #            column(width = 3,selectInput(inputId = ns("select_primary_variable"),
    #                                         label = "Select primary variable",
    #                                         choices = select_primary_var,
    #                                         selected = "agegp")),
    # 
    #            column(width = 3, uiOutput(ns("select_secondary_variable"))),
    #            br(),
    #            br(),
    #            br()
    #          )
    #   ),
    #   
    #   fluidRow(
    #     column(width = 4, highchartOutput(ns("hc_plot_out"))),
    #     
    #     column(width = 4, plotlyOutput(ns("plotly_plot_out"))),
    #     
    #     column(width = 4, plotlyOutput(ns("ggplotly_plot_out")))
    #   )
      
      
    # )
  )
}






plot_comparison_line_server <- function(id, label = "plot_comparison_line_server", parent) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      
      
      # output$select_secondary_variable <- renderUI({
      #   req(input$select_primary_variable, input$select_numerical, input$select_year)
      #   
      #   vars_remaining <- select_primary_var[select_primary_var != input$select_primary_variable] 
      #   
      #   selectInput(session$ns("secondary_var"),
      #               label = "Select secondary grouping variable" ,
      #               choices = vars_remaining
      #   )
      # })
      # 
      # 
      # # input.select_year <- "2009"
      # # input.select_primary_variable <- "agegp"
      # # input.secondary_var <- "alcgp"
      # # input.select_numerical <- "ncontrols"
      # 
      # dataset_filtered <- reactive({
      #   req(input$select_primary_variable, input$secondary_var, input$select_numerical, input$select_year)
      #   # dataset_filtered <-
      #   dataset %>%
      #     filter(year == input$select_year) %>%
      #     group_by(!!sym(input$select_primary_variable), !!sym(input$secondary_var)) %>%
      #     summarise(sum_num = sum(!!sym(input$select_numerical)),
      #               .groups = 'drop')
      # 
      # 
      #   
      # })
      # 
      # observe(
      # print(dataset_filtered())
      # )
      
      # output$hc_plot_out <- renderHighchart({
      #   dataset_filtered %>%
      #     hchart(.,
      #            type = "line",
      #            hcaes(x = name,
      #                  y = some_data))
      # })




      # output$plotly_plot_out <- renderPlotly({
      #   data %>% 
      #     plot_ly(
      #       x = ~ name,
      #       y = ~ some_data,
      #       type = "scatter",
      #       mode = 'lines+markers'
      #     ) 
      #   
      # })
      
    }
  )
}
