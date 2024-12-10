#created Aug 2023 - Steve K


# read in scripts in r/ dir
invisible(sapply(list.files('R', full.names = TRUE, recursive = TRUE), function(i) {
  source(paste0(i))
}))




ui <- {
  fluidPage(
    title = "R plotting package comparison",
    
    useShinydashboard(),
    
    includeCSS("www/style.css"),
    
    #~~~~~~ Render top banner with svg pattern
    box(id = "title_box",
        width = 12,
        status = "primary",
        h1("R PLOTTING PACKAGE COMPARISON"
        ),
        
        #pattern in header
        HTML(pattern3) #thanks to https://www.svgbackgrounds.com/
    ),
    
    
    
    tabBox(width = 12,
           id = "primary_box",
           
           # render home tab
           tabPanel(value = "home_tab", h3(id = "nav_title", "Home"),
                    home_ui(id = "home")
           ),
           
           # render key plot comparisons bar tab
           navbarMenu(
             h3(id = "nav_title", "Bar plots"),
             tabPanel(
               value = "Plotly_Bar",
               h3(id = "nav_title2", "Plotly"),
               plotly_bar_ui(id = "plotly_bar")
             ),
             tabPanel(
               value = "GGplotly_Bar",
               h3(id = "nav_title2", "ggPlotly"),
               ggplotly_bar_ui(id = "ggplotly_bar")
             ),
             tabPanel(
               value = "Highcharts_Bar",
               h3(id = "nav_title2", "Highcharts"),
               highcharts_bar_ui(id = "highcharts_bar")
             ),
             tabPanel(
               value = "c3_Bar",
               h3(id = "nav_title2", "C3"),
               c3_bar_ui(id = "c3_bar")
             ),
             tabPanel(
               value = "D3_Bar",
               h3(id = "nav_title2", "D3"),
               D3_bar_ui(id = "D3_bar")
             ),
             tabPanel(
               value = "echarts_Bar",
               h3(id = "nav_title2", "Echarts"),
               echarts_bar_ui(id = "echarts_bar")
             )
             # tabPanel(
             #   value = "GGvis_Bar",
             #   h3(id = "nav_title", "GGvis"),
             #   GGvis_bar_ui(id = "GGvis_bar")
             # )
           ),
           
           # render key plot comparisons time series tab
           navbarMenu(
             h3(id = "nav_title", "Line plots"),
             tabPanel(
               value = "Plotly_Line",
               h3(id = "nav_title2", "Plotly"),
               plotly_line_ui(id = "plotly_line")
             ),
             tabPanel(
               value = "GGplotly_Line",
               h3(id = "nav_title2", "ggPlotly"),
               ggplotly_line_ui(id = "ggplotly_line")
             ),
             tabPanel(
               value = "Highcharts_Line",
               h3(id = "nav_title2", "Highcharts"),
               highcharts_line_ui(id = "highcharts_line")
             ),
             tabPanel(
               value = "c3_Line",
               h3(id = "nav_title2", "C3"),
               c3_line_ui(id = "c3_line")
             ),
             tabPanel(
               value = "D3_Line",
               h3(id = "nav_title2", "D3"),
               D3_line_ui(id = "D3_line")
             ),
             tabPanel(
               value = "echarts_Line",
               h3(id = "nav_title2", "Echarts"),
               echarts_line_ui(id = "echarts_line")
             )
             # tabPanel(
             #   value = "GGvis_Line",
             #   h3(id = "nav_title", "GGvis"),
             #   GGvis_line_line_ui(id = "GGvis_line")
             # )
           ),
           tabPanel(
             value = "colourpicker",
             h3(id = "nav_title", "colourpicker"),
             colourpicker_ui(id = "colourpicker")
           )
    )
    
    
  )
}




server <- function(input, output, session) {
  
  
  # call all the server modules
  home_server(id = "home", parent = session)
  
  plotly_bar_server(id = "plotly_bar", parent = session)
  ggplotly_bar_server(id = "ggplotly_bar", parent = session)
  highcharts_bar_server(id = "highcharts_bar", parent = session)
  D3_bar_server(id = "D3_bar", parent = session)
  c3_bar_server(id = "c3_bar", parent = session)
  echarts_bar_server(id = "echarts_bar", parent = session)
  # GGvis_bar_server(id = "GGvis_bar", parent = session)
  
  plotly_line_server(id = "plotly_line", parent = session)
  ggplotly_line_server(id = "ggplotly_line", parent = session)
  highcharts_line_server(id = "highcharts_line", parent = session)
  D3_line_server(id = "D3_line", parent = session)
  c3_line_server(id = "c3_line", parent = session)
  echarts_line_server(id = "echarts_line", parent = session)
  
  colourpicker_server(id = "colourpicker", parent = session)
  
}


shinyApp(ui, server)







