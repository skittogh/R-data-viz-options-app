home_ui <- function(id, label = "home_ui") {
  ns <- NS(id)
  
  tagList(
    
    br(),
    
    fluidRow(
      column(
        width = 12,
        h2("Review of Packages for Producing Interactive Plots in R"),
      )
    ),
    
    br(),
    br(),
    column(width = 10, offset = 1,
    wellPanel(id = "homewell",
    hometext
    )),
    br(),
    br(),
    
    fluidRow(
      column(id = 'logo_col',
        width = 12,
          
        #plotly logo
        HTML("<a id='plotly_logo'
              alt='Plotly logo'
           		href='https://plotly.com/r/'>
		          <img src='plotly.png'>
              </a>"),
        
        #plotly logo
        HTML("<a id='highcharts_logo'
              alt='highcharts logo'
           		href='https://plotly.com/r/'>
		          <img src='highcharts.png'>
              </a>"),
        
        #plotly logo
        HTML("<a id='d3_logo'
              alt='d3 logo'
           		href='https://plotly.com/r/'>
		          <img src='d3.png'>
              </a>"),
        
        #plotly logo
        HTML("<a id='c3_logo'
              alt='c3 logo'
           		href='https://plotly.com/r/'>
		          <img src='c3.png'>
              </a>"),
        
        #plotly logo
        HTML("<a id='ggplot_logo'
              alt='ggplot logo'
           		href='https://plotly.com/r/'>
		          <img src='ggplot.png'>
              </a>"),
        
        #plotly logo
        HTML("<a id='echarts_logo'
              alt='echarts logo'
           		href='https://plotly.com/r/'>
		          <img src='echarts.png'>
              </a>")
        
      )
    )
    
    
    
    )
  
}


home_server <- function(id, label = "home_server", parent) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
   
      
    }
  )
}
