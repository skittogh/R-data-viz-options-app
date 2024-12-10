D3_line_ui <- function(id, label = "D3_line_ui") {
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
             
             h2("D3 Line Chart"),
             br(),
             
             fluidRow(
               tabBox(
                 width = 10,
                 id = 'secondary_box',
                 tabPanel(value = 'plot1',
                          h4(id = "nav_title2", "View plot"),
                          uiOutput(ns("uioutput")),
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
                   verbatimTextOutput(ns("d3_code_out"))
                 )
               )
             )
      ) 
    )
  )
}





D3_line_server <- function(id, label = "D3_line_server", parent) {
  
  
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
      
      show_error_yn <- reactive({
        yn <- if(input$error_bars == FALSE | input$select_numerical == "Number") {
          "NARP"} else {"YARP"} 
        yn
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
        
        # not the best way to do it, but the upper y axis limit is set by the highest upper_ci value of error bars. 
        # if Number is selected then there are no error bars so have to give the plot something to scale to on the y axis
        if(input$select_numerical == "Number"){
        dataset <- dataset %>% 
          mutate(upper_ci = max(Number)*1.1)
        } else {dataset}
          
        dataset
      })
      
      
      
      # ----------- D3 ---------------------------------------------------------
      output$uioutput <- renderUI({
        
        req(dataset_filtered(), !is.null(input$error_bars))
        
        # to return load_time later in script
        start_time <- Sys.time()
        
        # palette further refined - D3 likes to have the correct number of options in the colour vector
        if (input$primary_var == "none") {
        palette <- palette()[1]
        } else if (input$primary_var != "none") {
          palette <- palette()[1:length(unique(dataset_filtered()$Var))]
        }
        
        if(input$primary_var == "none" & show_error_yn() == "NARP") {
          data <- dataset_filtered() 
          output$d3_line_plot_out1 <- renderD3({
            r2d3(data = data, script = "www/line.js",
                 options = list(margin = 70,
                                colour = palette,
                                xLabel = "Year",
                                yLabel = input$select_numerical,
                                chartTitle = ""))
            
          })
          
        } else if(input$primary_var != "none" & show_error_yn() == "NARP") {
          data <- dataset_filtered() 
          output$d3_line_plot_out2 <- renderD3({
            r2d3(data = data, script = "www/multi_line.js",
                 options = list(margin = 70,
                                colour = palette,
                                xLabel = input$primary_var,
                                yLabel = input$select_numerical,
                                chartTitle = ""))
          })
          
        } else if(input$primary_var == "none" & show_error_yn() == "YARP") {
          data <- dataset_filtered() 
          output$d3_line_plot_out3 <- renderD3({
            r2d3(data = data, script = "www/line_with_error.js",
                 options = list(margin = 70,
                                colour = palette,
                                xLabel = "Year",
                                yLabel = input$select_numerical,
                                chartTitle = ""))
            
          })
          
        } else  if(input$primary_var != "none" & show_error_yn() == "YARP") {
          data <- dataset_filtered()
          output$d3_line_plot_out4 <- renderD3({
            r2d3(data = data, script = "www/multi_line_with_error.js",
                 options = list(margin = 70,
                                colour = palette,
                                xLabel = input$primary_var,
                                yLabel = input$select_numerical,
                                chartTitle = ""))
          })
          
        } else {NULL}
        
        
        # load time return
        end_time <- Sys.time()
        time <- round(as.numeric(as.character(end_time - start_time)) * 1000, 2)
        output$load_time <- renderUI({
          HTML(paste0("<span style = 'color: #23395d; font-size: 19px'>", 
            "Time to generate plot: ", "<span style = 'color: #23395d; font-size: 19px; font-weight: bold;'>", as.character(time), " milliseconds",
            "</span>", "</span>"))
        })
        
        
        # render output depending on selection 
        if (input$primary_var == "none" & show_error_yn() == "NARP") {
          d3Output(session$ns("d3_line_plot_out1"), height = "600px") %>% withSpinner(hide.ui = FALSE)
        } else if (input$primary_var != "none" &
                   show_error_yn() == "NARP") {
          d3Output(session$ns("d3_line_plot_out2"), height = "600px") %>% withSpinner(hide.ui = FALSE)
        } else if (input$primary_var == "none" &
                   show_error_yn() == "YARP") {
          d3Output(session$ns("d3_line_plot_out3"), height = "600px") %>% withSpinner(hide.ui = FALSE)
        } else  if (input$primary_var != "none" &
                    show_error_yn() == "YARP") {
          d3Output(session$ns("d3_line_plot_out4"), height = "600px") %>% withSpinner(hide.ui = FALSE)
        } else {
          NULL
        }
        
      })
      
      
      
      # generate  d3 plot code for rendering code in app
      d3_plot_code_render <- reactive({
        req(input$primary_var, input$select_numerical)
        
        if(input$primary_var == "none" & show_error_yn() == "NARP") {
          paste0("
          library(r2d3)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1] 
          
          ",
                 
                 single_line,
                 
                 
                 "
          
          #create js file for r2d3 to read",
                 "
          js <- HTML(
              \"
             // Remove some elements so that the refreshed plot doesn't render overtop of old
              svg.selectAll('circle').remove();
              svg.selectAll('lines').remove();
              svg.selectAll('text').remove();
              svg.selectAll('g').remove();
              svg.selectAll('rect').remove();
              svg.selectAll('error-bar').remove();
              svg.selectAll('path').remove();
              svg.selectAll().remove();
              
              // Set some initial values
              var margin = options.margin,
                  width = width - (2 * margin),
                  height = height - (2 * margin),
                  ymax = d3.max(data, function(d) { return d.upper_ci; });
              
              // Format the years on the x-axis 
              var parseYear = d3.timeParse('%Y');
              
              var x = d3.scaleTime()
                  .domain(d3.extent(data, function(d) { return parseYear(d.year); }))
                  .range([margin, margin + width]);
              
              svg.append('g')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '12pt')
                  .attr('transform', 'translate(0,' + (height + margin) + ')')
                  .call(d3.axisBottom(x).ticks(d3.timeYear).tickFormat(d3.timeFormat('%Y')));
              
              svg.append('text')
                  .attr('transform', 'translate(' + (width / 2 + margin - 20) + ' ,' + (height + 2 * margin) + ')')
                  .attr('dx', '1em')
                  .style('text-anchor', 'middle')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '16pt')
                  .text(options.xLabel);
              
              // Create the y axis
              var y = d3.scaleLinear()
                  .range([height, 0])
                  .domain([0, ymax]);
              
              svg.append('g')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '12pt')
                  .attr('transform', 'translate(' + margin + ', ' + margin + ')')
                  .call(d3.axisLeft(y).ticks(10,'s')); // added .ticks(10,'s') to format as 10k instead of 10,000
                  
              
              svg.append('text')
                  .attr('transform', 'translate(' + 0 + ' ,' + ((height + 2 * margin) / 2) + ') rotate(-90)')
                  .attr('dy', '1em')
                  .style('text-anchor', 'middle')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '16pt')
                  .text(options.yLabel);
              
              // Create the chart title
              svg.append('text')
                  .attr('x', width / 2 + margin)
                  .attr('y', margin / 2)
                  .attr('text-anchor', 'middle')
                  .attr('dx', '1em')
                  .style('font-size', '18pt')
                  .style('font-family', 'sans-serif')
                  .text(options.chartTitle);
              
              // Create the line
              svg.append('path')
                  .datum(data)
                  .attr('fill', 'none')
                  .attr('stroke', options.colour)
                  .attr('stroke-width', 2)
                  .attr('d', d3.line()
                      .x(function(d) { return x(parseYear(d.year)); })
                      .y(function(d) { return y(d.Number) + margin; })
                  )
                  .style('opacity', 0) // set initial opacity of 0
                  .transition()
                  .delay(function (d, i) { return i * 50; })
                  .duration(function (d, i) { return 1000 + (i * 10); })
                   .attr('d', d3.line()
                      .x(function(d) { return x(parseYear(d.year)); })
                      .y(function(d) { return y(d.Number) + margin; })
                  )
                  .style('opacity', 1); // transition to final opacity of 1;
              
              // Create circles for scatter points
              svg.selectAll('circle')
                  .data(data)
                  .enter()
                  .append('circle')
                  .attr('cx', function(d) { return x(parseYear(d.year)); })
                  .attr('cy', function(d) { return y(d.Number) + margin; })
                  .attr('r', 0)  // Starting with a radius of 0 for a smooth transition
                  .attr('fill', options.colour)
                  .transition()
                  .delay(function(d, i) { return i * 100; })
                  .duration(500)
                  .attr('r', 5);  // Final radius
              
              
              // Create a tooltip
              var Tooltip = d3.select('body')
                  .append('div')
                  .attr('class', 'tooltip')
                  .style('position', 'absolute')
                  .style('background-color', 'rgba(255,255,255,0.8)')
                  .style('border-radius', '5px')
                  .style('padding', '5px')
                  .style('z-index', '100')
                  .style('opacity', 0)
                  .style('font-family', 'Tahoma, Geneva, sans-serif')
                  .style('font-size', '12pt');
              
              // Mouseover effects for tooltip
              var mouseover = function(d) {
                  Tooltip
                      .style('opacity', 1)
                      .style('box-shadow', '5px 5px 5px rgba(0,0,0,0.2)');
              };
              
              var mousemove = function(event, d) {
                  Tooltip
                      .html(d.year + ': ' + Math.round(d.Number * 10) / 10)
                      .style('left', (event.pageX + 30) + 'px')
                      .style('top', (event.pageY) + 'px');
              };
              
              var mouseleave = function(d) {
                  Tooltip
                      .style('opacity', 0);
              };
              
              svg.selectAll('circle')
                  .on('mouseover', mouseover)
                  .on('mousemove', mousemove)
                  .on('mouseleave', mouseleave);
              \"
              )
              
              ",
                 
                 "
          file.create('plot.js')
          
          ",
                 
                 "
          write(js, file = 'plot.js')
          
          ",
                 
                 
                 "
          
          #create d3 plot",
                 "
          data <- dataset_filtered 
            r2d3(data = data, script = 'plot.js',
                 options = list(margin = 70,
                                colour = palette,
                                xLabel = 'Year',
                                yLabel = 'y_var',
                                chartTitle = ''))"
          )
          
        } else if(input$primary_var != "none" & show_error_yn() == "NARP") {
          paste0("
          library(r2d3)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1:5] 
          
          ",
                 
                 multi_line,
                 
                 
                 "
          
          #create js file for r2d3 to read",
                 "
          js <- HTML(
              \"
              // Remove some elements so that the refreshed plot doesn't render overtop of old
              svg.selectAll('circle').remove();
              svg.selectAll('lines').remove();
              svg.selectAll('text').remove();
              svg.selectAll('g').remove();
              svg.selectAll('rect').remove();
              svg.selectAll('error-bar').remove();
              svg.selectAll('path').remove();
              svg.selectAll().remove();
              
              // Set some initial values
              var margin = options.margin,
                  width = width - (2 * margin),
                  height = height - (2 * margin) -30, 
                  ymax = d3.max(data, function(d) { return d.upper_ci; })
                  groupNames = [...new Set(data.map(d => d.Var))],
                  colour = d3.scaleOrdinal()
                      .range(options.colour);;
              
              // Format the years on the x-axis 
              var parseYear = d3.timeParse('%Y');
              
              var x = d3.scaleTime()
                  .domain(d3.extent(data, function(d) { return parseYear(d.year); }))
                  .range([margin, margin + width]);
              
              svg.append('g')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '12pt')
                  .attr('transform', 'translate(0,' + (height + margin) + ')')
                  .call(d3.axisBottom(x).ticks(d3.timeYear).tickFormat(d3.timeFormat('%Y')));
              
              svg.append('text')
                  .attr('transform', 'translate(' + (width / 2 + margin - 20) + ' ,' + (height + 2 * margin - 15) + ')') //
                  .attr('dx', '1em')
                  .style('text-anchor', 'middle')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '16pt')
                  .text(options.xLabel);
              
              // Create the y axis
              var y = d3.scaleLinear()
                  .range([height, 0])
                  .domain([0, ymax]);
              
              svg.append('g')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '12pt')
                  .attr('transform', 'translate(' + margin + ', ' + margin + ')')
                  .call(d3.axisLeft(y).ticks(10,'s')); // added .ticks(10,'s') to format as 10k instead of 10,000
                  
              
              svg.append('text')
                  .attr('transform', 'translate(' + 0 + ' ,' + ((height + 2 * margin) / 2) + ') rotate(-90)')
                  .attr('dy', '1em')
                  .style('text-anchor', 'middle')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '16pt')
                  .text(options.yLabel);
              
              // Create the chart title
              svg.append('text')
                  .attr('x', width / 2 + margin)
                  .attr('y', margin / 2)
                  .attr('text-anchor', 'middle')
                  .attr('dx', '1em')
                  .style('font-size', '18pt')
                  .style('font-family', 'sans-serif')
                  .text(options.chartTitle);
              
              // Create a function to group data by the 'Group' property
              var groupedData = d3.group(data, d => d.Var);
              
              // Create multiple lines for each group
              var line = d3.line()
                  .x(function(d) { return x(parseYear(d.year)); })
                  .y(function(d) { return y(d.Number) + margin; });
              
              svg.selectAll('.line')
                  .data(groupedData)
                  .enter()
                  .append('path')
                  .attr('class', 'line')
                  .transition()
                  .delay(function (d, i) { return i * 50; })
                  .duration(function (d, i) { return 1000 + (i * 10); })
                  .attr('d', function(d) { return line(Array.from(d[1])); })
                  .attr('fill', 'none')
                  .style('stroke', colour)
                  .attr('stroke-width', 2);
              
              // Create circles for scatter points with transition
              svg.selectAll('circle')
                  .data(data)
                  .enter()
                  .append('circle')
                  .attr('cx', function(d) { return x(parseYear(d.year)); })
                  .attr('cy', function(d) { return y(d.Number) + margin; })
                  .style('fill', d => colour(d.Var))
                  .style('opacity', 0) // set initial opacity of 0
                  .attr('r', 0) // set initial radius of 0
                   .transition()
                  .delay(function (d, i) { return i * 0; }) // delay between group rendering - set here to 0
                  .duration(function (d, i) { return 1000 + (i * 10); })
                  .style('opacity', 1) // set opacity of 1 after transition period
                  .attr('r', 5) // set radius of 5 after transition period
                  .attr('class', 'pltcircle'); //
                  
              // Create a legend
              var n = data.length / 2;
              var legend = svg.selectAll('.legend')
                  .data(groupNames)
                  .enter().append('g')
                  .attr('class', 'legend')
                  .attr('transform', function (d, i) { return 'translate(' + i % n * 95 + ',' + Math.floor(i / n) * 100 + ')'; }); //
              
              legend.append('circle') 
                  .attr('cx', margin + 60) 
                  .attr('r', 5) 
                  .style('fill', colour)
                  .attr('cy', height + margin + 80); 
              
              legend.append('text')
                  .attr('x', margin - 6 + 60) 
                  .attr('y', height + margin + 80) 
                  .attr('dy', '.35em')
                  .style('text-anchor', 'end')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '10pt') 
                  .text(function (d) { return d; });
              
              // Create a tooltip
              var Tooltip = d3.select('body')
                  .append('div')
                  .attr('class', 'tooltip')
                  .style('position', 'absolute')
                  .style('background-color', 'rgba(255,255,255,0.8)')
                  .style('border-radius', '5px')
                  .style('padding', '5px')
                  .style('z-index', '100')
                  .style('opacity', 0)
                  .style('font-family', 'Tahoma, Geneva, sans-serif')
                  .style('font-size', '12pt');
              
              // Mouseover effects for tooltip
              var mouseover = function(d) {
                  Tooltip
                      .style('opacity', 1)
                      .style('box-shadow', '5px 5px 5px rgba(0,0,0,0.2)');
              };
              
              var mousemove = function(event, d) {
                  Tooltip
                      .html(d.year + ': ' + Math.round(d.Number * 10) / 10)
                      .style('left', (event.pageX + 30) + 'px')
                      .style('top', (event.pageY) + 'px');
              };
              
              var mouseleave = function(d) {
                  Tooltip
                      .style('opacity', 0);
              };
              
              svg.selectAll('circle') 
                  .on('mouseover', mouseover)
                  .on('mousemove', mousemove)
                  .on('mouseleave', mouseleave);
              \"
              )
              
              ",
                 
                 "
          file.create('plot.js')
          
          ",
                 
                 "
          write(js, file = 'plot.js')
          
          ",
                 
                 
                 "
          #create d3 plot",
                 "
          data <- dataset_filtered 
            r2d3(data = data, script = 'plot.js',
                 options = list(margin = 70,
                                colour = palette,
                                xLabel = 'x_var',
                                yLabel = 'y_var',
                                chartTitle = ''))"
          )
        } else if(input$primary_var == "none" & show_error_yn() == "YARP") {
          
          paste0("
          library(r2d3)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1] 
          
          ",
                 
                 single_line,
                 
                 
                 "
          
          #create js file for r2d3 to read",
                 "
          js <- HTML(
              \"
              // Remove some elements so that the refreshed plot doesn't render overtop of old
              svg.selectAll('circle').remove();
              svg.selectAll('lines').remove();
              svg.selectAll('text').remove();
              svg.selectAll('g').remove();
              svg.selectAll('rect').remove();
              svg.selectAll('error-bar').remove();
              svg.selectAll('path').remove();
              svg.selectAll().remove();
              
              // Set some initial values
              var margin = options.margin,
                  width = width - (2 * margin),
                  height = height - (2 * margin),
                  /*ymax = d3.max(data, function(d) { return d.Number; });*/
                  ymax = d3.max(data, function(d) { return d.upper_ci; });
              
              // Format the years on the x-axis 
              var parseYear = d3.timeParse('%Y');
              
              var x = d3.scaleTime()
                  .domain(d3.extent(data, function(d) { return parseYear(d.year); }))
                  .range([margin, margin + width]);
              
              svg.append('g')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '12pt')
                  .attr('transform', 'translate(0,' + (height + margin) + ')')
                  .call(d3.axisBottom(x).ticks(d3.timeYear).tickFormat(d3.timeFormat('%Y')));
              
              svg.append('text')
                  .attr('transform', 'translate(' + (width / 2 + margin - 20) + ' ,' + (height + 2 * margin) + ')')
                  .attr('dx', '1em')
                  .style('text-anchor', 'middle')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '16pt')
                  .text(options.xLabel);
              
              // Create the y axis
              var y = d3.scaleLinear()
                  .range([height, 0])
                  .domain([0, ymax]);
              
              svg.append('g')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '12pt')
                  .attr('transform', 'translate(' + margin + ', ' + margin + ')')
                  .call(d3.axisLeft(y).ticks(10,'s')); // added .ticks(10,'s') to format as 10k instead of 10,000
                  
              
              svg.append('text')
                  .attr('transform', 'translate(' + 0 + ' ,' + ((height + 2 * margin) / 2) + ') rotate(-90)')
                  .attr('dy', '1em')
                  .style('text-anchor', 'middle')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '16pt')
                  .text(options.yLabel);
              
              // Create the chart title
              svg.append('text')
                  .attr('x', width / 2 + margin)
                  .attr('y', margin / 2)
                  .attr('text-anchor', 'middle')
                  .attr('dx', '1em')
                  .style('font-size', '18pt')
                  .style('font-family', 'sans-serif')
                  .text(options.chartTitle);
              
              // Create the line
              svg.append('path')
                  .datum(data)
                  .attr('fill', 'none')
                  .attr('stroke', options.colour)
                  .attr('stroke-width', 2)
                  .attr('d', d3.line()
                      .x(function(d) { return x(parseYear(d.year)); })
                      .y(function(d) { return y(d.Number) + margin; })
                  )
                  .style('opacity', 0) // set initial opacity of 0
                  .transition()
                  .delay(function (d, i) { return i * 50; })
                  .duration(function (d, i) { return 1000 + (i * 10); })
                   .attr('d', d3.line()
                      .x(function(d) { return x(parseYear(d.year)); })
                      .y(function(d) { return y(d.Number) + margin; })
                  )
                  .style('opacity', 1); // transition to final opacity of 1;
              
              // Create circles for scatter points
              svg.selectAll('circle')
                  .data(data)
                  .enter()
                  .append('circle')
                  .attr('cx', function(d) { return x(parseYear(d.year)); })
                  .attr('cy', function(d) { return y(d.Number) + margin; })
                  .attr('r', 0)  // Starting with a radius of 0 for a smooth transition
                  .attr('fill', options.colour)
                  .transition()
                  .delay(function(d, i) { return i * 100; })
                  .duration(500)
                  .attr('r', 5);  // Final radius
              
              // Create error bars
              svg.selectAll('.error-bar')
                  .data(data)
                  .enter()
                  .append('line')
                  .attr('class', 'error-bar')
                  .attr('x1', function(d) { return x(parseYear(d.year)); }) // x of iupper end of error bar
                  .attr('y1', function(d) { return y(d.upper_ci) + margin; }) // y of Upper end of error bar
                  .attr('x2', function(d) { return x(parseYear(d.year)); }) // x of lower end of error bar
                  .attr('y2', function(d) { return y(d.lower_ci) + margin; }) // Lower end of error bar
                  .attr('stroke', 'black')
                  .attr('stroke-width', 1)
                  .style('opacity', 0) // set initial opacity of 0
                  .transition()
                  .delay(function (d, i) { return i * 50; })
                  .duration(function (d, i) { return 1000 + (i * 10); })
                  .attr('x1', function(d) { return x(parseYear(d.year)); })
                  .attr('y1', function(d) { return y(d.upper_ci) + margin; }) // Upper end of error bar
                  .attr('x2', function(d) { return x(parseYear(d.year)); })
                  .attr('y2', function(d) { return y(d.lower_ci) + margin; }) // Lower end of error bar
                  .style('opacity', 0.6); // transition to final opacity of 1
              
              // Add error bar caps
              svg.selectAll('.error-bar-cap-upper')
                  .data(data)
                  .enter()
                  .append('line')
                  .attr('class', 'error-bar-cap-upper')
                  .attr('x1', function(d) { return x(parseYear(d.year)) - 5; }) // adjust the cap position as needed
                  .attr('y1', function(d) { return y(d.upper_ci) + margin; })
                  .attr('x2', function(d) { return x(parseYear(d.year)) + 5; }) // adjust the cap position as needed
                  .attr('y2', function(d) { return y(d.upper_ci) + margin; })
                  .attr('stroke', 'black')
                  .attr('stroke-width', 1)
                  .style('opacity', 0) // set initial opacity of 0
                  .transition()
                  .delay(function (d, i) { return i * 50; })
                  .duration(function (d, i) { return 1000 + (i * 10); })
                 .attr('x1', function(d) { return x(parseYear(d.year)) - 5; }) // adjust the cap position as needed
                  .attr('y1', function(d) { return y(d.upper_ci) + margin; })
                  .attr('x2', function(d) { return x(parseYear(d.year)) + 5; }) // adjust the cap position as needed
                  .attr('y2', function(d) { return y(d.upper_ci) + margin; })
                  .style('opacity', 0.6); // transition to final opacity of 1
              
              svg.selectAll('.error-bar-cap-lower')
                  .data(data)
                  .enter()
                  .append('line')
                  .attr('class', 'error-bar-cap-lower')
                  .attr('x1', function(d) { return x(parseYear(d.year)) - 5; }) // adjust the cap position as needed
                  .attr('y1', function(d) { return y(d.lower_ci) + margin; })
                  .attr('x2', function(d) { return x(parseYear(d.year)) + 5; }) // adjust the cap position as needed
                  .attr('y2', function(d) { return y(d.lower_ci) + margin; })
                  .attr('stroke', 'black')
                  .attr('stroke-width', 1)
                  .style('opacity', 0) // set initial opacity of 0
                  .transition()
                  .delay(function (d, i) { return i * 50; })
                  .duration(function (d, i) { return 1000 + (i * 10); })
                  .attr('x1', function(d) { return x(parseYear(d.year)) - 5; }) // adjust the cap position as needed
                  .attr('y1', function(d) { return y(d.lower_ci) + margin; })
                  .attr('x2', function(d) { return x(parseYear(d.year)) + 5; }) // adjust the cap position as needed
                  .attr('y2', function(d) { return y(d.lower_ci) + margin; })
                  .style('opacity', 0.6); // transition to final opacity of 1
              
              // Create a tooltip
              var Tooltip = d3.select('body')
                  .append('div')
                  .attr('class', 'tooltip')
                  .style('position', 'absolute')
                  .style('background-color', 'rgba(255,255,255,0.8)')
                  .style('border-radius', '5px')
                  .style('padding', '5px')
                  .style('z-index', '100')
                  .style('opacity', 0)
                  .style('font-family', 'Tahoma, Geneva, sans-serif')
                  .style('font-size', '12pt');
              
              // Mouseover effects for tooltip
              var mouseover = function(d) {
                  Tooltip
                      .style('opacity', 1)
                      .style('box-shadow', '5px 5px 5px rgba(0,0,0,0.2)');
              };
              
              var mousemove = function(event, d) {
                  Tooltip
                      .html(d.year + ': ' + Math.round(d.Number * 10) / 10 + ' (' + Math.round(d.lower_ci * 10) / 10 + ', ' + Math.round(d.upper_ci * 10) / 10 + ')')
                      .style('left', (event.pageX + 30) + 'px')
                      .style('top', (event.pageY) + 'px');
              };
              
              var mouseleave = function(d) {
                  Tooltip
                      .style('opacity', 0);
              };
              
              svg.selectAll('circle')
                  .on('mouseover', mouseover)
                  .on('mousemove', mousemove)
                  .on('mouseleave', mouseleave);
              \"
              )
              
              ",
                 
                 "
          file.create('plot.js')
          
          ",
                 
                 "
          write(js, file = 'plot.js')
          
          ",
                 
                 
                 "
          
          #create d3 plot",
                 "
           data <- dataset_filtered 
            r2d3(data = data, script = 'plot.js',
                 options = list(margin = 70,
                                colour = palette,
                                xLabel = 'Year',
                                yLabel = 'y_var',
                                chartTitle = ''))"
          )
          
        } else if(input$primary_var != "none" & show_error_yn() == "YARP") {
          paste0("
          library(r2d3)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey()[1:5] 
          
          ",
                 
                 multi_line,
                 
                 
                 "
          
          #create js file for r2d3 to read",
                 "
          js <- HTML(
              \"
              // Remove some elements so that the refreshed plot doesn't render overtop of old
              svg.selectAll('circle').remove();
              svg.selectAll('lines').remove();
              svg.selectAll('text').remove();
              svg.selectAll('g').remove();
              svg.selectAll('rect').remove();
              svg.selectAll('error-bar').remove();
              svg.selectAll('path').remove();
              svg.selectAll().remove();
              
              // Set some initial values
              var margin = options.margin,
                  width = width - (2 * margin),
                  height = height - (2 * margin) -30, //
                  ymax = d3.max(data, function(d) { return d.upper_ci; })
                  groupNames = [...new Set(data.map(d => d.Var))],
                  colour = d3.scaleOrdinal()
                      .range(options.colour);;
              
              // Format the years on the x-axis 
              var parseYear = d3.timeParse('%Y');
              
              var x = d3.scaleTime()
                  .domain(d3.extent(data, function(d) { return parseYear(d.year); }))
                  .range([margin, margin + width]);
              
              svg.append('g')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '12pt')
                  .attr('transform', 'translate(0,' + (height + margin) + ')')
                  .call(d3.axisBottom(x).ticks(d3.timeYear).tickFormat(d3.timeFormat('%Y')));
              
              svg.append('text')
                  .attr('transform', 'translate(' + (width / 2 + margin - 20) + ' ,' + (height + 2 * margin - 15) + ')') //
                  .attr('dx', '1em')
                  .style('text-anchor', 'middle')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '16pt')
                  .text(options.xLabel);
              
              // Create the y axis
              var y = d3.scaleLinear()
                  .range([height, 0])
                  .domain([0, ymax]);
              
              svg.append('g')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '12pt')
                  .attr('transform', 'translate(' + margin + ', ' + margin + ')')
                  .call(d3.axisLeft(y).ticks(10,'s')); // added .ticks(10,'s') to format as 10k instead of 10,000
                  
              svg.append('text')
                  .attr('transform', 'translate(' + 0 + ' ,' + ((height + 2 * margin) / 2) + ') rotate(-90)')
                  .attr('dy', '1em')
                  .style('text-anchor', 'middle')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '16pt')
                  .text(options.yLabel);
              
              // Create the chart title
              svg.append('text')
                  .attr('x', width / 2 + margin)
                  .attr('y', margin / 2)
                  .attr('text-anchor', 'middle')
                  .attr('dx', '1em')
                  .style('font-size', '18pt')
                  .style('font-family', 'sans-serif')
                  .text(options.chartTitle);
              
              // Create a function to group data by the 'Group' property
              var groupedData = d3.group(data, d => d.Var);
              
              // Create multiple lines for each group
              var line = d3.line()
                  .x(function(d) { return x(parseYear(d.year)); })
                  .y(function(d) { return y(d.Number) + margin; });
              
              svg.selectAll('.line')
                  .data(groupedData)
                  .enter()
                  .append('path')
                  .attr('class', 'line')
                  
                  .transition()
                  .delay(function (d, i) { return i * 50; })
                  .duration(function (d, i) { return 1000 + (i * 10); })
                  
                  .attr('d', function(d) { return line(Array.from(d[1])); })
                  .attr('fill', 'none')
                  .style('stroke', colour)
                  .attr('stroke-width', 2);
              
              // Create circles for scatter points with transition
              svg.selectAll('circle')
                  .data(data)
                  .enter()
                  .append('circle')
                  .attr('cx', function(d) { return x(parseYear(d.year)); })
                  .attr('cy', function(d) { return y(d.Number) + margin; })
                  .style('fill', d => colour(d.Var))
                  .style('opacity', 0) // set initial opacity of 0
                  .attr('r', 0) // set initial radius of 0
                   .transition()
                  .delay(function (d, i) { return i * 0; }) // delay between group rendering - set here to 0
                  .duration(function (d, i) { return 1000 + (i * 10); })
                  .style('opacity', 1) // set opacity of 1 after transition period
                  .attr('r', 5); // set radius of 5 after transition period
                  
              // Create error bars
              svg.selectAll('.error-bar')
                  .data(data)
                  .enter()
                  .append('line')
                  .attr('class', 'error-bar')
                  .attr('x1', function(d) { return x(parseYear(d.year)); })
                  .attr('y1', function(d) { return y(d.upper_ci) + margin; }) // Upper end of error bar
                  .attr('x2', function(d) { return x(parseYear(d.year)); })
                  .attr('y2', function(d) { return y(d.lower_ci) + margin; }) // Lower end of error bar
                  .style('stroke', d => colour(d.Var))
                  .attr('stroke-width', 1)
                  .style('opacity', 0) // set initial opacity of 0
                  .transition()
                  .delay(function (d, i) { return i * 0; }) // delay between group rendering - set here to 0
                  .duration(function (d, i) { return 1000 + (i * 10); })
                 .attr('x1', function(d) { return x(parseYear(d.year)); })
                  .attr('y1', function(d) { return y(d.upper_ci) + margin; }) // Upper end of error bar
                  .attr('x2', function(d) { return x(parseYear(d.year)); })
                  .attr('y2', function(d) { return y(d.lower_ci) + margin; }) // Lower end of error bar
                  .style('opacity', 0.6); // transition to final opacity of 0.6
               
               // Add error bar caps
              svg.selectAll('.error-bar-cap-upper')
                  .data(data)
                  .enter()
                  .append('line')
                  .attr('class', 'error-bar-cap-upper')
                  .attr('x1', function(d) { return x(parseYear(d.year)) - 5; }) // adjust the cap position as needed
                  .attr('y1', function(d) { return y(d.upper_ci) + margin; })
                  .attr('x2', function(d) { return x(parseYear(d.year)) + 5; }) // adjust the cap position as needed
                  .attr('y2', function(d) { return y(d.upper_ci) + margin; })
                  .style('stroke', d => colour(d.Var))
                  .attr('stroke-width', 1)
                  .style('opacity', 0) // set initial opacity of 0
                  .transition()
                  .delay(function (d, i) { return i * 0; })
                  .duration(function (d, i) { return 1000 + (i * 10); })
                 .attr('x1', function(d) { return x(parseYear(d.year)) - 5; }) // adjust the cap position as needed
                  .attr('y1', function(d) { return y(d.upper_ci) + margin; })
                  .attr('x2', function(d) { return x(parseYear(d.year)) + 5; }) // adjust the cap position as needed
                  .attr('y2', function(d) { return y(d.upper_ci) + margin; })
                  .style('opacity', 0.6); // transition to final opacity of 0.6
              
              svg.selectAll('.error-bar-cap-lower')
                  .data(data)
                  .enter()
                  .append('line')
                  .attr('class', 'error-bar-cap-lower')
                  .attr('x1', function(d) { return x(parseYear(d.year)) - 5; }) // adjust the cap position as needed
                  .attr('y1', function(d) { return y(d.lower_ci) + margin; })
                  .attr('x2', function(d) { return x(parseYear(d.year)) + 5; }) // adjust the cap position as needed
                  .attr('y2', function(d) { return y(d.lower_ci) + margin; })
                  .style('stroke', d => colour(d.Var))
                  .attr('stroke-width', 1)
                  .style('opacity', 0) // set initial opacity of 0
                  .transition()
                  .delay(function (d, i) { return i * 0; })
                  .duration(function (d, i) { return 1000 + (i * 10); })
                  .attr('x1', function(d) { return x(parseYear(d.year)) - 5; }) // adjust the cap position as needed
                  .attr('y1', function(d) { return y(d.lower_ci) + margin; })
                  .attr('x2', function(d) { return x(parseYear(d.year)) + 5; }) // adjust the cap position as needed
                  .attr('y2', function(d) { return y(d.lower_ci) + margin; })
                  .style('opacity', 0.6); // transition to final opacity of 0.6
               
              // Create a legend
              var n = data.length / 2;
              var legend = svg.selectAll('.legend')
                  .data(groupNames)
                  .enter().append('g')
                  .attr('class', 'legend')
                  .attr('transform', function (d, i) { return 'translate(' + i % n * 95 + ',' + Math.floor(i / n) * 100 + ')'; }); 
              
              legend.append('circle') 
                  .attr('cx', margin + 60) 
                  .attr('r', 5) 
                  .style('fill', colour)
                  .attr('cy', height + margin + 80); 
              
              legend.append('text')
                  .attr('x', margin - 6 + 60) 
                  .attr('y', height + margin + 80) 
                  .attr('dy', '.35em')
                  .style('text-anchor', 'end')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '10pt') 
                  .text(function (d) { return d; });
              
              // Create a tooltip
              var Tooltip = d3.select('body')
                  .append('div')
                  .attr('class', 'tooltip')
                  .style('position', 'absolute')
                  .style('background-color', 'rgba(255,255,255,0.8)')
                  .style('border-radius', '5px')
                  .style('padding', '5px')
                  .style('z-index', '100')
                  .style('opacity', 0)
                  .style('font-family', 'Tahoma, Geneva, sans-serif')
                  .style('font-size', '12pt');
              
              // Mouseover effects for tooltip
              var mouseover = function(d) {
                  Tooltip
                      .style('opacity', 1)
                      .style('box-shadow', '5px 5px 5px rgba(0,0,0,0.2)');
              };
              
              var mousemove = function(event, d) {
                  Tooltip
                      .html(d.year + ' (' + d.Var + '): ' + Math.round(d.Number * 10) / 10 + ' (' + Math.round(d.lower_ci * 10) / 10 + ', ' + Math.round(d.upper_ci * 10) / 10 + ')')
                      .style('left', (event.pageX + 30) + 'px')
                      .style('top', (event.pageY) + 'px');
              };
              
              var mouseleave = function(d) {
                  Tooltip
                      .style('opacity', 0);
              };
              
              svg.selectAll('circle')
                  .on('mouseover', mouseover)
                  .on('mousemove', mousemove)
                  .on('mouseleave', mouseleave);
              \"
              )
              
              ",
                 
                 "
          file.create('plot.js')
          
          ",
                 
                 "
          write(js, file = 'plot.js')
          
          ",
                 
                 
                 "
          #create d3 plot",
                 "
          data <- dataset_filtered
            r2d3(data = data, script = 'plot.js',
                 options = list(margin = 70,
                                colour = palette,
                                xLabel = 'x_var',
                                yLabel = 'y_var',
                                chartTitle = ''))"
          )
        }
      })
    
      
      output$d3_code_out <- renderPrint({
        cat(d3_plot_code_render(),sep = "\n")
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
                tags$li(tags$p("Uses r2d3 package that allows users to manipulate D3.js code using objects defined in R.")),
                tags$li(tags$p("D3 with r2d3 offers a huge level of customisation when compared to other plotting packages. Since plot features
                               are primary coded with d3.js javascript, the full functionaly of D3 is available to R users.")),
                tags$li(tags$p("Many of the other available packages used for creating interactive plots using R piggy back of this d3.js library.")),
                tags$li(tags$p("The downside of using r2d3 is that there is considerable js programming required to produce plots that resemble those created much more easily using other R packages.")),
                tags$li(tags$p("Plots above could be refined quite a bit and options for this are nearly unlimited, but in the interest of time have been left as is."))
              )
            ),
            "</span>"
          ))
        text
      })
      
      
      
      
      # Add clipboard buttons
      output$clip <- renderUI({
        # text to copy
        text <- d3_plot_code_render()
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
