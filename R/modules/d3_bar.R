D3_bar_ui <- function(id, label = "D3_bar_ui") {
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
             
             h2("D3 Bar Chart"),
             br(),
             
             fluidRow(
               tabBox(
                 width = 10,
                 id = 'secondary_box',
                 tabPanel(value = 'barplot1',
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





D3_bar_server <- function(id, label = "D3_bar_server", parent) {
  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      
      # output$select_colours <- renderUI({
      #   req(group(), input$primary_var, input$secondary_var, input$select_numerical, input$select_year)
      # 
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
      
      
      show_error_yn <- reactive({
        yn <- if(input$error_bars == FALSE | input$select_numerical == "Number") {
          "NARP"} else {"YARP"} 
        yn
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
        
        print(show_error_yn())
        
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
        
        # not the best way to do it, but the upper y axis limit is set by the highest upper_ci value of error bars. 
        # if Number is selected then there are no error bars so have to give the plot something to scale to on the y axis
        if(input$select_numerical == "Number"){
          dataset <- dataset %>%
            mutate(upper_ci = max(Number)*1.1)
        } else {dataset}
        
        dataset
        print(dataset)
      })
      
      
     
      # ----------- D3 ---------------------------------------------------------
      output$uioutput <- renderUI({
        
        req(dataset_filtered(), !is.null(input$error_bars))
        
        # to return load_time later in script
        start_time <- Sys.time()
        
        # palette further refined - D3 likes to have the correct number of options in the colour vector
        if (input$secondary_var == "none") {
          palette <- palette()[1]
        } else if (input$secondary_var != "none") {
          palette <- palette()[1:nrow(unique(dataset_filtered()[2]))]
        }
        
          if(length(group()) == 1 & show_error_yn() == "NARP") {
          
          output$d3_plot_out1 <- renderD3({
            data <- dataset_filtered() %>% rename(Var = 1)
            r2d3(data = data, script = "www/bar.js",
                 options = list(margin = 70,
                                barPadding = 0.1,
                                colour = palette[1],
                                xLabel = input$primary_var,
                                yLabel = input$select_numerical,
                                chartTitle = ""))
            
          })
          
        } else {NULL}
          
          if(length(group()) == 2 & input$primary_var != input$secondary_var & show_error_yn() == "NARP") {
          
          output$d3_plot_out2 <- renderD3({
              data <- dataset_filtered() %>% rename(var1 = 1, var2 = 2, number = 3)
              r2d3(data = data, script = "www/grouped_bar.js",
                   options = list(margin2 = 70,
                                  barPadding2 = 0.1,
                                  colour2 = palette,
                                  xLabel2 = input$primary_var,
                                  yLabel2 = input$select_numerical,
                                  chartTitle2 = ""))
            })
          
        } else {NULL}
         
          if(length(group()) == 1 & show_error_yn() == "YARP") {
          
          output$d3_plot_out3 <- renderD3({
            
            data <- dataset_filtered() %>% rename(Var = 1)
            r2d3(data = data, script = "www/bar_with_error.js",
                 options = list(margin = 70,
                                barPadding = 0.1,
                                colour = palette[1],
                                xLabel = input$primary_var,
                                yLabel = input$select_numerical,
                                chartTitle = ""))
            
          })
          
        } else {NULL}
        
          if(length(group()) == 2 & input$primary_var != input$secondary_var & show_error_yn() == "YARP") {
          
          output$d3_plot_out4 <- renderD3({
            data <- dataset_filtered() %>% rename(var1 = 1, var2 = 2, number = 3)
            r2d3(data = data, script = "www/grouped_bar_with_error.js",
                 options = list(margin2 = 70,
                                barPadding2 = 0.1,
                                colour2 = palette,
                                xLabel2 = input$primary_var,
                                yLabel2 = input$select_numerical,
                                chartTitle2 = ""))
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
        if (length(group()) == 1 &
            show_error_yn() == "NARP") {
          d3Output(session$ns("d3_plot_out1"), height = "600px") %>% withSpinner(hide.ui = FALSE)
          
        } else if (length(group()) == 2 &
                   input$primary_var != input$secondary_var &
                   show_error_yn() == "NARP") {
          d3Output(session$ns("d3_plot_out2"), height = "600px") %>% withSpinner(hide.ui = FALSE)
          
        } else if (length(group()) == 1 &
                   show_error_yn() == "YARP") {
          d3Output(session$ns("d3_plot_out3"), height = "600px") %>% withSpinner(hide.ui = FALSE)
          
        } else  if (length(group()) == 2 &
                    input$primary_var != input$secondary_var &
                    show_error_yn() == "YARP") {
          d3Output(session$ns("d3_plot_out4"), height = "600px") %>% withSpinner(hide.ui = FALSE)
        } else {
          NULL
        }
        
        
      })
      
      
      
      
      # generate  d3 plot code for rendering code in app
      d3_plot_code_render <- reactive({
        req(input$primary_var, input$secondary_var, input$select_numerical, input$select_year)

        if(length(group()) == 1 & show_error_yn() == "NARP") {
          paste0("
          library(r2d3)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey() 
          
          ",
                 
          single_bar,
          
             
          "
          
          #create js file for r2d3 to read",
          "
          js <- HTML(
              \"
              //remove some elements so that refreshed plot doesnt render overtop of old
              svg.selectAll('lines').remove();
              svg.selectAll('text').remove();
              svg.selectAll('g').remove();
              svg.selectAll('rect').remove();
              
              //Set some initial values
              var margin = options.margin,
              width = width-(2*margin),
              height = height-(2*margin),
              barPadding = options.barPadding*(width/data.length),
              barWidth = (width-(data.length*barPadding))/data.length,
              xmax = d3.max(data, function(d) { return d.Var; }),
              xmin = d3.min(data, function(d) { return d.Var; }),
              ymax = d3.max(data, function(d) { return d.upper_ci; });
              
              //Create the x axis
              var x = d3.scaleBand()
              .domain(data.map(function(d) { return d.Var; }))
              .range([margin, margin+width]);
              
              svg.append('g')
              .style('font-family', 'sans-serif')
              .style('font-size', '12pt')
              .attr('transform', 'translate(' + 0 + ',' + (height+margin) + ')')
              .call(d3.axisBottom(x));
              
              svg.append('text')
              .attr('transform', 'translate(' + (width/2 + margin -20) + ' ,' + (height+2*margin) + ')') /*may need to tweak this as margin and font size changes ... width/2 + margin -xxx*/
                .attr('dx', '1em')
              .style('text-anchor', 'middle')
              .style('font-family', 'sans-serif')
              .style('font-size', '16pt')
              .text(options.xLabel);
              
              //Create the y axis
              var y = d3.scaleLinear()
              .range([height, 0])
              .domain([0, ymax]);
              svg.append('g')
              .style('font-family', 'sans-serif')
              .style('font-size', '12pt')
              .attr('transform', 'translate(' + margin + ', ' + margin + ')')
              .call(d3.axisLeft(y));
              svg.append('text')
              .attr('transform', 'translate(' + 0 + ' ,' + ((height+2*margin)/2) + ') rotate(-90)')
              .attr('dy', '1em')
              .style('text-anchor', 'middle')
              .style('font-family', 'sans-serif')
              .style('font-size', '16pt')
              .text(options.yLabel);
              
              //Create the chart title
              svg.append('text')
              .attr('x', (width / 2))
              .attr('y', (margin/2))
              .attr('text-anchor', 'middle')
              .attr('dx', '1em')
              .style('font-size', '18pt')
              .style('font-family', 'sans-serif')
              .text(options.chartTitle);
              
              //Create the chart
              svg.selectAll('rect')
              .data(data)
              .enter()
              .append('rect')
              .attr('width', barWidth)
              .attr('x', function(d, i) { return (margin+((i+0.5)*barPadding)+(i*barWidth)); })
              .attr('y', height + margin)
              .attr('fill', options.colour);
              
              //Transition animation on load
              svg.selectAll('rect')
              .transition()
              .delay(function(d,i){return (i*100);})
              .duration(function(d,i){return (1000+(i*200));})
              .attr('height', function(d) { return d.Number/ymax * height; })
              .attr('y', function(d) { return (height+margin-(d.Number/ymax * height)); });
              
              //Create a tooltip
              var Tooltip = d3.select('body') /*placing tooltip in body seems to work*/
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
              
              //Mouseover effects for tooltip
              var mouseover = function(d) {
                Tooltip
                .style('opacity', 1)
                .style('box-shadow', '5px 5px 5px rgba(0,0,0,0.2)');
                d3.select(this)
                .attr('fill', 'rgba(100,0,0,1)');
              };
              var mousemove = function(event, d) {
                Tooltip
                .html(d.Var + ': ' + Math.round(d.Number * 10) / 10)
                .style('left', (event.pageX + 30) + 'px') 
                .style('top', (event.pageY) + 'px');
              };
              var mouseleave = function(d) {
                Tooltip
                .style('opacity', 0);
                d3.select(this)
                .attr('fill', options.colour);
              };
              
              svg.selectAll('rect')
              .on('mouseover', mouseover)
              .on('mousemove', mousemove)
              .on('mouseleave', mouseleave)
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
          data <- dataset_filtered %>% rename(Var = 1)
            r2d3(data = data, script = 'plot.js',
                 options = list(margin = 70,
                                barPadding = 0.1,
                                colour = palette[1],
                                xLabel = 'x_var',
                                yLabel = 'y_var',
                                chartTitle = ''))"
          )
          
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var & show_error_yn() == "NARP") {
          paste0("
          library(r2d3)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey() 
          
          ",
                 
          multi_bar,
          
          
          "
          
          #create js file for r2d3 to read",
          "
          js <- HTML(
              \"
              // Remove some elements so that the refreshed plot doesn't render over the old
              svg.selectAll('lines').remove();
              svg.selectAll('text').remove();
              svg.selectAll('g').remove();
              svg.selectAll('rect').remove();
              
              // Set some initial values
              var margin = options.margin2,
                  width = width - (2 * margin),
                  height = height - (2 * margin) -30, //
                  barPadding = options.barPadding2 * (width / data.length),
                  groupPadding = 0.3, // Adjust as needed
                  groupNames = [...new Set(data.map(d => d.var2))],
                  barWidth = (width - (groupNames.length * barPadding)) / groupNames.length,
                  xmax = d3.max(data, function (d) { return d.number; }),
                  xmin = d3.min(data, function (d) { return d.number; }),
                  ymax = d3.max(data, function (d) { return d.upper_ci; }),
                  colour = d3.scaleOrdinal()
                    .range(options.colour2);
              
              // Create the x axis
              var x0 = d3.scaleBand()
                  .domain(data.map(function (d) { return d.var1; }))
                  .range([margin, margin + width])
                  .padding(groupPadding);
              
              var x1 = d3.scaleBand()
                  .domain(groupNames)
                  .range([0, x0.bandwidth()]);
              
              svg.append('g')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '12pt')
                  .attr('transform', 'translate(' + 0 + ',' + (height + margin) + ')')
                  .call(d3.axisBottom(x0));
              
              svg.append('text')
                  .attr('transform', 'translate(' + (width / 2 + margin - 20) + ' ,' + (height + 2 * margin - 15) + ')') //*may need to tweak this as margin and font size changes ... width/2 + margin -xxx*/
                  .attr('dx', '1em')
                  .style('text-anchor', 'middle')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '16pt')
                  .text(options.xLabel2);
              
              // Create the y axis
              var y = d3.scaleLinear()
                  .range([height, 0])
                  .domain([0, ymax]);
              svg.append('g')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '12pt')
                  .attr('transform', 'translate(' + margin + ', ' + margin + ')')
                  .call(d3.axisLeft(y));
              svg.append('text')
                  .attr('transform', 'translate(' + 0 + ' ,' + ((height+2*margin)/2) + ') rotate(-90)')
                  .attr('dy', '1em')
                  .style('text-anchor', 'middle')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '16pt')
                  .text(options.yLabel2);
              
              // Create the chart title
              svg.append('text')
                  .attr('x', (width / 2))
                  .attr('y', (margin / 2))
                  .attr('text-anchor', 'middle')
                  .attr('dx', '1em')
                  .style('font-size', '18pt')
                  .style('font-family', 'sans-serif')
                  .text(options.chartTitle2);
              
              // Create the grouped bars
              svg.selectAll()
                  .data(data)
                  .enter()
                  .append('rect')
                  .attr('x', function (d) { return x0(d.var1) + x1(d.var2); })
                  .attr('y', height + margin)
                  .attr('height', 0)
                  .attr('width', x1.bandwidth())
                  .attr('fill', function (d) { return options.colour2[groupNames.indexOf(d.var2)]; })
                  .on('mouseover', mouseover)
                  .on('mousemove', mousemove)
                  .on('mouseleave', mouseleave)
                  .transition()
                  .delay(function (d, i) { return i * 10; })
                  .duration(function (d, i) { return 1000 + (i * 10); })
                  .attr('height', function (d) { return height - y(d.number) ; })
                  .attr('y', function (d) { return y(d.number) + margin; });
              
              // Create a legend
              var n = data.length/2;
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
              function mouseover(d) {
                  Tooltip
                      .style('opacity', 1)
                      .style('box-shadow', '5px 5px 5px rgba(0,0,0,0.2)');
                  d3.select(this)
                      .attr('fill', 'rgba(100,0,0,1)');
              }
              
              function mousemove(event, d) {
                  Tooltip
                      .html(d.var1 + ' (' + d.var2 + '): ' + Math.round(d.number * 10) / 10)
                      .style('left', (event.pageX + 30) + 'px')
                      .style('top', (event.pageY) + 'px');
              }
              
              function mouseleave(d) {
                  Tooltip
                      .style('opacity', 0);
                  d3.select(this)
                      .attr('fill', function (d) { return options.colour2[groupNames.indexOf(d.var2)]; })
              }
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
          data <- dataset_filtered %>% rename(var1 = 1, var2 = 2, number = 3)
              r2d3(data = data, script = 'plot.js',
                   options = list(margin2 = 70,
                                  barPadding2 = 0.1,
                                  colour2 = palette,
                                  xLabel2 = 'x_var',
                                  yLabel2 = 'y_var',
                                  chartTitle2 = ''))"
          )
        } else if(length(group()) == 1 & show_error_yn() == "YARP") {
          
          paste0("
          library(r2d3)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey() 
          
          ",
                 
                 single_bar,
                 
                 
                 "
          
          #create js file for r2d3 to read",
                 "
          js <- HTML(
              \"
              //remove some elements so that refreshed plot doesnt render overtop of old
              svg.selectAll('lines').remove();
              svg.selectAll('text').remove();
              svg.selectAll('g').remove();
              svg.selectAll('rect').remove();
              
              //Set some initial values
              var margin = options.margin,
              width = width-(2*margin),
              height = height-(2*margin),
              barPadding = options.barPadding*(width/data.length),
              barWidth = (width-(data.length*barPadding))/data.length,
              xmax = d3.max(data, function(d) { return d.Var; }),
              xmin = d3.min(data, function(d) { return d.Var; }),
              ymax = d3.max(data, function(d) { return d.upper_ci; });
              
              //Create the x axis
              var x = d3.scaleBand()
              .domain(data.map(function(d) { return d.Var; }))
              .range([margin, margin+width]);
              svg.append('g')
              .style('font-family', 'sans-serif')
              .style('font-size', '12pt')
              .attr('transform', 'translate(' + 0 + ',' + (height+margin) + ')')
              .call(d3.axisBottom(x));
              svg.append('text')
              .attr('transform', 'translate(' + (width/2 + margin -20) + ' ,' + (height+2*margin) + ')') /*may need to tweak this as margin and font size changes ... width/2 + margin -xxx*/
                .attr('dx', '1em')
              .style('text-anchor', 'middle')
              .style('font-family', 'sans-serif')
              .style('font-size', '16pt')
              .text(options.xLabel);
              
              //Create the y axis
              var y = d3.scaleLinear()
              .range([height, 0])
              .domain([0, ymax]);
              svg.append('g')
              .style('font-family', 'sans-serif')
              .style('font-size', '12pt')
              .attr('transform', 'translate(' + margin + ', ' + margin + ')')
              .call(d3.axisLeft(y));
              svg.append('text')
              .attr('transform', 'translate(' + 0 + ' ,' + ((height+2*margin)/2) + ') rotate(-90)')
              .attr('dy', '1em')
              .style('text-anchor', 'middle')
              .style('font-family', 'sans-serif')
              .style('font-size', '16pt')
              .text(options.yLabel);
              
              //Create the chart title
              svg.append('text')
              .attr('x', (width / 2))
              .attr('y', (margin/2))
              .attr('text-anchor', 'middle')
              .attr('dx', '1em')
              .style('font-size', '18pt')
              .style('font-family', 'sans-serif')
              .text(options.chartTitle);
              
              //Create the chart
              svg.selectAll('rect')
              .data(data)
              .enter()
              .append('rect')
              .attr('width', barWidth)
              .attr('x', function(d, i) { return (margin+((i+0.5)*barPadding)+(i*barWidth)); })
              .attr('y', height + margin)
              .attr('fill', options.colour);
              
              //Transition animation on load
              svg.selectAll('rect')
              .transition()
              .delay(function(d,i){return (i*100);})
              .duration(function(d,i){return (1000+(i*200));})
              .attr('height', function(d) { return d.Number/ymax * height; })
              .attr('y', function(d) { return (height+margin-(d.Number/ymax * height)); });
              
              // Error bars
              svg.selectAll()
              .data(data)
              .enter()
              .append('line')
              .attr('x1', function (d) { return x(d.Var) + x.bandwidth() / 2; })
              .attr('x2', function (d) { return x(d.Var) + x.bandwidth() / 2; })
              .attr('y1', function (d) { return y(d.lower_ci) + margin; })
              .attr('y2', function (d) { return y(d.upper_ci) + margin; })
              .style('stroke', 'black')
              .style('stroke-width', 2)
              .style('opacity', 0) // set initial opacity of 0
              .transition()
              .delay(function (d, i) { return i * 50; })
              .duration(function (d, i) { return 1000 + (i * 10); })
              .attr('y1', function (d) { return y(d.lower_ci) + margin; })
              .attr('y2', function (d) { return y(d.upper_ci) + margin; })
              .style('opacity', 1); // transition to final opacity of 1
              
              //Create a tooltip
              var Tooltip = d3.select('body') /*placing tooltip in body seems to work*/
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
              
              //Mouseover effects for tooltip
              var mouseover = function(d) {
                Tooltip
                .style('opacity', 1)
                .style('box-shadow', '5px 5px 5px rgba(0,0,0,0.2)');
                d3.select(this)
                .attr('fill', 'rgba(100,0,0,1)');
              };
              var mousemove = function(event, d) {
                Tooltip
                .html(d.Var + ': ' + Math.round(d.Number * 10) / 10 + ' (' + Math.round(d.lower_ci * 10) / 10 + ', ' + Math.round(d.upper_ci * 10) / 10 + ')')
                .style('left', (event.pageX + 30) + 'px') 
                .style('top', (event.pageY) + 'px');
              };
              var mouseleave = function(d) {
                Tooltip
                .style('opacity', 0);
                d3.select(this)
                .attr('fill', options.colour);
              };
              
              svg.selectAll('rect')
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
           data <- dataset_filtered %>% rename(Var = 1)
            r2d3(data = data, script = 'plot.js',
                 options = list(margin = 70,
                                barPadding = 0.1,
                                colour = palette[1],
                                xLabel = 'x_var',
                                yLabel = 'y_var',
                                chartTitle = ''))"
          )
          
        } else if(length(group()) == 2 & input$primary_var != input$secondary_var & show_error_yn() == "YARP") {
          paste0("
          library(r2d3)
          library(tidyverse)
          library(pals)
          
          palette <- pals::glasbey() 
          
          ",
                 
                 multi_bar,
                 
                 
                 "
          
          #create js file for r2d3 to read",
                 "
          js <- HTML(
              \"
              // Remove some elements so that the refreshed plot doesn't render over the old
              svg.selectAll('lines').remove();
              svg.selectAll('text').remove();
              svg.selectAll('g').remove();
              svg.selectAll('rect').remove();
              
              // Set some initial values
              var margin = options.margin2,
                  width = width - (2 * margin),
                  height = height - (2 * margin) -30, //
                  barPadding = options.barPadding2 * (width / data.length),
                  groupPadding = 0.3, // Adjust as needed
                  groupNames = [...new Set(data.map(d => d.var2))],
                  barWidth = (width - (groupNames.length * barPadding)) / groupNames.length,
                  xmax = d3.max(data, function (d) { return d.number; }),
                  xmin = d3.min(data, function (d) { return d.number; }),
                  ymax = d3.max(data, function (d) { return d.upper_ci; }),
                  colour = d3.scaleOrdinal()
                      .range(options.colour2);
              
              // Create the x axis
              var x0 = d3.scaleBand()
                  .domain(data.map(function (d) { return d.var1; }))
                  .range([margin, margin + width])
                  .padding(groupPadding);
              
              var x1 = d3.scaleBand()
                  .domain(groupNames)
                  .range([0, x0.bandwidth()]);
              
              svg.append('g')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '12pt')
                  .attr('transform', 'translate(' + 0 + ',' + (height + margin) + ')')
                  .call(d3.axisBottom(x0));
              
              svg.append('text')
                  .attr('transform', 'translate(' + (width / 2 + margin - 20) + ' ,' + (height + 2 * margin - 15) + ')') //
                  .attr('dx', '1em')
                  .style('text-anchor', 'middle')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '16pt')
                  .text(options.xLabel2);
              
              // Create the y axis
              var y = d3.scaleLinear()
                  .range([height, 0])
                  .domain([0, ymax]);
              
              svg.append('g')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '12pt')
                  .attr('transform', 'translate(' + margin + ', ' + margin + ')')
                  .call(d3.axisLeft(y));
              
              svg.append('text')
                  .attr('transform', 'translate(' + 0 + ' ,' + ((height + 2 * margin) / 2) + ') rotate(-90)')
                  .attr('dy', '1em')
                  .style('text-anchor', 'middle')
                  .style('font-family', 'sans-serif')
                  .style('font-size', '16pt')
                  .text(options.yLabel2);
              
              // Create the chart title
              svg.append('text')
                  .attr('x', (width / 2))
                  .attr('y', (margin / 2))
                  .attr('text-anchor', 'middle')
                  .attr('dx', '1em')
                  .style('font-size', '18pt')
                  .style('font-family', 'sans-serif')
                  .text(options.chartTitle2);
              
              // Create the grouped bars with error bars
              svg.selectAll()
                  .data(data)
                  .enter()
                  .append('rect')
                  .attr('x', function (d) { return x0(d.var1) + x1(d.var2); })
                  .attr('y', height + margin)
                  .attr('height', 0)
                  .attr('width', x1.bandwidth())
                  .attr('fill', function (d) { return options.colour2[groupNames.indexOf(d.var2)]; })
                  .on('mouseover', mouseover)
                  .on('mousemove', mousemove)
                  .on('mouseleave', mouseleave)
                  .transition()
                  .delay(function (d, i) { return i * 10; })
                  .duration(function (d, i) { return 1000 + (i * 10); })
                  .attr('height', function (d) { return height - y(d.number); })
                  .attr('y', function (d) { return y(d.number) + margin; });
              
              // Error bars
              svg.selectAll()
                  .data(data)
                  .enter()
                  .append('line')
                  .attr('x1', function (d) { return x0(d.var1) + x1(d.var2) + x1.bandwidth() / 2; })
                  .attr('x2', function (d) { return x0(d.var1) + x1(d.var2) + x1.bandwidth() / 2; })
                  .attr('y1', function (d) { return y(d.lower_ci) + margin; })
                  .attr('y2', function (d) { return y(d.upper_ci) + margin; })
                  .style('stroke', 'black')
                  .style('stroke-width', 2)
                  .style('opacity', 0) // set initial opacity of 0
                  .transition()
                  .delay(function (d, i) { return i * 10; })
                  .duration(function (d, i) { return 1000 + (i * 10); })
                  .attr('y1', function (d) { return y(d.lower_ci) + margin; })
                  .attr('y2', function (d) { return y(d.upper_ci) + margin; })
                  .style('opacity', 1); // transition to final opacity of 1
              
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
              function mouseover(d) {
                  Tooltip
                      .style('opacity', 1)
                      .style('box-shadow', '5px 5px 5px rgba(0,0,0,0.2)');
                  d3.select(this)
                      .attr('fill', 'rgba(100,0,0,1)');
              }
              
              function mousemove(event, d) {
                  Tooltip
                      .html(d.var1 + ' (' + d.var2 + '): ' + Math.round(d.number * 10) / 10 + ' (' + Math.round(d.lower_ci * 10) / 10 + ', ' + Math.round(d.upper_ci * 10) / 10 + ')')
                      .style('left', (event.pageX + 30) + 'px')
                      .style('top', (event.pageY) + 'px');
              }
              
              function mouseleave(d) {
                  Tooltip
                      .style('opacity', 0);
                  d3.select(this)
                      .attr('fill', function (d) { return options.colour2[groupNames.indexOf(d.var2)]; })
              }
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
          data <- dataset_filtered %>% rename(var1 = 1, var2 = 2, number = 3)
            r2d3(data = data, script = 'plot.js',
                 options = list(margin2 = 70,
                                barPadding2 = 0.1,
                                colour2 = palette,
                                xLabel2 = 'x_var',
                                yLabel2 = 'y_var',
                                chartTitle2 = ''))"
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
