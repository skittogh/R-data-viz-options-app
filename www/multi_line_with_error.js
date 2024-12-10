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
var parseYear = d3.timeParse("%Y");

var x = d3.scaleTime()
    .domain(d3.extent(data, function(d) { return parseYear(d.year); }))
    .range([margin, margin + width]);

svg.append("g")
    .style("font-family", "sans-serif")
    .style("font-size", "12pt")
    .attr("transform", "translate(0," + (height + margin) + ")")
    .call(d3.axisBottom(x).ticks(d3.timeYear).tickFormat(d3.timeFormat("%Y")));

svg.append("text")
    .attr("transform", "translate(" + (width / 2 + margin - 20) + " ," + (height + 2 * margin - 15) + ")") //
    .attr("dx", "1em")
    .style("text-anchor", "middle")
    .style("font-family", "sans-serif")
    .style("font-size", "16pt")
    .text(options.xLabel);

// Create the y axis
var y = d3.scaleLinear()
    .range([height, 0])
    .domain([0, ymax]);

svg.append("g")
    .style("font-family", "sans-serif")
    .style("font-size", "12pt")
    .attr("transform", "translate(" + margin + ", " + margin + ")")
    .call(d3.axisLeft(y).ticks(10,"s")); // added .ticks(10,"s") to format as 10k instead of 10,000
    
svg.append("text")
    .attr("transform", "translate(" + 0 + " ," + ((height + 2 * margin) / 2) + ") rotate(-90)")
    .attr("dy", "1em")
    .style("text-anchor", "middle")
    .style("font-family", "sans-serif")
    .style("font-size", "16pt")
    .text(options.yLabel);

// Create the chart title
svg.append("text")
    .attr("x", width / 2 + margin)
    .attr("y", margin / 2)
    .attr("text-anchor", "middle")
    .attr("dx", "1em")
    .style("font-size", "18pt")
    .style("font-family", "sans-serif")
    .text(options.chartTitle);

// Create a function to group data by the "Group" property
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
    .style("fill", d => colour(d.Var))
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
    .style("stroke", d => colour(d.Var))
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
    .style("stroke", d => colour(d.Var))
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
    .style("stroke", d => colour(d.Var))
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
var legend = svg.selectAll(".legend")
    .data(groupNames)
    .enter().append("g")
    .attr("class", "legend")
    .attr("transform", function (d, i) { return "translate(" + i % n * 95 + "," + Math.floor(i / n) * 100 + ")"; }); 

legend.append("circle") 
    .attr("cx", margin + 60) 
    .attr('r', 5) 
    .style('fill', colour)
    .attr("cy", height + margin + 80); 

legend.append("text")
    .attr("x", margin - 6 + 60) 
    .attr("y", height + margin + 80) 
    .attr("dy", ".35em")
    .style("text-anchor", "end")
    .style("font-family", "sans-serif")
    .style("font-size", "10pt") 
    .text(function (d) { return d; });

// Create a tooltip
var Tooltip = d3.select('body')
    .append('div')
    .attr("class", "tooltip")
    .style('position', 'absolute')
    .style('background-color', 'rgba(255,255,255,0.8)')
    .style('border-radius', '5px')
    .style('padding', '5px')
    .style("z-index", "100")
    .style('opacity', 0)
    .style("font-family", "Tahoma, Geneva, sans-serif")
    .style("font-size", "12pt");

// Mouseover effects for tooltip
var mouseover = function(d) {
    Tooltip
        .style('opacity', 1)
        .style('box-shadow', '5px 5px 5px rgba(0,0,0,0.2)');
};

var mousemove = function(event, d) {
    Tooltip
        .html(d.year + ' (' + d.Var + '): ' + Math.round(d.Number * 10) / 10 + ' (' + Math.round(d.lower_ci * 10) / 10 + ', ' + Math.round(d.upper_ci * 10) / 10 + ')')
        .style("left", (event.pageX + 30) + "px")
        .style("top", (event.pageY) + "px");
};

var mouseleave = function(d) {
    Tooltip
        .style("opacity", 0);
};

svg.selectAll('circle')
    .on("mouseover", mouseover)
    .on("mousemove", mousemove)
    .on("mouseleave", mouseleave);
