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
    .attr("transform", "translate(" + (width / 2 + margin - 20) + " ," + (height + 2 * margin) + ")")
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

// Create the line
svg.append("path")
    .datum(data)
    .attr("fill", "none")
    .attr("stroke", options.colour)
    .attr("stroke-width", 2)
    .attr("d", d3.line()
        .x(function(d) { return x(parseYear(d.year)); })
        .y(function(d) { return y(d.Number) + margin; })
    )
    .style('opacity', 0) // set initial opacity of 0
    .transition()
    .delay(function (d, i) { return i * 50; })
    .duration(function (d, i) { return 1000 + (i * 10); })
     .attr("d", d3.line()
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
        .html(d.year + ': ' + Math.round(d.Number * 10) / 10)
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
