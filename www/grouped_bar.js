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

svg.append("g")
    .style("font-family", "sans-serif")
    .style("font-size", "12pt")
    .attr("transform", "translate(" + 0 + "," + (height + margin) + ")")
    .call(d3.axisBottom(x0));

svg.append("text")
    .attr("transform", "translate(" + (width / 2 + margin - 20) + " ," + (height + 2 * margin - 15) + ")") //*may need to tweak this as margin and font size changes ... width/2 + margin -xxx*/
    .attr("dx", "1em")
    .style("text-anchor", "middle")
    .style("font-family", "sans-serif")
    .style("font-size", "16pt")
    .text(options.xLabel2);

// Create the y axis
var y = d3.scaleLinear()
    .range([height, 0])
    .domain([0, ymax]);
svg.append("g")
    .style("font-family", "sans-serif")
    .style("font-size", "12pt")
    .attr("transform", "translate(" + margin + ", " + margin + ")")
    .call(d3.axisLeft(y));
svg.append("text")
    .attr("transform", "translate(" + 0 + " ," + ((height+2*margin)/2) + ") rotate(-90)")
    .attr("dy", "1em")
    .style("text-anchor", "middle")
    .style("font-family", "sans-serif")
    .style("font-size", "16pt")
    .text(options.yLabel2);

// Create the chart title
svg.append("text")
    .attr("x", (width / 2))
    .attr("y", (margin / 2))
    .attr("text-anchor", "middle")
    .attr("dx", "1em")
    .style("font-size", "18pt")
    .style("font-family", "sans-serif")
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
    .on("mouseover", mouseover)
    .on("mousemove", mousemove)
    .on("mouseleave", mouseleave)
    .transition()
    .delay(function (d, i) { return i * 10; })
    .duration(function (d, i) { return 1000 + (i * 10); })
    .attr('height', function (d) { return height - y(d.number) ; })
    .attr('y', function (d) { return y(d.number) + margin; });

// Create a legend
var n = data.length/2;
var legend = svg.selectAll(".legend")
    .data(groupNames)
    .enter().append("g")
    .attr("class", "legend")
    .attr("transform", function (d, i) { return "translate(" + i % n * 95 + "," + Math.floor(i / n) * 100 + ")"; }); //

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
        .style("left", (event.pageX + 30) + "px")
        .style("top", (event.pageY) + "px");
}

function mouseleave(d) {
    Tooltip
        .style("opacity", 0);
    d3.select(this)
        .attr('fill', function (d) { return options.colour2[groupNames.indexOf(d.var2)]; })
}