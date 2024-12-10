// Remove some elements so that the refreshed plot doesn't render over the old
svg.selectAll('lines').remove();
svg.selectAll('text').remove();
svg.selectAll('g').remove();
svg.selectAll('rect').remove();
d3.selectAll().remove();


// Set some initial values
var margin2 = options.margin2,
    width = width - (2 * margin2),
    height = height - (2 * margin2),
    barPadding = options.barPadding * (width / data.length),
    groupPadding = 0.3, // Adjust as needed
    groupNames = [...new Set(data.map(d => d.Var2))],
    barWidth2 = (width - (groupNames.length * barPadding)) / groupNames.length,
    xmax = d3.max(data, function (d) { return d.Number; }),
    xmin = d3.min(data, function (d) { return d.Number; }),
    ymax = d3.max(data, function (d) { return d.Number; }),
    colour = d3.scaleOrdinal()
      .range(options.colour);

// Create the x axis
var x0 = d3.scaleBand()
    .domain(data.map(function (d) { return d.Var1; }))
    .range([margin2, margin2 + width])
    .padding(groupPadding);

var x1 = d3.scaleBand()
    .domain(groupNames)
    .range([0, x0.bandwidth()]);
    /*.padding(barPadding);*/

svg.append("g")
    .style("font-family", "sans-serif")
    .style("font-size", "12pt")
    .attr("transform", "translate(" + 0 + "," + (height + margin2) + ")")
    .call(d3.axisBottom(x0));

svg.append("text")
    .attr("transform", "translate(" + (width/2 + margin2 -20) + " ," + (height+2*margin2 -24) + ")") /*may need to tweak this as margin and font size changes ... width/2 + margin -xxx*/
    .attr("dx", "1em")
    .style("text-anchor", "middle")
    .style("font-family", "sans-serif")
    .style("font-size", "16pt")
    .text(options.xLabel);


// Create the y axis
var y2 = d3.scaleLinear()
    .range([height, 0])
    .domain([0, ymax]);
svg.append("g")
    .style("font-family", "sans-serif")
    .style("font-size", "12pt")
    .attr("transform", "translate(" + margin2 + ", " + margin2 + ")")
    .call(d3.axisLeft(y2));
svg.append("text")
    .attr("transform", "translate(" + 0 + " ," + ((height+2*margin2)/2) + ") rotate(-90)")
    .attr("dy", "1em")
    .style("text-anchor", "middle")
    .style("font-family", "sans-serif")
    .style("font-size", "16pt")
    .text(options.yLabel);


// Create the chart title
svg.append("text")
    .attr("x", (width / 2))
    .attr("y", (margin2 / 2))
    .attr("text-anchor", "middle")
    .attr("dx", "1em")
    .style("font-size", "18pt")
    .style("font-family", "sans-serif")
    .text(options.chartTitle);

// Create the grouped bars
svg.selectAll()
    .data(data)
    .enter()
    .append('rect')
    .attr('x', function (d) { return x0(d.Var1) + x1(d.Var2); })
    .attr('y2', height + margin2)
    .attr('height', 0)
    .attr('width', x1.bandwidth())
    .attr('fill', function (d) { return options.colour[groupNames.indexOf(d.Var2)]; })
    .on("mouseover", mouseover)
    .on("mousemove", mousemove)
    .on("mouseleave", mouseleave)
    .transition()
    .delay(function (d, i) { return i * 50; })
    /*.delay(function (d, i) { return i * 100; })*/
    .duration(function (d, i) { return 1000 + (i * 10); })
    /*.duration(function (d, i) { return 1000 + (i * 200); })*/
    .attr('height', function (d) { return height - y(d.Number) ; })
  /*  .attr('y', function (d) { return y(d.Number); });*/
    .attr('y2', function (d) { return y(d.Number) + margin2; });

// Create a legend
var n = data.length/2;

var legend = svg.selectAll(".legend")
    .data(groupNames)
    .enter().append("g")
    .attr("class", "legend")
    /*.attr("transform", function (d, i) { return "translate(0," + i * 20 + ")"; });*/
    .attr("transform", function(d,i) { return "translate(" + i%n * 90 + "," + Math.floor(i/n) * 80 + ")"; });

legend.append("rect")
    /*.attr("x", width - 18)*/
    .attr("x", width /2)
    .attr("width", 18)
    .attr("height", 18)
    /*.style('fill', function (d) { return options.colour[groupNames.indexOf(d.Var2)]; })*/
    .style('fill', colour)
    /*.attr("y", 6)*/
    .attr("y", height + margin2 +53)

legend.append("text")
    /*.attr("x", width - 24)*/
    .attr("x", (width /2) - 5)
    .attr("y", height + margin2 +60)
    /*.attr("y", 9)*/
    .attr("dy", ".35em")
    .style("text-anchor", "end")
    .style("font-family", "sans-serif")
    .style("font-size", "12pt")
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
        /*.html(d.Var1 + ' (' + d.Var2 + '): ' + d.Number)*/
        .html(d.Var1 + ' (' + d.Var2 + '): ' + Math.round(d.Number * 10) / 10)
        .style("left", (event.pageX + 30) + "px")
        .style("top", (event.pageY) + "px");
}

function mouseleave(d) {
    Tooltip
        .style("opacity", 0);
    d3.select(this)
        .attr('fill', function (d) { return options.colour[groupNames.indexOf(d.Var2)]; })
}