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
svg.append("g")
    .style("font-family", "sans-serif")
    .style("font-size", "12pt")
    .attr("transform", "translate(" + 0 + "," + (height+margin) + ")")
    .call(d3.axisBottom(x));
svg.append("text")
    .attr("transform", "translate(" + (width/2 + margin -20) + " ," + (height+2*margin) + ")") /*may need to tweak this as margin and font size changes ... width/2 + margin -xxx*/
    .attr("dx", "1em")
    .style("text-anchor", "middle")
    .style("font-family", "sans-serif")
    .style("font-size", "16pt")
    .text(options.xLabel);

//Create the y axis
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
    .text(options.yLabel);

//Create the chart title
svg.append("text")
    .attr("x", (width / 2))
    .attr("y", (margin/2))
    .attr("text-anchor", "middle")
    .attr("dx", "1em")
    .style("font-size", "18pt")
    .style("font-family", "sans-serif")
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
    .attr("class", "tooltip")
    .style('position', 'absolute')
    .style('background-color', 'rgba(255,255,255,0.8)')
    .style('border-radius', '5px')
    .style('padding', '5px')
    .style("z-index", "100")
    .style('opacity', 0)
    .style("font-family", "Tahoma, Geneva, sans-serif")
    .style("font-size", "12pt");
    
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
        .style("left", (event.pageX + 30) + "px") 
        .style("top", (event.pageY) + "px");
};
var mouseleave = function(d) {
    Tooltip
        .style("opacity", 0);
    d3.select(this)
        .attr('fill', options.colour);
};

svg.selectAll('rect')
    .on("mouseover", mouseover)
    .on("mousemove", mousemove)
    .on("mouseleave", mouseleave);
    
    
  
    
    
    