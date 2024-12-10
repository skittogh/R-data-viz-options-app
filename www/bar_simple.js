svg.selectAll("rect").remove()
d3.select("rect").html("")
d3.select("d.parent").selectAll("*").remove()
Tooltip = null
mouseover = null
mousemove = null
mouseleave = null
x = null
y = null

//Set some initial Numbers
var margin = options.margin,
    barPadding = options.barPadding,
    width = width-(2*margin),
    height = height-(2*margin),
    barWidth = Math.floor(width/data.length),
    xmax = d3.max(data, function(d) { return d.Var; }),
    xmin = d3.min(data, function(d) { return d.Var; }),
    ymax = d3.max(data, function(d) { return d.Number; });
 
//Create a tooltip
var Tooltip = d3.select('#htmlwidget_container')
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
        .attr('fill', 'rgba(0,0,139,0.5)');
};
var mousemove = function(event, d) {
    Tooltip
        /*.html('Variable ' + d.Var + ': ' + d.Number)*/
        .html(d.Var + ': ' + d.Number)
        /*.style("left", (d3.pointer(this)[0]+100) + "px")
        .style("top", (d3.pointer(this)[1]+10) + "px");*/
        .style("left", (event.pageX + 30) + "px") 
        .style("top", (event.pageY) + "px")
        /*.style("left", "200px")
        .style("top", "200px");*/
};
var mouseleave = function(d) {
    Tooltip
        .style("opacity", 0);
    d3.select(this)
        .attr('fill', options.colour);
};
    
    
//Transition animation on load
svg.selectAll('rect')
    .transition()
    .delay(function(d,i){return (i*100);})
    .duration(function(d,i){return (1000+(i*200));})
    .attr('height', function(d) { return d.value/ymax * height; })
    .attr('y', function(d) { return (height+margin-(d.value/ymax * height)); });
    
    
//Create the chart
svg.selectAll('rect')
   .data(data)
   .enter()
   .append('rect')
   .attr('height', function(d) { return d.Number/ymax * height; })
   .attr('width', barWidth-barPadding)
   .attr('x', function(d, i) { return (margin+(i * barWidth)); })
   .attr('y', function(d) { return (height+margin-(d.Number/ymax * height)); })
   .attr('fill', options.colour);
   
//add tooltip to chart   
   svg.selectAll('rect')
    .on("mouseover", mouseover)
    .on("mousemove", mousemove)
    .on("mouseleave", mouseleave);
   
       
//Create the x axis
var x = d3.scaleBand()
          .domain(data.map(function(d) { return d.Var; }))
          .range([0, width-barPadding]);
svg.append("g")
  .attr("transform", "translate(" + margin + "," + (height+margin) + ")")
  .call(d3.axisBottom(x));
svg.append("text")             
  .attr("transform", "translate(" + (width/2) + " ," + (height+2*margin) + ")")
  .attr("dx", "1em")
  .style("text-anchor", "middle")
  .style("font-family", "Tahoma, Geneva, sans-serif")
  .style("font-size", "12pt")
  .text(options.xLabel);

//Create the y axis
var y = d3.scaleLinear()
          .range([height, 0])
          .domain([0, ymax]);
svg.append("g")
  .attr("transform", "translate(" + margin + ", " + margin + ")")
  .call(d3.axisLeft(y));
svg.append("text")
  .attr("transform", "translate(" + 0 + " ," + ((height+2*margin)/2) + ") rotate(-90)")
  .attr("dy", "1em")
  .style("text-anchor", "middle")
  .style("font-family", "Tahoma, Geneva, sans-serif")
  .style("font-size", "12pt")
  .text(options.yLabel);

//Create the chart title
svg.append("text")
  .attr("x", (width / 2))             
  .attr("y", (margin/2))
  .attr("text-anchor", "middle")
  .attr("dx", "1em")
  .style("font-size", "16pt")
  .style("font-family", "Tahoma, Geneva, sans-serif")
  .text(options.chartTitle);