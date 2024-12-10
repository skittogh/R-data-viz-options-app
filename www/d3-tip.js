/* Initialize tooltip */
  tip = d3.tip().attr('class', 'd3-tip').html(function(d) { return d; });
  
  /* Invoke the tip in the context of your visualization */
    vis.call(tip)
  
  vis.selectAll('rect')
  .data(data)
  .enter()
  .append('rect')
  .attr('width', function() { return x.rangeBand() })
  .attr('height', function(d) { return h - y(d) })
  .attr('y', function(d) { return y(d) })
  .attr('x', function(d, i) { return x(i) })
  .on('mouseover', tip.show)
  .on('mouseout', tip.hide)