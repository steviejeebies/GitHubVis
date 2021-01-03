function drawHistogram(dataJSON) {
 // set the dimensions and margins of the graph
 var margin = {top: 10, right: 30, bottom: 30, left: 40},
     hist_width = 460 - margin.left - margin.right,
     hist_height = 400 - margin.top - margin.bottom;

// append the svg object to the body of the page
var hist_svg = d3v4.select("#histogram_div")
   .append("svg")
     .attr("width", hist_width + margin.left + margin.right)
     .attr("height", hist_height + margin.top + margin.bottom)
   .append("g")
     .attr("transform",
           "translate(" + margin.left + "," + margin.top + ")");

    var data = [{price:10}, {price:10}, {price:20}, {price:20}, {price:20}, {price:30}, {price:23}, {price:40}, {price:40}, {price:40}]

 // get the data
//  d3v4.csv("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/1_OneNum.csv", function(data) {
//      console.log(data)
   // X axis: scale and draw:
   var x = d3v4.scaleLinear()
       .domain([0, d3v4.max(data, function(d) { return (+d.price)+10 })])     // can use this instead of 1000 to have the max of data: )
       .range([0, hist_width]);
   hist_svg.append("g")
       .attr("transform", "translate(0," + hist_height + ")")
       .call(d3v4.axisBottom(x));

//   // set the parameters for the histogram
   var histogram = d3v4.histogram()
       .value(function(d) { return d.price; })   // I need to give the vector of value
       .domain(x.domain())  // then the domain of the graphic
       .thresholds(x.ticks(70)); // then the numbers of bins

   // And apply this function to data to get the bins
   var bins = histogram(data);

   // Y axis: scale and draw:
   var y = d3v4.scaleLinear()
       .range([hist_height, 0]);
       y.domain([0, d3v4.max(bins, function(d) { return d.length; })]);   // d3v4.hist has to be called before the Y axis obviously
   hist_svg.append("g")
       .call(d3v4.axisLeft(y));

//   // append the bar rectangles to the hist_svg element
   hist_svg.selectAll("rect")
       .data(bins)
       .enter()
       .append("rect")
         .attr("x", 1)
         .attr("transform", function(d) { return "translate(" + x(d.x0) + "," + y(d.length) + ")"; })
         .attr("width", function(d) { return x(d.x1) - x(d.x0) -1 ; })
         .attr("height", function(d) { return hist_height - y(d.length); })
         .style("fill", "#69b3a2")

}