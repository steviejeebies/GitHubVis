function drawBarChart(jsonData) {
	// BAR CHART
	// set the dimensions and margins of the graph
	var barchart_margin = {top: 30, right: 30, bottom: 70, left: 60},
		barchart_width = 460 - barchart_margin.left - barchart_margin.right,
		barchart_height = 400 - barchart_margin.top - barchart_margin.bottom;

	// append the svg object to the body of the page
	d3v4.select("#barchart_div").remove()
	d3v4.select("#barchart_container").append("div").attr("id", "barchart_div")
	var barchart_svg = d3v4.select("#barchart_div")
	.append("h1")
		.html(jsonData.name)
	.append("svg")
		.attr("width", barchart_width + barchart_margin.left + barchart_margin.right)
		.attr("height", barchart_height + barchart_margin.top + barchart_margin.bottom)
	.append("g")
		.attr("transform",
			"translate(" + barchart_margin.left + "," + barchart_margin.top + ")");

	// Parse the Data
	d3v4.csv("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/7_OneCatOneNum_header.csv", function(data) {

	// sort data
	data.sort(function(b, a) {
		return a.Value - b.Value;
	});

	// X axis
	var x = d3v4.scaleBand()
		.range([ 0, barchart_width ])
		.domain(data.map(function(d) { return d.Country; }))
		.padding(0.2);
	barchart_svg.append("g")
		.attr("transform", "translate(0," + barchart_height + ")")
		.call(d3v4.axisBottom(x))
		.selectAll("text")
		.attr("transform", "translate(-10,0)rotate(-45)")
		.style("text-anchor", "end");

	// Add Y axis
	var y = d3v4.scaleLinear()
		.domain([0, 13000])
		.range([ barchart_height, 0]);
	barchart_svg.append("g")
		.call(d3v4.axisLeft(y));

	// Bars
	barchart_svg.selectAll("mybar")
		.data(data)
		.enter()
		.append("rect")
		.attr("x", function(d) { return x(d.Country); })
		.attr("y", function(d) { return y(d.Value); })
		.attr("width", x.bandwidth())
		.attr("height", function(d) { return barchart_height - y(d.Value); })
		.attr("fill", "#69b3a2")

	})
}