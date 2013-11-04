<!doctype>
<head>
        <meta charset="UTF-8">
        
        <script src="../d3/d3.v3.js"></script>
	<script src="../d3/rickshaw.js"></script>
        <script src="../d3/jquery.js"></script>
</head>
<body>

<div id="chart_container">
	<div id="chart"></div>
</div>


<script type="text/javascript">

 var margin = {top:30, right: 40, bottom:30, left: 50},
	width = 500 - margin.left - margin.right,
	height = 125 - margin.top - margin.bottom;

 var parseDate = d3.time.format("%Y-%m-%d").parse;

 var x = d3.time.scale().range([0, width]);
 var y = d3.scale.linear().range([height,0]);

 var xAxis = d3.svg.axis().scale(x).orient("bottom").ticks(5);
 var yAxis = d3.svg.axis().scale(y).orient("left").ticks(5);

 var valueline = d3.svg.line()
	.x(function(d) { return x(d.speechDate); })
	.y(function(d) { return y(+d.topicProportion); })
        .interpolate("curve");

 var svg = d3.select("body")
	.append("svg")
		.attr("width", width + margin.left + margin.right)
		.attr("height", height + margin.top + margin.bottom)
	.append("g")
		.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

	

 d3.csv("../GreenspanData/topic_7_market_future_price.csv", function(error, data) {   
	data.forEach(function(d) {  
        	 d.speechDate = parseDate(d.speechDate);
		 d.topicProportion = +d.topicProportion
   }); 

   // Scale the range of the data
   x.domain(d3.extent(data, function(d) { return d.speechDate; }));
   y.domain([0, d3.max(data, function(d) { return +d.topicProportion; })]);

   svg.append("path") // Add valueline path
      .datum(data)
      .style("fill", "none")
      .style("stroke","steelblue")
      .style("stroke-width","1px")
      .attr("class","line")
      .attr("d", valueline);

   svg.append("g")
	.attr("class", "x axis")
	.attr("transform", "translate(0," + height + ")")
	.call(xAxis);

   svg.append("g")
	.attr("class", "y axis")
	.call(yAxis);

});

</script>


</body>
