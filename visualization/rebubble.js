// set the dimensions and margins of the graph
const margin = {top: 30, right: 30, bottom: 30, left: 30},
	height = 820 - margin.top - margin.bottom,
	width = height*0.8225965844943084;

// append the svg object to the body of the page
const svg = d3.select("#nl_bubbles")
	.append("svg")
		.attr("width", width + margin.left + margin.right)
		.attr("height", height + margin.top + margin.bottom)
	.append("g")
		.attr("transform", `translate(${margin.left},${margin.top})`);

// initial matrix (Euclidean)
var kMatrix = [];
d3.csv("https://raw.githubusercontent.com/mateokazuo/sterfte/refs/heads/main/tabel_10k_euc.csv").then(data => {
	kMatrix = data.map(row => {
		return Object.values(row).map(cell => parseFloat(cell));
	});
	second();
});

var sMatrix = [];
function second() {
	d3.csv("https://raw.githubusercontent.com/mateokazuo/sterfte/refs/heads/main/tabel_SMR_euc.csv").then(data => {
		sMatrix = data.map(row => {
			return Object.values(row).map(cell => parseFloat(cell));
		});
		bubbles();
	});
}

function linker10k(mth) {
	return "https://raw.githubusercontent.com/mateokazuo/sterfte/refs/heads/main/tabel_10k_"+mth+".csv";
}
function linkerSMR(mth) {
	return "https://raw.githubusercontent.com/mateokazuo/sterfte/refs/heads/main/tabel_SMR_"+mth+".csv";
}

const scaleFill = d3.scaleLinear()
	.domain([0, 1])
	.range(["#0000ff", "#ff0000"]);
function colorir(i, vec) {
	let cols = vec.map(v => scaleFill(v));
	cols[i] = "#FF7F00";
	return cols;
}

const scaleEdge = d3.scaleLinear()
	.domain([0, 1])
	.range(["#000077", "#770000"]);
function coledge(i, vec) {
	let cols = vec.map(v => scaleEdge(v));
	cols[i] = "#994D00";
	return cols;
}

function bubbles() {
	d3.csv("https://raw.githubusercontent.com/mateokazuo/sterfte/refs/heads/main/sterfte_meta.csv").then( function(data) {
		// build gemeente selector
		const T = 300;
		d3.select("#selector")
			.selectAll('option')
				.data(data)
			.enter()
				.append('option')
			.text(function (d) {return d.name;})
			.attr("value", function (d) {return d.index;});
		d3.select("#selector")
			.property("value","108");

		// build method selector
		const methods = ["Euclidean", "Cosine", "Chi-square", "Hellinger", "Jensen-Shannon"];
		const vals = ["euc", "cos", "chi", "hel", "jsd"];
		d3.select("#measure")
			.selectAll('option')
				.data(methods)
			.enter()
				.append('option')
			.text(function (d) {return d;})
			.attr("value", function (d, i) {return vals[i];});

		// initial metric, method
		var metric = "10k_";

		// define axes
		const x = d3.scaleLinear()
			.domain([0, 1])
			.range([ 0, width ]);
		const y = d3.scaleLinear()
			.domain([0, 1])
			.range([ height, 0]);
		
		// size scaling
		const z = d3.scaleLinear()
			.domain([0, 1])
			.range([ 4, 30]);

		// make tooltip
		const tooltip = d3.select("body")
			.append("div")
				.style("opacity", 0)
				.attr("class", "tooltip")
				.style("background-color", "black")
				.style("border-radius", "5px")
				.style("padding", "10px")
				.style("color", "white")
				.style("position", "absolute");

		  // show / move / hide tooltip
		const showTooltip = function(event, d) {
			tooltip
				.style("opacity", 1)
				.style("background-color", colors[d.index])
				.style("border", "2px solid " + cedges[d.index])
				.html("Gemeente: " + d.name+"<br>10k: " + Math.round(kalues[d.index]*100)/100+" SMR: "+ Math.round(salues[d.index]*100)/100)
				.style("left", (event.pageX + 10) + "px")
				.style("top", (event.pageY + 10) + "px")
			}
		const moveTooltip = function(event, d) {
			tooltip
				.style("left", (event.pageX + 10) + "px")
				.style("top", (event.pageY + 10) + "px")
			}
		const hideTooltip = function(event, d) {
			tooltip
				.style("opacity", 0)
				.html("")
			}

		// initial state
		var picked = 108;
		var kimils = kMatrix;
		var simils = sMatrix;
		var kalues = kimils[picked];
		var salues = simils[picked];
		var colors = colorir(picked, kalues);
		var cedges = coledge(picked, kalues);
		// create initial plot
		svg.append('g')
			.selectAll("dot")
			.data(data)
			.join("circle")
				.attr("cx", d => x(d.xPos))
				.attr("cy", d => y(d.yPos))
				.attr("r", (d,j) => z(salues[j]))
				.style("fill", (d, j) => colors[j])
				.style("opacity", "0.7")
				.attr("stroke", (d, j) => cedges[j])
				.style("stroke-width", "1px")
				.attr("class", "bubble")
				// click event triggers updates
				.on("click", function(event, d) {
					const index = d3.select(this).data()[0].index; // get index
					update(index); // update circles
					updateTooltip(d, index); // and tooltip
					d3.select("#selector").node().value = index; // AND dropdown
				})
			// events call show / move / hide tooltip
			.on("mouseover", showTooltip )
			.on("mousemove", moveTooltip )
			.on("mouseleave", hideTooltip );

		// legends
		const valuesToShow = [1, 0.5,0];
		const xCircle = 0+30;
		const xLabel = 50+30;
		const yCircle = height-30;
		svg
			.selectAll("legend")
			.data(valuesToShow)
			.enter()
			.append("circle")
				.attr("cx", xCircle)
				.attr("cy", function(d){ return yCircle - z(d) } )
				.attr("r", function(d){ return z(d) })
				.style("fill", "none")
				.attr("stroke", "black");

		// Add legend: segments
		svg
			.selectAll("legend")
			.data(valuesToShow)
			.enter()
			.append("line")
				.attr('x1', function(d){ return xCircle + z(d) } )
				.attr('x2', xLabel)
				.attr('y1', function(d){ return yCircle - z(d) } )
				.attr('y2', function(d){ return yCircle - z(d) } )
				.attr('stroke', 'black')
				.style('stroke-dasharray', ('2,2'));

		// Add legend: labels
		svg
		  .selectAll("legend")
		  .data(valuesToShow)
		  .enter()
		  .append("text")
			.attr('x', xLabel)
			.attr('y', function(d){ return yCircle - z(d) } )
			.text( function(d){ return d } )
			.style("font-size", 10)
			.attr('alignment-baseline', 'middle');
		svg
			.append("text")
				.attr('x',15)
				.attr('y', height-70)
				.text("SMR");

		// color legend
		const legScale = d3.scaleLinear()
			.domain([0,1])
			.range([0,300]);
		const ticks = d3.axisTop()
			.scale(legScale)
			.ticks(3)
			.tickValues([0,0.5,1]);
		const defs = svg.append("defs");
		const linearGradient = defs.append("linearGradient")
			.attr("id", "linear-gradient");
		linearGradient
			.attr("x1", "0%")
			.attr("y1", "0%")
			.attr("x2", "100%")
			.attr("y2", "0%");
		//Set the color for the start (0%)
		linearGradient.append("stop")
			.attr("offset", "0%")
			.attr("stop-color", "#0000ff");

		//Set the color for the end (100%)
		linearGradient.append("stop")
			.attr("offset", "100%")
			.attr("stop-color", "#ff0000");

		svg.append("rect")
			.attr('x', 0)
			.attr('y', height)
			.attr("width", 300)
			.attr("height", 20)
			.style("fill", "url(#linear-gradient)");
		svg.append("g")
			.attr("transform", "translate(0," + (height) + ")")
			.call(ticks);
		svg
			.append("text")
				.attr('x',140)
				.attr('y', height+15)
				.attr("fill", "white")
				.text("10k");
		// update current tooltip on click
		const updateTooltip = function(d, gem) {
			tooltip
				.html("Gemeente: " + d.name+"<br>10k: " + kimils[gem][gem]+" SMR: "+ simils[gem][gem])
				.style("border", "2px solid " + "#994D00")
				.transition()
				.duration(T)
				.style("background-color", "#FF7F00")
			}

		// change 10k similarity matrix
		function update1() {
			d3.csv(linker10k(method)).then(data => {
				kimils = data.map(row =>
					Object.values(row).map(parseFloat)
				);
				update2();
			});
		}
		// measure dropdown change loads new matrices
		d3.select("#measure").on("change", function() {
			method = this.value;
			update1();
		});

		// change SMR similarity matrix
		function update2() {
			d3.csv(linkerSMR(method)).then(data => {
				simils = data.map(row =>
					Object.values(row).map(parseFloat)
				);
				update(picked);
			});
		}

		// update circles
		function update(gem) {
			picked = gem;
			kalues = kimils[picked];
			salues = simils[picked];
			colors = colorir(picked, kalues);
			cedges = coledge(picked, kalues);
			svg.selectAll("circle")
				.filter(function() {return this.classList.contains("bubble")})
				.transition()
				.duration(T)
				.ease(d3.easeCubic)
				.attr("r", (d, i) => z(salues[i]))
				.style("fill", (d, i) => colors[i])
				.attr("stroke", (d, i) => cedges[i])
				.style("opacity", "0.7");
		}
		// gemeente dropdown change triggers circle update
		d3.select("#selector").on("change", function() {
			const selectedIndex = +this.value; // convert to number
			update(selectedIndex);
		});
	});
}