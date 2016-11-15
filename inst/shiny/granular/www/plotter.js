	(function(){

		var networkOutputBinding = new Shiny.OutputBinding();

		$.extend(networkOutputBinding, {
			lines : 		{"counter":0, "brushed": false},
			lineHeight : 	40,
			margin : 		{top: 20, right: 80, bottom: 30, left: 50},
			width : 		500, 
			height: 		400,
			initialised: 	false,
			brush : 		null,
			x : 			null, 
			x0 : 			null, 
			y : 			null, 
			z : 			null, 
			line: 			null, 
			sample : 		null,
			samples : 		null,
			min : 			null,
			max : 			null,
			svg : 			null,
			hoverLineGroup: null,
			hoverLine : 	null ,
			hoverText: 		null,
			xAxis: 			null,
			yAxis: 			null,
			init: function(){
				if(this.initialised) return;
				this.brush = d3.brushX().extent([[0, 0], [this.width, this.height]]).on("end", this.brushended.bind(this));
				this.x = d3.scaleLog().range([0, this.width]);
				this.x0 = d3.scaleLog().range([0, this.width]);
				this.y = d3.scaleLinear().range([this.height, 0]); 
				this.z = d3.scaleOrdinal(d3.schemeCategory10);
				this.line = d3.line().curve(d3.curveBasis)
					.x( (function(d) { return this.x(d.psize); }).bind(this))
					.y( (function(d) { return this.y(d.frequency); }).bind(this));
				this.xAxis = d3.axisTop(this.x); 
				this.yAxis = d3.axisRight(this.y);

				this.initalised = true;
			},
			drawGraph: function(data){
				var g = this.svg.append("g").attr("transform", "translate(" + this.margin.left + "," + this.margin.top + ")");
				g = this.svg.append("g");

				g.append("g")
					.attr("class", "axis axis--x")
					.call(this.xAxis)
					.attr("transform", "translate(0," + (this.height) + ")")
					.append("text")
					.attr("fill", "#000")
					.text("Particle Size");

				g.append("g")
					.attr("class", "axis axis--y")
					.call(this.yAxis)
					.append("text")
					//.attr("transform", "rotate(-90)")
					.attr("y", 15)
					.attr("x",10)
					.attr("dy", "0.71em")
					.attr("fill", "#000")
					.text("Frequency");

				this.sample = g.selectAll(".sample")
					.data(this.samples)
					.enter().append("g")
					.attr("class", "sample");

				var that = this;
				this.sample.append("path")
					.attr("class", "line")
					.attr("d", (function(d) { return this.line(d.values); }).bind(this))
					.style("stroke", (function(d) { return this.z(d.id); }).bind(this));

				
				// Add mouseover events.
				d3.select("svg").on("mousemove", function() {
					var mouse_x = d3.mouse(this)[0];
					var graph_x = Math.round(that.x.invert(mouse_x)*100)/100;
				
					if(that.lines.counter >= 3 || !that.lines.brushed) return;
					var mouse_x = d3.mouse(this)[0];
					var graph_x = Math.round(that.x.invert(mouse_x)*100)/100;

					var char = "A", counter = 0;
					if(!that.lines.A){char="A"; }
					else if(!that.lines.B){char="B"; counter=1;}
					else if(!that.lines.C){char="C"; counter=2;}

					that.hoverLine.attr("x1", mouse_x).attr("x2", mouse_x);
					that.hoverText.text("("+char+") "+graph_x)
						.attr('y', 20 + that.lineHeight*counter);
					that.hoverText.attr('x', mouse_x + 10);
					that.hoverLineGroup.style("opacity", 1);

				}).on("mouseout", function() {
					that.hoverLineGroup.style("opacity", 1e-6);
				}).on('click', function(){
					if(that.lines.counter >= 3 || !that.lines.brushed) return;
					var mouse_x = d3.mouse(this)[0];
					var graph_x = Math.round(that.x.invert(mouse_x)*100)/100;

					var lineGroup = that.svg.append("g").attr("class", "hover-line");

					var char = "A", counter = 0;
					if(!that.lines.A){that.lines.A = lineGroup; char="A"; }
					else if(!that.lines.B){that.lines.B = lineGroup; char="B"; counter=1;}
					else if(!that.lines.C){that.lines.C = lineGroup; char="C"; counter=2;}

					var line = lineGroup
						.append("line")
						.attr("x1", mouse_x).attr("x2", mouse_x) 
						.attr("y1", 0).attr("y2", that.height); 

					var text = lineGroup.append('text')
					.attr("class", "hover-text")
					.attr('y', that.margin.top + that.lineHeight*counter)
					.attr('x', mouse_x + 10)
					.text("("+char+") "+graph_x);
							
					Shiny.onInputChange("peak_"+char, graph_x);

					text.on("dblclick", function(){
							lineGroup.remove();
							that.lines.counter--;
							delete that.lines[char];
						})
					that.lines.counter++;
				});	
			},
			brushended: function() {
				var s = d3.event.selection;

				//Only brush once
				if(this.lines.brushed) return;

				if (!s) {
					console.log("not s");
					return;
				} else {
					console.log("yes s:" + s);
					min = s[0];
					max = s[1];
					this.x.domain([s[0],s[1]].map(this.x.invert, this.x));
					Shiny.onInputChange("min_val", this.x0.invert(s[0]));
					Shiny.onInputChange("max_val", this.x0.invert(s[1]));
					this.svg.select(".brush").call(this.brush.move, null);
				}
				this.lines.brushed = true;
				this.zoom();
			},
			zoom: function () {
				var t = this.svg.transition().duration(750);
				this.svg.select(".axis--x").transition(t).call(this.xAxis);

				this.svg.selectAll(".line").transition(t)
					.attr("d", (function(d) { return this.line(d.values); }).bind(this));
			},
			find: function(scope) {
				this.init();
				return $(scope).find('.shiny-network-output');
			},
			renderValue: function(el, data) {
				d3.selectAll(".svg-container").remove();
				if(!data) return;
				d3.select("div#mastersizer")
					.append("div")
					.classed("svg-container", true) //container class to make it responsive
					.append("svg")
					//responsive SVG needs these 2 attributes and no width and height attr
					.attr("preserveAspectRatio", "xMinYMin meet")
					.attr("viewBox", "0 0 600 400")
					//class to make it responsive
					.classed("svg-content-responsive", true);
				
				// Initialise SVG and related variables
				this.svg = d3.select("svg");
				this.svg.selectAll("#sample").remove();
				this.svg.append("g")
					.attr("class", "brush")
					.call(this.brush);
				
				// Hover line. 
				this.hoverLineGroup = this.svg.append("g")
					.attr("class", "hover-line");

				this.hoverLine = this.hoverLineGroup
					.append("line")
					.attr("x1", 0).attr("x2", 0) 
					.attr("y1", 0).attr("y2", this.height); 

				this.hoverText = this.hoverLineGroup.append('text')
					.attr("class", "hover-text")
					.attr('y', this.margin.top);
				
				// Hide hover line by default.
				this.hoverLineGroup.style("opacity", 1e-6);

				this.samples = [];

				for(xx in data){
					if(xx=="size") continue;
					var myobj = {id: xx, values: []}
					for(var i=0; i<data.size.length; i++){
						myobj.values.push({psize: parseFloat(data.size[i]), frequency: data[xx][i]})
					}
					this.samples.push(myobj);
				}

				this.x.domain(d3.extent(data.size, function(d) { return d; }));
				this.x0.domain(d3.extent(data.size, function(d) { return d; }));

				this.y.domain( [
					d3.min(this.samples, function(c) { return d3.min(c.values, function(d) { return d.frequency; }); }),
					d3.max(this.samples, function(c) { return d3.max(c.values, function(d) { return d.frequency; }); })
				]);
				this.z.domain(this.samples.map(function(c) { return c.id; }));

				this.drawGraph(data);
			}

		})
		Shiny.outputBindings.register(networkOutputBinding, 'alexwhan.networkbinding');
	}());
