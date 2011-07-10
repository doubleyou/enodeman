(function($) {
	window.ENMTree = function(config) {
		this.build = false;
		this.config = $.extend({}, config);
		this.init();
	};

	ENMTree.prototype.init = function() {
		var self = this;
		$.get(this.config.dataURL, {}, function(data) {
			self.data = data;
		}, "jsonp")
	};

	ENMTree.prototype.show = function(data) {
		if (this.build) {
			return;
		}
		var self = this;
		var targetId = this.config.targetId;
		var cfg = this.config;
		this.tree = new $jit.ST({
			injectInto: targetId,
			duration:cfg.duration,
			transition: $jit.Trans.Quart.easeInOut,
			levelDistance: cfg.levelDistance,
			Navigation: {
				enable:true,
				panning:true
			},
		 	Node: {
				height: cfg.node.height,
			 	width: cfg.node.width,
				type: cfg.node.type,
				color: cfg.node.color,
			 	overridable: true
			},
	        	Edge: {
				type: cfg.edge.type,
				overridable: true
			},
			onBeforeCompute: function(node){
			},
			onAfterCompute: function(){
			},
			onCreateLabel: function(label, node) {
				self.onCreateLabel(label, node);
			},
	   		onBeforePlotNode: self.onBeforePlotNode,
					        
			onBeforePlotLine: self.onBeforePlotLine,
		});
		this.tree.loadJSON(data);
		this.tree.compute();
		this.tree.geom.translate(new $jit.Complex(-200, 0), "current");
		this.tree.onClick(this.tree.root);
		this.build = true;
	};

	ENMTree.prototype.onCreateLabel = function(label, node) {
		var self = this;
		var label = $(label);
		label.get(0).id = node.id;           
		label.html(node.name);
		label.click(function(){
			self.tree.onClick(node.id);
		});
		label.css({
			"width": 60,
			"heigth": 17,
			"cursor": "pointer",
			"color": "#333",
			"font-size": ".8em",
			"text-align": "center",
			"padding-top": 3
		});
	};

	ENMTree.prototype.onBeforePlotNode = function(node) {
		
		if (node.selected) {
			node.data.$color = "#ff7";
		} else {
			delete node.data.$color;
			if(!node.anySubnode("exist")) {
				var count = 0;
				node.eachSubnode(function(n) { count++; });
				node.data.$color = ['#aaa', '#baa', '#caa', '#daa', '#eaa', '#faa'][count];                    
			}
		}
	};

	ENMTree.prototype.onBeforePlotLine = function(adj) {
		
		if (adj.nodeFrom.selected && adj.nodeTo.selected) {
			adj.data.$color = "#eed";
			adj.data.$lineWidth = 3;
		} else {
			delete adj.data.$color;
			delete adj.data.$lineWidth;
		}
	};
	ENMTree.prototype.reload = function() {
		var self = this;
		this.init();
		if (this.tree) this.tree.onClick(this.tree.root);
	};
})(jQuery);
