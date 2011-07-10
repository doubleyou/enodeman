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
			onCreateLabel: function(label, node){
				label.id = node.id;           
				label.innerHTML = node.name;
				label.onclick = function(){
					self.tree.onClick(node.id);
				};
				var style = label.style;
				style.width = 60 + 'px';
				style.height = 17 + 'px';     
				style.cursor = 'pointer';
				style.color = '#333';
				style.fontSize = '0.8em';
				style.textAlign= 'center';
				style.paddingTop = '3px';
	 		},
	   		onBeforePlotNode: function(node) {
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
			},
					        
			onBeforePlotLine: function(adj){
				if (adj.nodeFrom.selected && adj.nodeTo.selected) {
					adj.data.$color = "#eed";
					adj.data.$lineWidth = 3;
				} else {
					delete adj.data.$color;
					delete adj.data.$lineWidth;
				}
			}
		});
		self.tree.loadJSON(data);
		self.tree.compute();
		self.tree.geom.translate(new $jit.Complex(-200, 0), "current");
		self.tree.onClick(self.tree.root);
		this.build = true;
	};
})(jQuery);
