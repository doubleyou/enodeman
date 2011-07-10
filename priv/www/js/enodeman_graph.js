(function($) {
	window.ENMGraph = function(config) {
		this.offsets = {
			top: 20,
			right: 10,
			bottom: 10,
			left: 60
		};
		this._curIndex = 0;
		this.config = config;
		this.createPaper();
	};

	ENMGraph.prototype.createPaper = function() {
		if (!this.paper) {
			this.paper = Raphael(this.config.targetId, this.config.width, this.config.height);
			this.init();
			this.paper.rect(0, 0, this.config.width, this.config.height);
			this.paper.g.text(130, 10, this.config.caption);
		}
	};

	ENMGraph.prototype.init = function() {
		this.paper.clear();
		this.titles = [];
		this.lines = {
			xvalues: [],
			yvalues: []
		};
		this.colors = [];
	};

	ENMGraph.prototype.add = function(title, xs, ys, multipart) {
		var self = this;
		if (xs[0] instanceof Array) {
			$.each(xs, function(i, xv) {
				self.add(title, xv, ys[i], true);
			});
			this._curIndex++;
		} else {
			this.lines.xvalues.push(xs);
			this.lines.yvalues.push(ys);
			this.colors.push(Raphael.fn.g.colors[this._curIndex]);
			if (!multipart) {
				this._curIndex++;
			}
		}
		if (this._curIndex >= Raphael.fn.g.colors.length) {
			this._curIndex = 0;
		}
	};

	ENMGraph.prototype.draw = function() {
		var self = this;
		var lines = this.paper.g.linechart(
			this.offsets.left,
			this.offsets.top,
			this.config.width - this.offsets.left - this.offsets.right,
			this.config.height - this.offsets.top - this.offsets.bottom,
			this.lines.xvalues, this.lines.yvalues,
			{nostroke: false, axis: "0 0 0 1", symbol: "o", smooth: true, colors: this.colors}
		).hoverColumn(function () {
			this.tags = self.paper.set();
			for (var i = 0, len = this.y.length; i < len; i++) {
				var label = this.values[i];// + "\n" + self.config.xTranslator(self.lines.xvalues[i]);
				this.tags.push(
					self.paper.g.tag(this.x, this.y[i], label, 160, 10)
					.insertBefore(this)
					.attr([
					       {fill: "#fff"},
					       {fill: this.symbols[i].attr("fill")}
					])
				);
			}
		}, function () {
			this.tags && this.tags.remove();
		});
		lines.symbols.attr({r: 3});
	};
})(jQuery);
