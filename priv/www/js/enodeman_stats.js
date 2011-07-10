(function($) {
	window.ENMStats = function(node) {
		var graphsEl = $("#enm-graphs").empty();
		$.get("/" + node + "/stats", {}, function(stats) {
			$.each(stats, function(i, stat) {
				var xs = [];
				var ys = [];
				$.each(stat.data, function(j, part) {
					xs[j] = [];
					for (k = 0; k < part.stats.length; k++) {
					    xs[j].push(parseInt(part.start_time + k * part.interval) % 1000);
					}
					ys[j] = [];
					for (k = 0 ; k < part.stats.length; k++) {
					    var v = part.stats[k];
					    ys[j].push((v / (1024 * 1024)).toFixed(1));
					}
				});
				var id = "metric_" + stat.metric;
				graphsEl.append('<div id="' + id + '" class="enm-graph"></div>');
				var graph = new ENMGraph({
					targetId: id,
					width: 900,
					height: 300,
					caption: node + ": " + stat.metric/*,
					xTranslator: function(x) {
						return new Date(x).toTimeString();
					}*/
				});
				graph.add(
					stat.metric,
					xs,
					ys
				);
				graph.draw();
			});
		}, "jsonp");
	};
})(jQuery);
