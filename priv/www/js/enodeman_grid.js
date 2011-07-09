(function($) {
	var columnModelParams = [
		{width: 70},
		{width: 90},
		{width: 100},
		{width: 80, align: "right"},
		{width: 80, align: "right"},
		{width: 80, align: "right"},
		{width: 150, sortable: false}
	];
	var built = false;

	window.ENMGrid = function(config) {
		this.config = $.extend({}, config);
		this.config.targetId = this.config.target.attr("id");
		this.init();
	};

	ENMGrid.prototype.init = function() {
		var self = this;
		$.get(this.config.columnsSpecUrl, {}, function(columnsSpec) {
			self.show(columnsSpec);
		}, "jsonp");
	};

	ENMGrid.prototype.show = function(columnsSpec) {
		var self = this;
		var columnModel = [];
		$.each(columnsSpec.ids, function(i, id) {
			var params = $.extend({name: id, index: id}, columnModelParams[i]);
			columnModel.push(params);
		});
		if (built) {
			this.reload();
			return;
		}
		built = true;
		var target = this.config.target;
		target.jqGrid({
			url: this.config.dataUrl,
			datatype: "json",
			colNames: columnsSpec.titles,
			colModel: columnModel,
			//rowNum: 10,
			//rowList: [10, 20, 30],
			//pager: this.config.pager,
			sortname: columnsSpec.ids[0],
			viewrecords: true,
			sortorder: "desc",
			caption: this.config.caption,
			gridComplete: function() {
				target.find(".jqgrow td").click(function() {
					var el = $(this);
					var rowId = el.parent().attr("id");
					var columnId = el.attr("aria-describedby").replace(self.config.targetId + "_", "");
					console.log([rowId, columnId]);
				});
			}
		});
		//target.jqGrid("navGrid", this.config.pager, {edit: false, add: false, del: false});
	};

	ENMGrid.prototype.reload = function() {
		this.config.target.trigger("reloadGrid", [{current:true}]);
	};
})(jQuery);
