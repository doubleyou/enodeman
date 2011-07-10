(function($) {
	window.ENMGrid = function(config) {
		this.built = false;
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
		if (this.built) {
			this.reload();
			return;
		}
		var self = this;
		var columnModel = [];
		$.each(columnsSpec, function(i, column) {
			var params = {
				name: column.id,
				index: column.id,
				title: false,
				align: "center"
			};
			if (column.api) {
				params.cellattr = function() {
					return 'class="enm-extended-api"';
				}
			}
			columnModel.push(params);
		});
		this.built = true;
		var target = this.config.target;
		target.jqGrid({
			url: this.config.dataUrl,
			datatype: "json",
			gridview: true,
			autowidth: true,
			colNames: columnsSpec.titles,
			colModel: columnModel,
			//rowNum: 10,
			//rowList: [10, 20, 30],
			//pager: this.config.pager,
			//viewrecords: true,
			sortorder: "desc",
			caption: this.config.caption,
			gridComplete: function() {
				target.find(".jqgrow td.enm-extended-api").click(function() {
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
		this.config.target.trigger("reloadGrid", [{current: true}]);
	};
})(jQuery);
