(function($) {
	window.ENMGrid = function(config) {
		if (!config.target || !config.target.length) return;
		this.built = false;
		this.config = config;
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
				align: "center",
				sortable: false
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
			url: self.config.dataUrl,
			datatype: "json",
			gridview: true,
			autowidth: true,
			colModel: columnModel,
			rowNum: 10000,
			//rowList: [10, 20, 30],
			//pager: this.config.pager,
			//viewrecords: true,
			//sortorder: "desc",
			caption: self.config.caption,
			gridComplete: function() {
				var els = target.find("tr.jqgrow");
				if (self.config.nodeClickHandler) {
					els.find("td:first").css("cursor", "pointer");
					var id = $(els[0]).attr("id");
					target.jqGrid("setSelection", id, false);
					self.config.nodeClickHandler(id);
				}
			},
			onCellSelect: function(rowId, columnId, cellContent, element) {
				if (columnId == 0) {
					if (self.config.nodeClickHandler) {
						self.config.nodeClickHandler(rowId);
						return;
					}
				}
				if (!$(element.target).hasClass("enm-extended-api")) return;
				//console.log([rowId, columnModel[columnId].name]);
			}
		});
		//target.jqGrid("navGrid", this.config.pager, {edit: false, add: false, del: false});
	};

	ENMGrid.prototype.reload = function() {
		this.config.target.trigger("reloadGrid", [{current: true}]);
	};
})(jQuery);
