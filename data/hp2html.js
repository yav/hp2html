$(function () {
  var xmin = null, xmax = null, ymin = null, ymax = null; // view
  var placeholder = $("#placeholder");
  var data = getData();

  function suffixFormatter(val, axis) {
    var kb = 1024, mb = 1024 * kb;

    if (val > mb)
      return (val / mb).toFixed(axis.tickDecimals) + " MB";
    else if (val > kb)
      return (val / kb).toFixed(axis.tickDecimals) + " kB";
    else
      return val.toFixed(axis.tickDecimals) + " B";
  }

  function plotWithOptions() {
    var stack = ($("input:radio[name=view]:checked").val() == "stack") ?
                                                                true : null;
    var usePoints = xmax && xmin && (xmax - xmin) < 5;

    $.plot(placeholder, data,
      { series: { stack: stack
                , lines: { show: true, fill: true }
                }
      , points: { show: usePoints }
      , legend: { show: false }
      , grid: { hoverable: true }
      , xaxis: { min: xmin, max: xmax }
      , yaxis: { tickFormatter: suffixFormatter, min: ymin, max: ymax }
      , selection: { mode: "xy" }
      });
    }

  $(".stackControls input").click(function (e) { plotWithOptions(); });

  placeholder
    .dblclick(function (e) {
      xmin = xmax = ymin = ymax = null;
      plotWithOptions();
    })
    .bind("plothover", function (event, pos, item) {
        if (item) $("#point").text(item.series.label);
     })
    .bind("plotselected", function (event, ranges) {
        // clamp the zooming to prevent eternal zoom
        if (ranges.xaxis.to - ranges.xaxis.from < 0.00001)
            ranges.xaxis.to = ranges.xaxis.from + 0.00001;
        if (ranges.yaxis.to - ranges.yaxis.from < 0.00001)
            ranges.yaxis.to = ranges.yaxis.from + 0.00001;
        xmin = ranges.xaxis.from; xmax = ranges.xaxis.to;
        ymin = ranges.yaxis.from; ymax = ranges.yaxis.to;
        plotWithOptions();
     });

  plotWithOptions();
});

