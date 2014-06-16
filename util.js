var util__colors = [
    '#6F79D9',
    '#D86EAE',
    '#D8CE6E',
    '#6ED899'
];
function getContext(name) {
    var canvas = document.getElementById(name);
    var context = canvas.getContext('2d');
    return context;
}

function sum(arr) {
    return arr.reduce(function(x, y) { return x + y; }, 0);
}

function drawBar(canvasName, w, x, y, z) {
    if (w + x + y + z === 0) {
        w = 1;
    }
    var context = getContext(canvasName);
    var total = w + x + y + z;
    var w_ = 100*w/total;
    var x_ = 100*x/total;
    var y_ = 100*y/total;
    var z_ = 100*z/total;
    var vars = [w_, x_, y_, z_]
    context.beginPath();
    var colors = util__colors;
    var numColors = Math.min(colors.length, vars.length);
    for (var i = 0; i < numColors; i += 1) {
        context.fillStyle = colors[i];
        var x = sum(vars.slice(0, i));
        var width = vars[i];
        context.fillRect(x, 0, width, 16);
    }
}

function formatLine(label) {
    return function() {
        return '<b>'+ this.x +'</b>: '+ this.point.y + ' ' + label ;
    };
}

function line(canvasName, values, labels, tooltipLabel) {
    $(function () {
        $('#' + canvasName).highcharts({
            chart: {
                backgroundColor: '#404040',
                width: 500,
                height: 200,
                marginTop: 50,
                paddingRight:0,
                borderRadius: 10
            },
            plotOptions: {
                series: {
                    marker : { enabled: true }
                },
                line: {
                    animation: false,
                    states: { hover: { enabled: false } }
                }
            },
            credits: false,
            exporting: false,
            tooltip: formatTooltip(formatLine(tooltipLabel)),
            title: {
                style : { color: '#A2A4BA' },
                text: null,
                x: -20
            },
            xAxis: {
                labels: { style: { color: '#A2A4BA'} },
                categories: labels,
                gridLineWidth: 0,
                minorGridLineWidth: 0,
                tickWidth: 0,
                tickLength: 0,
                lineWidth: 0
            },
            yAxis: {
                labels: { style: { color: '#A2A4BA'} },
                title: { text: null },
                gridLineWidth: 0,
                lineWidth: 0,
                minorGridLineWidth: 1,
                floor: 0
            },
            legend: {
                enabled:false,
                layout: 'vertical',
                align: 'right',
                verticalAlign: 'middle',
                borderWidth: 0
            },
            series: [{
                color: '#D8CE6E',
                name: 'Tokyo',
                data: values
            }]
        });
    });

}

function checkOverflow(el)
{
   var curOverflow = el.style.overflow;
   if ( !curOverflow || curOverflow === "visible" )
      el.style.overflow = "hidden";

   var isOverflowing = el.clientWidth < el.scrollWidth
      || el.clientHeight < el.scrollHeight;

   el.style.overflow = curOverflow;

   return isOverflowing;
}

function triggerPointer() {
    var div = ".testtest";
    $(div).hover(function() {
        if (checkOverflow(this)) {
            $(div).css('cursor', 'pointer');
        } else {
            $(div).css('cursor', 'default');
        }
    }, function() {
        $(div).css('cursor', 'default');
    });
}

function formatWeekDay(label) {
    var t = this.point.name;
    return '<b>'+ t +'</b>: '+ this.point.y.toFixed(2) + '%';
}

function halfDonut(canvasName, elts) {
    $(function () {
        $('#' + canvasName).highcharts({
            chart: {
                backgroundColor: '#404040',
                plotBorderWidth: 0,
                plotShadow: true,
                width: 500,
                height: 500,
                borderRadius: 10
            },
            colors : ['#F7977A', '#FDC68A', '#FFF79A', '#A2D39C', '#6ECFF6', '#8493CA', '#A187BE'],
            credits: false,
            exporting: false,
            title: {
                enabled:false,
                text:""
            },
            tooltip: formatTooltip(formatWeekDay),
            plotOptions: {
                pie: {
                    animation: false,
                    states: { hover: { enabled: true } },
                    borderWidth: 0,
                    dataLabels: {
                        enabled: false,
                        distance: 0,
                        style: {
                            fontWeight: 'bold',
                            color: 'white',
                            textShadow: '0px 1px 0px rgba(0,0,0,.3)',
                            color: '#A2A4BA'
                        }
                    },
                    startAngle: -90,
                    endAngle: 90,
                    center: ['50%', '75%']
                }
            },
            series: [{
                type: 'pie',
                name: 'Hourly Activity',
                innerSize: '50%',
                data: elts
            }]
        });
    });
}
function formatTooltip(format) {
    return {
        backgroundColor: '#4A4A4A',
        formatter: format,
        borderColor: '#404040',
        borderRadius: 10,
        style: {
            color: '#A2A4BA'
        }

    };

}

function formatDonut() {
    var t = (parseInt(this.point.name) % 12) + 1;
    var m = parseInt(this.point.name) >= 12? "pm" : "am";
    return '<b>'+ t + ' ' + m +'</b>: '+ this.point.y.toFixed(2) + "%";
}

function donut(canvasName, elts) {
    $(function () {
        $('#' + canvasName).highcharts({
            chart: {
                backgroundColor: '#404040',
                plotBorderWidth: 0,
                margin:[0,0,0,0],
                plotShadow: true,
                width: 500,
                height: 500,
                borderRadius: 10
            },
            colors : ['#7A85EE', '#747FE3', '#6F79D9', '#6972CE', '#636CC3', '#5E66B8', '#B75D93', '#C2639C', '#CD68A5', '#D86EAE', '#E273B6', '#ED79BF', '#EDE279', '#E2D873', '#D8CE6E', '#CDC368', '#C2B963', '#B7AF5D', '#5DB782', '#63C289', '#68CD91', '#6ED899', '#73E2A0', '#79EDA8'],
            credits: false,
            exporting: false,
            title: {
                enabled:false,
                text:""
            },
            tooltip: formatTooltip(formatDonut),
            plotOptions: {
                pie: {
                    animation: false,
                    states: { hover: { enabled: true } },
                    borderWidth:0,
                    dataLabels: {
                        enabled: false,
                        distance: 0,
                        style: {
                            fontWeight: 'bold',
                            color: 'white',
                            textShadow: '0px 1px 0px rgba(0,0,0,.3)',
                            color: '#A2A4BA'
                        }
                    },
                    startAngle: 0,
                    endAngle: 360,
                    center: ['50%', '50%']
                }
            },
            series: [{
                type: 'pie',
                name: 'Hour',
                innerSize: '50%',
                data: elts
            }]
        });
    });
}
