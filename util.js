
var util__colors = [
    '#757CB8',
    '#B8759D',
    '#B8B175',
    '#75B890'
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
        console.log(x + " " + width);
        context.fillRect(x, 0, width, 16);
    }
}

function drawGraph(canvasName, xs) {
    var context = getContext(canvasName);
    var spacing = 5;
    var width = 16;

    var total = 0;
    var max = 0;
    var percents = []
    for (var i = 0; i < xs.length; i += 1) {
        total += xs[i];
        if (xs[i] > max) {
            max = xs[i];
        }

    }
    for (var i = 0; i < xs.length; i += 1) {
        var x = xs[i];
        percents[i] = (x / max) * 100;
    }

    for (var i = 0; i < xs.length; i += 1) {
        var x = i * (spacing + width);
        var y = 100 - percents[i] + 20;
        var color = util__colors[Math.floor(i/6)];
        var percentText = Math.floor((xs[i]*100/total)).toString() + "%";
        var iwidth = context.measureText(i).width;
        var pwidth = context.measureText(percentText).width;

        context.fillStyle = color;
        context.fillRect(x, y, width, percents[i]);
        context.fillStyle = '#A2A4BA';
        context.fillText(i, x + (width - iwidth)/2, 130);
        context.fillText(percentText, x + (width - pwidth)/2, y - 4);
    }
}
