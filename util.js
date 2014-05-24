var util__color1 = '#757CB8';
var util__color2 = '#B8759D';
var util__color3 = '#B8B175';
var util__color4 = '#75B890';

var util__colors = [util__color1,util__color2,util__color3,util__color4];
function getContext(name) {
    var canvas = document.getElementById(name);
    var context = canvas.getContext('2d');
    return context;
}

function drawBar(canvasName, w, x, y, z) {
    var context = getContext(canvasName);
    context.beginPath();
    context.fillStyle = util__color1;
    context.fillRect(0, 0,w, 16);
    context.fillStyle = util__color2;
    context.fillRect(w, 0, x, 16);
    context.fillStyle = util__color3;
    context.fillRect(w + x, 0, y, 16);
    context.fillStyle = util__color4;
    context.fillRect(w + x + y, 0, z, 16);
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
