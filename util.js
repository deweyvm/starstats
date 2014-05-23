function getContext(name) {
    var canvas = document.getElementById(name);
    var context = canvas.getContext('2d');
    return context;
}

function drawBar(canvasName, w, x, y, z) {
    var context = getContext(canvasName);
    context.beginPath();
    context.fillStyle = '#757CB8';
    context.fillRect(0, 0,w, 16);
    context.fillStyle = '#B8759D';
    context.fillRect(w, 0, x, 16);
    context.fillStyle = '#B8B175';
    context.fillRect(w + x, 0, y, 16);
    context.fillStyle = '#75B890';
    context.fillRect(w + x + y, 0, z, 16);
}

function drawGraph(canvasName, xs) {
    var context = getContext(canvasName);
    var spacing = 5;
    var width = 16;

    var total = 0;
    var max = 0;
    for (var i = 0; i < xs.length; i += 1) {
        total += xs[i];
        if (xs[i] > max) {
            max = xs[i];
        }

    }
    for (var i = 0; i < xs.length; i += 1) {
        var x = xs[i];
        xs[i] = (x / max) * 100;
    }

    for (var i = 0; i < xs.length; i += 1) {
        var x = i * (spacing + width);
        var y = 100 - xs[i];
        context.fillStyle = '#4A4A4A';
        context.fillRect(x, y, width, xs[i]);
        context.fillStyle = '#A2A4BA';
        context.fillText(i, x, 100);
    }
}
