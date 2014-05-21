function drawBar(canvasName, w, x, y, z) {
    var canvas = document.getElementById(canvasName);
    var context = canvas.getContext('2d');
    context.beginPath();
    context.fillStyle = '#FF0000';
    context.fillRect(0, 0,w, 16);
    context.fillStyle = '#FF00FF';
    context.fillRect(w, 0, x, 16);
    context.fillStyle = '#0000FF';
    context.fillRect(w + x, 0, y, 16);
    context.fillStyle = '#00FFFF';
    context.fillRect(w + x + y, 0, z, 16);

}
