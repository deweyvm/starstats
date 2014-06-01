set term png size 400,400
set xlabel 'Lines of Data Processed (thousands)'
set ylabel 'Time (seconds per thousand data lines)'
set title 'Data Processing Cost'
set output 'graph.png'
plot 'in.csv' using 0:1 pt 5 ps 0.1 notitle
