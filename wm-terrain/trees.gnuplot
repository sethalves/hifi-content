set term png medium
set output "tree-locations.png"
set title "tree locations"
set grid
set xlabel "<--- X --->"
set ylabel "<--- Z --->"
set size ratio 1
set timestamp
plot [-1200:1200] [-1200:1200] 'tree-locations.dat' with points lt 3 pt 4
quit
