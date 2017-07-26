set term png medium
set output "tree-locations.png"
set title "skybox tree locations"
set grid
set xlabel "<--- X --->"
set ylabel "<--- Z --->"
set size ratio 1
set timestamp
plot [-2976:2144] [-3109:2011] 'tree-locations.dat' with points lt 3 pt 4
quit
