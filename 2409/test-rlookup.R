
rm(list=ls());

source('https://raw.githubusercontent.com/tutrunghieu/minir24/refs/heads/main/2409/simul.R');

main <- function() {
  v <- c(1, 1, 1, 1, 1);
  n <- c("red", "green", "blue", "black", "white");
  gdf <- data.frame(prob = v/sum(v), text=n);
  gdf <- rdupl(rows=runif(10), gdf=gdf, sel="prob", cdf=FALSE); print(gdf);
}

main();

