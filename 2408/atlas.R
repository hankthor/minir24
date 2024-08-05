# The atlas algebra
# See the CPR establishing test

#-----------------------------------------------
library(ggplot2);
library(gridExra);

#-----------------------------------------------
grid.tab <- function(gdf) {
   grid.table(head(gdf));
}

#-----------------------------------------------
grobs.ncol <- function(grobs, ncol=2) {
   grid.arrange(grobs=grobs, ncol=ncol);
}

#-----------------------------------------------
rename <- function(gdf, ...) {
  names(gdf) <- unlist(list(...));
  return(gdf);
}

