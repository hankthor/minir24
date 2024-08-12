# The atlas algebra
# See the CPR establishing test

#-----------------------------------------------
library(ggplot2);
library(gridExtra);

#-----------------------------------------------
rgal <- function(N=5000, alpha=pi/4, sx=3e-3, sy=1e-3, y0=1.3634088, x0=103.8435614, tag="abc") {
  sina <- sin(alpha); cosa <- cos(alpha); 

  df <- data.frame(xx=rnorm(N), yy=rnorm(N));
  for(k in 1:nrow(df)) {
    xk <- sx * df[k, "xx"]; yk <- sy * df[k, "yy"];
    df[k, "xx"] <- x0 + xk * cosa - yk * sina;
    df[k, "yy"] <- y0 + xk * sina + yk * cosa;
  }
  df$tag <- tag;
  return(df);
}


#-----------------------------------------------
make_user_file <- function(rel) { 
    rel <- file.path(Sys.getenv("USERPROFILE"), rel); 
    dir.create(dirname(rel), showWarnings=FALSE, recursive=TRUE);
    return(rel);
}

#-----------------------------------------------
fmt_c0 <- function(x) { 
   format(x, big.mark=",", digits=1, scientific=FALSE);
}

#-----------------------------------------------
no_axis_titles <- function() { 
   theme(axis.title.x=element_blank(), axis.title.y=element_blank() ); 
}

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

