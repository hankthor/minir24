# The atlas algebra
# See the CPR establishing test

#-----------------------------------------------
library(ggplot2);
library(gridExtra);

TDS_W60 <- 670; TDS_HF <- 586;

#-----------------------------------------------
printab <- function(gdf = dataset, wd=670, hg=586, tag=TDS_TITLE, show=TRUE) {
    g <- ggplotab(gdf);
    fp <- file.path(Sys.getenv("USERPROFILE"), sprintf(".minir24/%s.png", tag));
    dir.create(dirname(fp), showWarnings=FALSE, recursive=TRUE);
    png(width=wd, height=hg, file=fp); print(g); muted <- dev.off();
    if(show) print(g);
}


#-----------------------------------------------
ggplotab <- function(gdf, top=7, lab=TDS_TITLE) {
    gdf <- head(gdf, top);
    g <- ggplot() + ggtitle(lab) + annotation_custom(tableGrob(gdf));
}


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

