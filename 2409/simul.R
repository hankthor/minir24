library(ggplot2);
library(gridExtra);

#------------------------------------------------
rdiff <- function(N=1) {
    runif(N) - runif(N);
}

#------------------------------------------------
rbell <- function(N=1) {
    sqrt(-2 * log(runif(N)) + 0) * cos( 2*pi*runif(N) + 0)
}

#------------------------------------------------
rturn2 <- function(x0, y0, x1, y1, dt) {
  x2 <- x0 + (x1-x0)*cos(dt) - (y1-y0)*sin(dt);
  y2 <- y0 + (y1-y0)*cos(dt) + (x1-x0)*sin(dt);
  return(c(x2, y2));
}


#------------------------------------------------
rturn <- function(x, y, dt) { 
  xk <- x*cos(dt) - y*sin(dt);
  yk <- y*cos(dt) + x*sin(dt);
  return( c(x=xk, y=yk) );
}



#------------------------------------------------
rdupl <- function(rows, gdf, sel, cdf=FALSE, inv=0) {
   rows <- sapply(rows, FUN=function(rk) { rlookup(rk, gdf[[sel]], cdf=cdf, inv=inv); });

   tdf <- data.frame(); cols <- names(gdf);
   for(k in seq_along(rows) ) {
      tdf[k, "rsel"] <- rk <- rows[k];
      if(rk == inv) next;
      for(nj in cols) tdf[k, nj] <- gdf[rk, nj];
   }

   return(tdf);
}

#------------------------------------------------
rlookup <- function(r, v, cdf=FALSE, inv=0) {
   if(cdf) {
      for(k in seq_along(v) ) if(r <= v[k] ) { return(k); } 
      return(inv);
   } else {
      s <- 0;
      for(k in seq_along(v) ) { s <- s + v[k]; if(r <= s) return(k); } 
      return(inv);
   }
}

#------------------------------------------------
splitl <- function(x, split="\\n") { 
    unlist(strsplit(x, split=split)) 
}

#------------------------------------------------
split2 <- function(x, split=",", cols=c("lat_yy", "long_xx") ) { 
    v <- splitl(x, split=split); 
    v <- data.frame(v1=v[1], v2=v[2]); 
    names(v) <- cols;
    return(v);
}

#------------------------------------------------
rbind_list <- function(ldf) {
  tdf <- NULL;
  for(tk in ldf) tdf <- rbind(tdf, tk); 
  return(tdf);
}


#------------------------------------------------
#END
