
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
#END
