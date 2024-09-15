#To load data from PBI source or cached PBI source
#Native to alteryx/pbi panels

#-----------------------------------------------
rename <- function(gdf, ...) {
  names(gdf) <- unlist(list(...));
  return(gdf);
}


#-----------------------------------------------
#END
