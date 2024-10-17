
paste_sf <- function(name) {
  sf <- "https://raw.githubusercontent.com/tutrunghieu/minir24/refs/heads/main/";
  sf <- paste0(sf, name);
  return(sf);
}

paste_tf <- function(name, verbose=FALSE) {
  tf <- file.path(Sys.getenv("USERPROFILE"), ".minir24");
  dir.create(tf, showWarnings=FALSE, recursive=TRUE);
  tf <- file.path(tf, name); 
  if(verbose) print(tf);
  return(tf);
}


download.file(url = paste_sf("2408/atlas.R"), destfile = paste_tf("atlas.R") );
download.file(url = paste_sf("2408/string.R"), destfile = paste_tf("string.R") );
download.file(url = paste_sf("2409/load.R"), destfile = paste_tf("load.R") );

gdf <- list.files(paste_tf("."), patt="*.R" );
gdf <- data.frame(name = unlist(gdf));
print(gdf); 
