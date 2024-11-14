library(ggplot2); library(maps); library(gridExtra);

#-----------------------------------------------
labs_off <- function() { labs(title = NULL, x = NULL, y = NULL) }

#-----------------------------------------------
get_world_map <- function(nan=TRUE) {
    gdf <- map_data("world");
    if(nan) gdf <- gdf[gdf$region != "Antarctica", ];
    return(gdf);
}

#-----------------------------------------------
ggplot_map <- function(gdf=dataset) {
    g <- ggplot(get_world_map()) + geom_polygon(aes(x = long, y = lat, group = group), fill = "lightgray", color = "white");
    g <- g + geom_point(data=gdf, aes(x=long_xx, y=lat_yy), show.legend=FALSE);
    g <- g + theme_minimal() + labs_off();
    print(g);
}

#-----------------------------------------------
#ggplot_map();

