#source("https://raw.githubusercontent.com/tutrunghieu/minir24/refs/heads/main/2411/waterfall.R");

#-----------------------------------------------
library(ggplot2); library(gridExtra);

#-----------------------------------------------
unique_list <- function(gdf, key="FY_long", val="FY_short", seq="FY_displ") {
    tdf <- unique(data.frame(key=gdf[[key]], val=gdf[[val]], seq=gdf[[seq]]));
    tdf <- tdf[order(tdf$seq), ];
    v <- tdf$val; names(v) <- tdf$key;
    grid.table(data.frame(x=v, y=names(v)));
    return(v);
}

#-----------------------------------------------
fmt_c1_e3 <- function(x, div=1e3) { 
    format(round(x/div, 1), nsmall=1, scientific = FALSE, big.mark=",") 
}

sort_levels <- function(gdf, val="FY_long", key="FY_displ") {
    gdf <- unique(gdf[, c(key, val)]);
    gdf <- gdf[order(gdf[[key]]), ];
    return(gdf[[val]]);
}


#-----------------------------------------------
ggplot_water <- function(gdf=dataset, fmt_cy = fmt_c1_e3, map_labels=NULL, map_cols=NULL, subset_ncol=3, subset_type="free", bar_width=0.45, show_bar=FALSE, show_box=TRUE, top=0) {
    gdf <- gdf[order(paste(gdf$store, gdf$FY_displ)), ];
    gdf$FY_long <- factor(gdf$FY_long, levels=sort_levels(gdf) );

    if( is.null(map_labels) ) map_labels <- unique_list(gdf, key="FY_long", val="FY_short", seq="FY_displ");
    if( is.null(map_cols) ) map_cols <- list(major="yellow", price="red", vol="green", mix="blue");

    st <- 0;
    for (i in 1:nrow(gdf) ) {
        if (gdf[i, "fill"] == "major") {  gdf[i, "y1"] <- 0; gdf[i, "y2"] <- st <- gdf$amt[i]; } 
        else { gdf[i, "y1"] <- st; gdf[i, "y2"] <- st <- st + gdf$amt[i]; }
    }

    gdf$x1 <- as.integer(factor(gdf$FY_long)) - bar_width;
    gdf$x2 <- gdf$x1 + 2*bar_width;

    g <- ggplot(gdf);
    
    if(show_box) {
        g <- g + geom_text(aes(x=FY_long, y=0, label=''), angle=90, show.legend=FALSE) + 
            geom_rect(aes(ymin=y1, ymax=y2, xmin=x1, xmax=x2, fill = fill), show.legend=FALSE); 
    } 

    if(show_bar) {
        g <- g + geom_bar(aes(x=FY_long, y=amt, fill=fill), stat="identity", show.legend=FALSE); 
    }

    g <- g + facet_wrap(scales=subset_type, ncol=subset_ncol, store ~ .);
    g <- g + scale_x_discrete(labels=map_labels);
    g <- g + scale_fill_manual(values=map_cols);
    g <- g + scale_y_continuous(labels=fmt_cy);
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank());
    print(g);
}

#-----------------------------------------------
# ggplot_water();
