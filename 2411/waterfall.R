library(ggplot2);

unique_list <- function(gdf, key="FY_long", val="FY_short") {
    tdf <- unique(data.frame(key=gdf[[key]], val=gdf[[val]]));
    tdf <- lapply(split(tdf, tdf$key), FUN=function(ddd) { max(ddd$val) });
    return(tdf);
}

fmt_c1_e3 <- function(x, div=1e3) { 
    format(round(x/div, 1), nsmall=1, scientific = FALSE, big.mark=",") 
}

split_water <- function(tdf, bar_width=0.45) {
    ldf <- lapply(split(tdf, tdf$store), FUN=function(gdf) {
        st <- 0;
        for (i in 1:nrow(gdf) ) {
            if (gdf[i, "fill"] == "major") {  gdf[i, "y1"] <- 0; gdf[i, "y2"] <- st <- gdf$amt[i]; } 
            else { gdf[i, "y1"] <- st; gdf[i, "y2"] <- st <- st + gdf$amt[i]; }
        }

        gdf$x1 <- as.integer(factor(gdf$FY_long)) - bar_width;
        gdf$x2 <- gdf$x1 + 2*bar_width;
        return(gdf);
    });
    tdf <- NULL;
    for(tk in ldf) tdf <- rbind(tdf, tk);
    return(tdf);
}

ggplot_water <- function(gdf=dataset, fmt_cy = fmt_c1_e3, map_labels=NULL, map_cols=NULL, subset_ncol=3, subset_type="free", bar_width=0.45, chart_type="fall") {
    gdf <- gdf[order(paste(gdf$store, gdf$FY_long)), ];

    if( is.null(map_labels) ) map_labels <- unique_list(gdf, key="FY_long", val="FY_short");
    if( is.null(map_cols) ) map_cols <- list(major="yellow", price="red", vol="green", mix="blue");

    gdf <- split_water(gdf, bar_width=bar_width);
    g <- ggplot(gdf);
    
    if(chart_type == "fall") {
        g <- g + geom_text(aes(x=FY_long, y=amt, label=''), show.legend=FALSE) + 
            geom_rect(aes(ymin=y1, ymax=y2, xmin=x1, xmax=x2, fill = fill), show.legend=FALSE); 
    } else {
        g <- g + geom_bar(aes(x=FY_long, y=amt, fill=fill), stat="identity", show.legend=FALSE); 
    }

    g <- g + facet_wrap(scales=subset_type, ncol=subset_ncol, store ~ .);
    g <- g + scale_x_discrete(labels=map_labels) + scale_fill_manual(values=map_cols);
    g <- g + scale_y_continuous(labels=fmt_cy);
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank());
    print(g);
}

# ggplot_water(chart_type="fall");
