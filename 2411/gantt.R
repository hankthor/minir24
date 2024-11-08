#source("https://raw.githubusercontent.com/tutrunghieu/minir24/refs/heads/main/2411/gantt.R");

#-----------------------------------------------
library(ggplot2); library(gridExtra);


#-----------------------------------------------
ggplot_gantt_inner <- function(gdf, subset_type="free", subset_ncol=1, gantt_size=3.5, wale_size=1, wale_col='black', div_type="dashed", div_col="blue") {
    g <- ggplot(gdf);
    gdf_WALE <- gdf[ gdf$WALE_start != "xx", ];

    g <- g + facet_wrap(scales=subset_type, ncol=subset_ncol, store ~ .);

    g <- g + geom_text(aes(x=rent_start, y=rent_code, label=''), show.legend=FALSE);
    g <- g + geom_text(aes(x=rent_end, y=rent_code, label=''), show.legend=FALSE);
    g <- g + geom_text(aes(x=WALE_date, y=rent_code, label=''), show.legend=FALSE);

    g <- g + geom_segment(aes(x=rent_start, xend=rent_end, y=rent_code, yend=rent_code, color=rent_code), size=gantt_size, show.legend=FALSE);
    g <- g + geom_vline(aes(xintercept=WALE_date), linetype=div_type, color=div_col, show.legend=FALSE);
    g <- g + geom_segment(data=gdf_WALE, aes(x=WALE_start, xend=rent_end, y=rent_code, yend=rent_code), color=wale_col, size=wale_size, show.legend=FALSE);

    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank() );
    g <- g + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()  );
    
    return(g);
}

#-----------------------------------------------
ggplot_gantt <- function(gdf=dataset, ncol=2, inner=ggplot_gantt_inner, mode="") {
    names(gdf) <- sapply(names(gdf), FUN=function(x) { gsub("\\s+", "_", x) })
    gdf$rent_start <- left_seq(gdf$rent_start, 10);
    gdf$rent_end <- left_seq(gdf$rent_end, 10);
    gdf$WALE_date <- left_seq(gdf$WALE_date, 10);

    g <- lapply(split(gdf, gdf$store), FUN=inner);
    g <- ggplot() + annotation_custom(arrangeGrob(grobs=g, ncol=ncol));
    
    if( mode == "g" ) return(g);
    if( mode == "grob" ) return( ggplotGrob(g) );

    print(g);
}

#-----------------------------------------------
left_seq <- function(x, n) {
    for(k in seq_along(x)) { x[k] <- substr(x[k], 1, n); }
    return(x);
}

#-----------------------------------------------
#ggplot_gantt(ncol=1);
