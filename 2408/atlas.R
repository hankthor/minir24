# The atlas algebra
# See the CPR establishing test

#-----------------------------------------------
TDS_WF <- 670; TDS_HF <- 586; TDS_TITLE <- "PANEL1";
library(ggplot2); library(gridExtra); library(base64enc); 
library(dplyr); library(png); library(grid); 

#-----------------------------------------------
print_v <- function(g) { if( is.function(g) ) { g <- g(); }; g <- g + theme_void() + no_axis_titles(); print(g); }

#-----------------------------------------------
geom_bar_xf1 <- function(leg=TRUE) { geom_bar(aes(x=FY, y=1, fill=fill), stat="identity", show.legend=leg) }

#-----------------------------------------------
geom_top <- function(gdf, top=7) { gdf <- tableGrob(head(gdf, top)); annotation_custom(gdf); }

#-----------------------------------------------
ggmatrix <- function(gdf, mode="g", show_marker=FALSE, marker_size=5, text_angle=25) {
    g <- ggplot(gdf) + no_axis_titles();
    if(show_marker) g <- g + geom_point(aes(x=xx_grad, y=yy_grad, color=fill_grad), size=marker_size, show.legend=FALSE);
    g <- g + geom_text(aes(x=xx_grad, y=yy_grad, label=fmt_c1(amt_grad), color=fill_grad), angle=text_angle, show.legend=FALSE);
    g <- g + scale_x_discrete(position = "top");
    if(mode == "g") { return(g); }
    if(mode == "p" | mode == "print") { print(g); return(cat()); }
}


#-----------------------------------------------
rename_dyadic_AB <- function(gdf=dataset) {
    cols <- names(gdf); pA <- which(cols=="__%%A"); pB <- which(cols=="__%%B");
    tdk <- gdf[, 1:(pA-1)];

    tdf <- NULL;
    for(k in seq_along(cols)) {
        nk <- cols[[k]]; 
        tk <- ifelse(k <= pA | k == pB, "xx", ifelse(k>pB, "line", "bar") );
        if(tk == "xx") { next; } 
        tdk$col_name <- nk; tdk$col_num <- k; 
        tdk$col_type <- tk; tdk$col_amt <- gdf[[nk]]; 
        tdf <- rbind(tdf, tdk);        
    }

    return(tdf);
}


#-----------------------------------------------
export_frame <- function(gdf, fp, mode="csv") {
    fp <- file.path(Sys.getenv("USERPROFILE"), ".minir24/tables", fp);
    dir.create(dirname(fp), showWarnings=FALSE, recursive=TRUE);    
    if(mode=="csv") write.csv(gdf, file=fp, row.names=FALSE);
    if(mode=="dput") dput(gdf, file=fp);
}


#-----------------------------------------------
fmt_c1 <- function(x, div=1) { format(round(x/div, 1), nsmall=1, scientific = FALSE, big.mark=",") }
fmt_c1_e3 <- function(x, div=1e3) { format(round(x/div, 1), nsmall=1, scientific = FALSE, big.mark=",") }
fmt_c1_e6 <- function(x, div=1e6) { format(round(x/div, 1), nsmall=1, scientific = FALSE, big.mark=",") }
fmt_c1_e9 <- function(x, div=1e9) { format(round(x/div, 1), nsmall=1, scientific = FALSE, big.mark=",") }

#-----------------------------------------------
replace <- function(x, lt, nt) { 
	gsub(lt, nt, x, fixed=TRUE); 
}

#-----------------------------------------------
labels_xx <- function(gdf, xx_long="xx_long", xx_short="xx_short") {
    tdf <- unique(data.frame(xx_long=gdf[[xx_long]], xx_short=gdf[[xx_short]])); 
    tdf <- sapply(split(tdf, tdf$xx_long), FUN=function(ddd) { ddd$xx_short });
    return(tdf);
}

#-----------------------------------------------
rename_pivot_dual <- function(gdf=dataset) {
    cols <- names(gdf); tdf <- NULL; st <- "";
    for(k in seq_along(cols)) {
        nk <- cols[k];
        if(nk == "__%%A") { st <- "val"; next; }
        if(st == "val") { 
            tk <- data.frame(xx=gdf[[1]], yy=gdf[[2]]); 
            tk$fill <- nk; tk$amt <- gdf[[nk]]; 
            tdf <- rbind(tdf, tk); 
        }
    }

    tdf$xx_long <- paste(replace(tdf$fill, "__", ""), tdf$xx);
    tdf$xx_short <- ifelse(tdf$xx=="FY21", tdf$xx_long, tdf$xx);

    return(tdf);
}


#-----------------------------------------------
rename_void <- function(gdf=dataset) { return(gdf); }

#-----------------------------------------------
ggplot_top <- function(gdf, top=11) { 
    gdf <- tableGrob(head(gdf, top)); ggplot() + annotation_custom(gdf); 
}

#-----------------------------------------------
rename_pApB <- function(gdf=dataset, tile="store fmt", tile_az="store fmt az") {
    gdf$tile <- factor_tile(gdf, tile=tile, tile_az=tile_az);
    
    cols <- names(gdf); st <- "bar"; tdf <- NULL;
    for(k in seq_along(cols)) {
        nk <- cols[k];
        if( nk %in% c("FY", "tile") ) { next; }
        if(nk == "__%%A") { st <- "line"; next; }
        if(nk == "__%%B") { st <- "other"; break; }
        tk <- data.frame(xx=gdf$FY, tile=gdf$tile, yy=gdf[[nk]], name=nk, type=st);
        tdf <- rbind(tdf, tk);
    }

#    tdf$tile <- "void";
    return(tdf);
}

#-----------------------------------------------
scale_cy_c1_e3 <- function(div=1e3) { 
	fmt <- function(x) { format(round(x/div, 1), nsmall=1, scientific = FALSE, big.mark=",") }
	scale_y_continuous(labels=fmt);  
}

#-----------------------------------------------
scale_cy_c1 <- function(div=1) { 
	fmt <- function(x) { format(round(x/div, 1), nsmall=1, scientific = FALSE, big.mark=",") }
	scale_y_continuous(labels=fmt);  
}


#-----------------------------------------------
printf <- function(g=dataset, ncol=2, top=11) { 
    if( is.ggplot(g) ) { print(g); return(cat()) }
    if( is.data.frame(g) ) { print(ggplot()); grid.table(head(g, top)); return(cat()) }
    grid.arrange(grobs=g, ncol=ncol); 
}


#-----------------------------------------------
lapply_tile_ncol <- function(gdf, FUN, ncol=2, mode="g") { 
    if(! ("tile" %in% names(gdf)) ) gdf$tile <- "all";  
    grobs <- lapply(split(gdf, gdf$tile), FUN=FUN);
    if(mode=="g") return( ggplot() + annotation_custom(arrangeGrob(grobs=grobs, ncol=ncol)) );
    if(mode=="grobs" | mode=="gg") return(grobs);
    return(cat());
}

#-----------------------------------------------
rename_xbl_t2 <- function(gdf=dataset, sort_tile=TRUE) {
    tdf <- data.frame(xx=gdf[[1]], yy_bar=gdf[[2]], yy_line=gdf[[3]]);
    tdf$tile <- gdf[[4]]; 
    tdf$tile_az <- gdf[[5]];
    if( sort_tile ) tdf$tile <- factor_tile(tdf);
    return(tdf);    
}

#-----------------------------------------------
factor_tile <- function(gdf, tile="tile", tile_az = "tile_az") { factor(gdf[[tile]], levels = levels_az(gdf, tile=tile, tile_az=tile_az) ); }

#-----------------------------------------------
factor_fill <- function(gdf, tile="fill", tile_az = "fill_az") { factor(gdf[[fill]], levels = levels_az(gdf, tile=fill, tile_az=fill_az) ); }

#-----------------------------------------------
lapply_tile <- function(gdf, FUN) { 
    if(! ("tile" %in% names(gdf)) ) gdf$tile <- "all";  
    lapply(split(gdf, gdf$tile), FUN=FUN);
}

#-----------------------------------------------
affine_scale <- function(x, y) {
    y1 <- min(y); y2 <- max(y); x1 <- min(x); x2 <- max(x);
    return( y1 + (x - x1)*(y2 - y1)/(x2 - x1) );
}


#-----------------------------------------------
facet_tile <- function(free="free", ncol=2) { 
    facet_wrap(scales = free, ncol = ncol, tile ~ .); 
} 

#-----------------------------------------------
no_axis_titles <- function() { 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank() ); 
}

#-----------------------------------------------
levels_az <- function(gdf, tile="tile", tile_az="tile_az") { 
    tdf <- data.frame(tile=gdf[[tile]], tile_az=gdf[[tile_az]]);
    tdf <- unique(tdf); 
    tdf <- tdf[order(tdf$tile_az), ]; 
    return(tdf$tile);
}


#-----------------------------------------------
print0 <- function(x="abc", gdf=dataset, top=11) { print(g <- ggplot() + ggtitle(x)); grid.table(head(gdf, top)); }

#-----------------------------------------------
read_grob <- function(path, width = unit(1, "npc"), height = unit(1, "npc"), rel=TRUE) {
    if(rel) path <- file.path(Sys.getenv("USERPROFILE"), ".minir24/Flags", sprintf("%s.PNG", path) );
    rasterGrob(readPNG(path)); 
}

#-----------------------------------------------
read_base64 <- function(path, width = unit(1, "npc"), height = unit(1, "npc"), rel=TRUE) {
    if(rel) path <- file.path(Sys.getenv("USERPROFILE"), ".minir24/Flags", sprintf("%s.PNG", path) );
    base64encode(path); 
}

#-----------------------------------------------
ggwater <- function(gdf, title = CHART_TITLE, tag = CHART_TAG, ncol = GRID_COL,
    bar_width = 0.45, pvm_levels = c("major", "price", "vol", "mix"), mode="dual") {
    gdf$x1 <- as.integer(factor(gdf$FY)) - bar_width;
    gdf$x2 <- gdf$x1 + 2*bar_width;
    gdf$fill <- factor(gdf$fill, levels = pvm_levels);
    gdf <- gdf %>% arrange(tile, FY, fill);

    st <- 0;

    for (i in 1:nrow(gdf)) {
        if (is.na(gdf[i, "fill"])) { next; }
        if (gdf[i, "fill"] == "major") {  gdf[i, "y1"] <- 0; gdf[i, "y2"] <- st <- gdf$amt[i]; } 
        else { gdf[i, "y1"] <- st; gdf[i, "y2"] <- st <- st + gdf$amt[i]; }
    }

    g <- ggplot(gdf) + facet_wrap(~ tile, scales = "free", ncol = ncol) + geom_text(aes(x=FY, y=0, label= ""), show.legend=FALSE);
    g <- g + geom_rect(aes(ymin=y1, ymax=y2, xmin=x1, xmax=x2, fill = fill), show.legend=FALSE);
	
    if(mode=="dual") { ggdual(g, tag = tag); return(cat()); }
    if(mode=="print") { print(g); return(cat()); }
    return(g);
} 

#-----------------------------------------------
head11 <- function(gdf=dataset, top=11) { grid.table(head(gdf, top)); } 

#-----------------------------------------------
scale_cy_c1 <- function(div=1e3) { scale_y_continuous(labels=function(x) {  format(x/div, big.mark=",", digits=1, scientific=FALSE) }) }

#-----------------------------------------------
scale_cy_c1_e3 <- function(div=1e3) { scale_cy_c1(div=1e3) }

#-----------------------------------------------
scale_cy_c1_e6 <- function(div=1e3) { scale_cy_c1(div=1e6) }

#-----------------------------------------------
geom_bar_xyf <- function() { geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", show.legend=TRUE) }

#-----------------------------------------------
legend_bottom <- bottom_legend <- function() { theme(legend.position="bottom", legend.title = element_blank() )  }


#-----------------------------------------------
geom_array <- function(...) {
    args <- list(...);
    ncol <- args$ncol; if( is.null(ncol) ) ncol <- 2;
    args$ncol <- NULL;
    annotation_custom(arrangeGrob(grobs=args, ncol=ncol));
}


#-----------------------------------------------
printab <- function(gdf = dataset, wd=670, hg=586, tag=TDS_TITLE, show=TRUE) {
    g <- ggplotab(gdf);
    fp <- file.path(Sys.getenv("USERPROFILE"), sprintf(".minir24/%s.png", tag));
    dir.create(dirname(fp), showWarnings=FALSE, recursive=TRUE);
    png(width=wd, height=hg, file=fp); print(g); muted <- dev.off();
    if(show) print(g);
}

#-----------------------------------------------
ggdual <- function(g, wd=670, hg=586, tag=TDS_TITLE, show=TRUE) {
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

