pkgs <- c('classInt', 'data.table', 'fst', 'htmltools', 'leaflet', 'RColorBrewer', 'sp')
lapply(pkgs, require, char = TRUE)

polyFillOpacity = 0.5
polyStrokeWeight = 1
polyStrokeOpacity = 0.5
polyStrokeColor = '#9e9ac8'
polyStrokeDash = '3'
cls_mth <- 'quantile' # choose from: fixed, equal, quantile, pretty, hclust, kmeans
n_breaks <- 12
fxd_brks <- c(0, 0.10, 0.25, 0.5, 1.00, 2.5, 5.0, 10.00, 25.00, 50.00, 75.00, 100.00)
use_palette <- TRUE
br_pal = 'OrRd' # see ColorBrewer website for all available Palettes: http://colorbrewer2.org/
rev_cols <- FALSE
fxd_cols <- c('#ffeda0', '#feb24c', '#f03b20')
lbl.options <- labelOptions(
    textsize = '12px', 
    direction = 'right', 
    sticky = FALSE, 
    opacity = 0.8,
    offset = c(60, -40), 
    style = list(
        'font-weight' = 'normal', 
        'padding' = '2px 6px'
    )
)
hlt.options <- highlightOptions(
          weight = 6,
          color = 'white',
          opacity = 1,
          bringToFront = TRUE
)

dts <- fread('./data/1918-2017election_results.csv')
dts <- dts[boundary_set == '2010-2017', names(dts)[!grepl('share', names(dts))], with = FALSE]
dts[, c('seats', 'constituency', 'country/region', 'boundary_set', '"') := NULL]
names(dts) <- gsub('_votes', '', names(dts))
setnames(dts, c('"constituency_id', 'total'), c('PCON', 'votes'))
setcolorder(dts, c('PCON', 'votes'))
dts <- dts[, lapply(.SD, function(x) gsub('"', '', x))]
cols <- names(dts)[2:ncol(dts)]
dts[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]

lcns <- read_fst(file.path(Sys.getenv('PUB_PATH'), 'datasets', 'uk', 'geography', 'locations'), as.data.table = TRUE) 
lcns <- lcns[type == 'PCON', .(PCON = location_id, name)]

bnd <- readRDS(file.path(Sys.getenv('PUB_PATH'), 'boundaries', 'uk', 'rds', 's20', 'PCON'))
bnd <- merge(bnd, lcns, by.x = 'id', by.y = 'PCON')

yr <- 2017
y <- dts[election == yr, .(PCON, Y = turnout)]

n_brks <- switch(cls_mth, 'fixed' = length(fxd_brks) - 1, 'quantile' = 10, n_breaks)
brks_poly <- 
    if(cls_mth == 'fixed'){
        classIntervals(y$Y, n = n_brks, style = 'fixed', fixedBreaks = fxd_brks)
    } else {
        classIntervals(y$Y, n_brks, cls_mth)
    }
if(use_palette){
    col_codes <-
        if(n_brks > brewer.pal.info[br_pal, 'maxcolors']){
            colorRampPalette(brewer.pal(brewer.pal.info[br_pal, 'maxcolors'], br_pal))(n_brks)
        } else {
            brewer.pal(n_brks, br_pal)
        }
    if(rev_cols) col_codes <- rev(col_codes)
} else {
    col_codes <- colorRampPalette(fxd_cols)(n_brks)
}
pal_poly <- findColours(brks_poly, col_codes)
y <- cbind(y, pal_poly)

bnd.y <- merge(bnd, y, by.x = 'id', by.y = 'PCON')

leaflet() %>% 
    addProviderTiles(providers$Wikimedia) %>% 
    addPolygons(
        data = bnd.y,
        fillColor = ~pal_poly,
        fillOpacity = polyFillOpacity,
        weight = polyStrokeWeight,
        opacity = polyStrokeOpacity,
        color = polyStrokeColor,
        dashArray = polyStrokeDash,
		smoothFactor = 0.2,
		highlightOptions = hlt.options,
        label = lapply(
            1:nrow(bnd.y),
            function(x)
                HTML(paste0(
                    'Constituency: <b>', bnd.y$name[x], '</b><br>', 
                    'Turnout: <b>', 100 * bnd.y$Y[x], '%</b>'
                ))
        ),
        labelOptions = lbl.options
    )
               