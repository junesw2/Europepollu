###############################################################################
###
### Population exposure to multiple air pollutants and its compound episodes in Europe
### December 2023
### Zhaoyue Chen
### 
################################################################################

library("tmap")
suppressMessages(library(giscoR)) # Functions: gisco_get_countries
suppressMessages(library(sf)) # Functions: st_read
suppressMessages(library(tidyverse)) # Functions: read_delim

# Simple Feature Object Containing All the Countries of the World
SFO_COU_WORLD = gisco_get_countries(year = "2020", epsg = "3035", resolution = "3", cache = TRUE, update_cache = FALSE, verbose = FALSE)

library(giscoR)
# Read the latest European shapefile at NUTS2 level at the highest resolution
nuts_polygons <- gisco_get_nuts(resolution = "01", year = "2021",
                                epsg = "3035", nuts_level = "3")
# Remove some regions in which we are no interested:
# Overseas France - FRY (out of the European extent)
# Turkey - TR (no population data)
# Svalbard - NO0B (out of the European extent)
SFO_COU_EU<- nuts_polygons[!(grepl("FRY", nuts_polygons$NUTS_ID) |
                                   nuts_polygons$CNTR_CODE == "TR" | 
                                   nuts_polygons$NUTS_ID == "NO0B1"| 
                                   nuts_polygons$NUTS_ID == "NO0B2"),]


cous_polygons <- gisco_get_nuts(resolution = "01", year = "2021",
                                epsg = "3035", nuts_level = "0")
###info of location
locationinfo=data.frame(location=nuts_polygons$NUTS_ID,ID=1:length(nuts_polygons$NUTS_ID))

# Function to set for the break
break_gen <- function(var) {
  FRAC_VAL <- 0.999
  
  # Colorbar Range Values
  VAR_TO <- ceiling(quantile(var, 1 - (1 - FRAC_VAL) / 2, na.rm = TRUE))
  VAR_FROM <- floor(quantile(var, (1 - FRAC_VAL) / 2, na.rm = TRUE))
  
  # Colorbar Interval Value
  BASE <- ceiling(log10(VAR_TO - VAR_FROM))
  if (10^BASE / 10 < VAR_TO - VAR_FROM & VAR_TO - VAR_FROM <= 10^BASE / 5) {
    VAR_BY <- 10^BASE / 25
  } else if (10^BASE / 5 < VAR_TO - VAR_FROM & VAR_TO - VAR_FROM <= 10^BASE / 2) {
    VAR_BY <- 10^BASE / 20
  } else if (10^BASE / 2 < VAR_TO - VAR_FROM & VAR_TO - VAR_FROM <= 10^BASE / 1) {
    VAR_BY <- 10^BASE / 15
  } else {
    stop("ERROR: Invalid VAR_BY !!!")
  }
  rm(BASE)
  
  # Rounding the Colorbar Range Values According to the Colorbar Interval Value
  VAR_TO <- ceiling(VAR_TO / VAR_BY) * VAR_BY
  VAR_FROM <- floor(VAR_FROM / VAR_BY) * VAR_BY
  
  # Colorbar Values
  breaks <- round(seq(from = VAR_FROM, to = VAR_TO, by = VAR_BY), 2)
  breaks[length(breaks)] <- round(max(var), 0)
  return(breaks)
}

# Function to plot the maps
map_generator <- function(countries = SFO_COU_WORLD,
                          nuts0 = SFO_COU_EU,
                          border_cou = cous_polygons,
                          nuts.var = SFO_NUTS_CASE,
                          c4col = NULL, palette = NULL,
                          varplot = "pm25v0", mid = NA,
                          style = "fixed", breaks = NA,
                          labels = NULL, title1 = NULL,
                          Cleararea = FALSE) {
  # countries: background shapefile
  # nuts0: NUTS3 region shapefile
  # border_cou: countries boundaries
  # nuts.var: shapefile for plots
  # c4col/palette: color palette for cols4all packages or directly give palette names(string)
  # varplot: variable for plot
  # mid: middle value for div color
  # style: style in tmap packages "fixed","pretty" or more
  # breaks: break for color if you did not set style
  # labels/titles: string for them
  # Cleararea: plot the clear area or not
  
  if (length(breaks) == 1) {
    breaks <- break_gen(nuts.var[[varplot]])
  }
  if (!is.null(c4col)) {
    palette <- c4a(c4col, length(breaks))
  }
  if (Cleararea) {
    mid <- 4
    nuts.var[[varplot]][nuts.var[[varplot]] <= 4] <- -max(breaks) / 2
    breaks[1] <- -max(breaks) / 2
  }  
  # Information for Europe
  europe = tm_shape( countries, bbox = c(20,14,60,55) * 10e4 ) +
    tm_fill( "#E0E0E0" ) +
    tm_shape( nuts.var ) +
    tm_fill( varplot, breaks = breaks,labels=labels, palette = palette, alpha = 1, midpoint = mid, title = "",style=style ) +
    tm_shape( nuts0, col="white") +
    tm_borders( lwd = .01 ) +
    tm_shape(border_cou)+
    tm_borders( lwd = .2 ) +
    tm_layout( main.title = title1, main.title.position = c( x = "left", y = "top" ), main.title.size = 1,legend.position = c("left","top"), legend.text.size = 0.8, frame = TRUE )
  
  # Information for Cyprus
  cyprus = tm_shape( countries, bbox = c(62.5,15.5,66,18) * 10e4 ) +
    tm_fill( "#E0E0E0" ) +
    tm_shape( nuts.var ) +
    tm_fill( varplot, breaks = breaks,labels=labels, palette = palette, alpha = 1, midpoint = mid, legend.show = FALSE,style=style ) +
    tm_shape( nuts0 , col="white") +
    tm_borders( lwd = .05 ) +
    tm_shape(border_cou)+
    tm_borders( lwd = .2 ) +
    tm_layout( main.title = "Cyprus (CY)", main.title.position = c( x = "centre", y = "top" ), main.title.size = 0.4, frame = TRUE )
  
  # Information for the Canary Islands
  canary = tm_shape( countries, bbox = c(15,9,20.75,12) * 10e4 ) +
    tm_fill( "#E0E0E0" ) +
    tm_shape( nuts.var ) +
    tm_fill( varplot, breaks = breaks,labels=labels, palette = palette, alpha = 1, midpoint = mid, legend.show = FALSE ,style=style) +
    tm_shape( nuts0 , col="white") +
    tm_borders( lwd = .05 ) +
    tm_shape(border_cou)+
    tm_borders( lwd = .2 ) +
    tm_layout( main.title = "Canarias (ES)", main.title.position = c( x = "centre", y = "top" ), main.title.size = 0.4, frame = TRUE )
  
  # Information for Acores
  acores = tm_shape( countries, bbox = c(8,22,15,29) * 10e4 ) +
    tm_fill( "#E0E0E0" ) +
    tm_shape( nuts.var ) +
    tm_fill( varplot, breaks = breaks,labels=labels, palette = palette, alpha = 1, midpoint = mid, legend.show = FALSE,style=style ) +
    tm_shape( nuts0 , col="white") +
    tm_borders( lwd = .05 ) +
    tm_shape(border_cou)+
    tm_borders( lwd = .2 ) +
    tm_layout( main.title = "Acores (PT)", main.title.position = c( x = "centre", y = "top" ), main.title.size = 0.4, frame = TRUE )
  
  return( list( europe = europe,
                cyprus = cyprus,
                canary = canary,
                acores = acores ) );
  
}

###double y axis plot
doubley_plot <- function(df, Y1, Y2, L1, L2, mtitle) {
  # df: Dataframe containing the data for plotting
  # Y1: Column name for the bar plot (primary y-axis)
  # Y2: Column name for the line plot (secondary y-axis)
  # L1: Label for the primary y-axis
  # L2: Label for the secondary y-axis
  # mtitle: Main title for the plot
  
  # Set colors for the plots
  temperatureColor <- "coral"
  priceColor <- rgb(0.2, 0.6, 0.9, 1)
  
  # Calculate the coefficient between Y2 and Y1 for secondary axis scaling
  coeff <- mean(df[[Y2]]) / mean(df[[Y1]])
  
  # Create the combined line and bar plots using ggplot
  p <- ggplot(df, aes(x = year)) +
    geom_bar(aes(y = !!sym(Y1)), stat = "identity", size = .1, fill = temperatureColor, color = "black", alpha = .4) + 
    geom_line(aes(y = !!sym(Y2) / coeff), size = 2, color = priceColor) +
    scale_x_continuous(breaks = c(2003, 2005, 2010, 2015, 2019)) +
    scale_y_continuous(
      # Features of the first axis
      name = L1,
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*coeff, name = L2)
    ) + 
    theme_minimal() +
    theme(
      axis.title.y = element_text(color = temperatureColor, size = 13),
      axis.title.y.right = element_text(color = priceColor, size = 13)
    ) +
    ggtitle(mtitle)
  
  return(p)
}

###generate the label for clean air areas
generate_label_b <- function(breaks,minus=1) {
  # Create vector of labels
  label_b <- c(paste0("Clean air Area (99% days within WHO guideline)"))
  for (i in 2:(length(breaks)-1)) {
    label_b <- c(label_b, paste0(breaks[i], " to ", breaks[i+1]))
  }
  return(label_b)
}


# Read annual unclean air trend data
foldout <- paste0("/Yourfolder/docs/scriptforNC/")
annual_re <- readRDS(file = paste0(foldout, "annualuncleanday_trend.Rdata"))

# Extract regions for data plotting
var0 <- "pm25v0"
mtitle <- "a) Trend for PM25"
pm25_slope <- doubley_plot(data.frame(annual_re), paste0(var0, "_expo_day"), paste0(var0, "_expo_day_npop"), "Unclean air days", "Population (%) in Clean air Area", mtitle = mtitle)

var0 <- "pm10v0"
mtitle <- "c) Trend for PM10"
pm10_slope <- doubley_plot(data.frame(annual_re), paste0(var0, "_expo_day"), paste0(var0, "_expo_day_npop"), "Unclean air days", "Population (%) in Clean air Area", mtitle = mtitle)

var0 <- "no2pred"
mtitle <- "e) Trend for NO2"
no2_slope <- doubley_plot(data.frame(annual_re), paste0(var0, "_expo_day"), paste0(var0, "_expo_day_npop"), "Unclean air days", "Population (%) in Clean air Area", mtitle = mtitle)

var0 <- "o3_8hpred"
mtitle <- "g) Trend for O3"
o3_slope <- doubley_plot(data.frame(annual_re), paste0(var0, "_expo_day"), paste0(var0, "_expo_day_npop"), "Unclean air days", "Population (%) in Clean air Area", mtitle = mtitle)


### Read spatial unclean air data

# Read spatial unclean air data
foldout <- paste0("/Yourfolder/inputdata/")
SFO_NUTS_CASE <- readRDS(file = paste0(foldout, "uncleanday_maps.Rdata"))

# Generate maps for PM2.5
library(tmap)
varplot <- "pm25v0_expo_day"
breaks <- c(0, 4, 50, 100, 150, 200, 250, 300, 365)
palette <- c4a('hcl.blue_red2')
palette[5] <- "snow"
maps_pm25 <- map_generator(countries = SFO_COU_WORLD,
                           nuts0 = SFO_COU_EU,
                           palette = palette,
                           breaks = breaks,
                           labels = generate_label_b(breaks),
                           nuts.var = SFO_NUTS_CASE,
                           varplot = varplot,
                           Cleararea = TRUE,
                           mtitle = "b) PM25 unclean air days")

# Generate maps for PM10
varplot <- "pm10v0_expo_day"
breaks <- c(0, 4, 20, 40, 60, 80, 100, 120, 140)
palette <- c4a('hcl.blue_red2')
palette[5] <- "snow"
maps_pm10 <- map_generator(countries = SFO_COU_WORLD,
                           nuts0 = SFO_COU_EU,
                           palette = palette,
                           breaks = breaks,
                           labels = generate_label_b(breaks),
                           nuts.var = SFO_NUTS_CASE,
                           varplot = varplot,
                           Cleararea = TRUE,
                           mtitle = "d) PM10 unclean air days")

# Generate maps for O3
varplot <- "o3_8hpred_expo_day"
breaks <- c(0, 4, 20, 40, 60, 80, 100)
palette <- c4a('hcl.blue_red2')
palette[5] <- "snow"
maps_o3 <- map_generator(countries = SFO_COU_WORLD,
                         nuts0 = SFO_COU_EU,
                         palette = palette,
                         breaks = breaks,
                         labels = generate_label_b(breaks),
                         nuts.var = SFO_NUTS_CASE,
                         varplot = varplot,
                         Cleararea = TRUE,
                         mtitle = "h) O3 unclean air days")

# Generate maps for NO2
varplot <- "no2pred_expo_day"
breaks <- c(0, 4, 25, 50, 75, 100, 150, 200)
palette <- c4a('hcl.blue_red2')
palette[5] <- "snow"
maps_no2 <- map_generator(countries = SFO_COU_WORLD,
                          nuts0 = SFO_COU_EU,
                          palette = palette,
                          breaks = breaks,
                          labels = generate_label_b(breaks),
                          nuts.var = SFO_NUTS_CASE,
                          varplot = varplot,
                          Cleararea = TRUE,
                          mtitle = "f) NO2 unclean air days")

# Create a PDF for plotted figures
library(grid)
foldout <- paste0("/Yourfolder/docs/scriptforplot/figure/")
if (!file_test("-d", foldout)) {
  dir.create(foldout, recursive = TRUE)
}
pdf(paste0(foldout, "Figure4_unclean_days.pdf"), width = 10, height = 20)
# Create a 2X2 grid
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 2)))

# Define the plots
plot1 <- maps_pm25$europe
plot2 <- maps_pm10$europe
plot3 <- maps_no2$europe
plot4 <- maps_o3$europe
plot11 <- pm25_slope
plot21 <- pm10_slope
plot31 <- no2_slope
plot41 <- o3_slope

# Add the plots to the grid
print(plot11, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(plot21, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(plot2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(plot31, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(plot3, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
print(plot41, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
print(plot4, vp = viewport(layout.pos.row = 4, layout.pos.col = 2))
dev.off()
