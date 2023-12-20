###############################################################################
###
### Population exposure to multiple air pollutants and its compound episodes in Europe
### December 2023
### Zhaoyue Chen
### 
################################################################################

library("tmap")
library(ggplot2)
library(cols4all)
suppressMessages(library(giscoR)) # Functions: gisco_get_countries
suppressMessages(library(sf)) # Functions: st_read
suppressMessages(library(tidyverse)) # Functions: read_delim


### Function for creating a double y annual change plot
# Parameters:
# - df: Dataframe containing the data
# - Y1: Variable for the stacked bar plot on the primary y-axis
# - df0: Dataframe containing secondary y-axis variable
# - Y2: Variable for the line plot on the secondary y-axis
# - mtitle: Title for the plot
slope_plot <- function(df, Y1, df0, Y2, mtitle) {
  # Set color palette
  economist_colors <- c('#97CFD0', '#00A2B3', '#CF3E53', '#F1788D')
  priceColor <- rgb(0.2, 0.6, 0.9, 1)
  
  # Calculate scaling coefficient
  coeff <- max(df0[[Y2]], na.rm = TRUE) / max(df[[Y1]], na.rm = TRUE)
  df <- df %>% left_join(df0)
  
  # Create the stacked bar and line plot
  p <- ggplot(df, aes(x = year)) + 
    geom_bar(aes(fill = region, y = !!sym(Y1)), position = "stack", stat = "identity") +
    geom_line(aes(y = !!sym(Y2) / coeff), size = 2, color = priceColor) +
    scale_x_continuous(breaks = c(2003, 2005, 2010, 2015, 2019)) +
    scale_y_continuous(labels = scales::label_number_si(), sec.axis = sec_axis(~.*coeff, name = "Annual Concentration")) +
    theme_minimal() +
    scale_fill_manual(values = economist_colors) +
    theme(
      axis.text.y.right = element_text(color = priceColor),
      axis.title.y.right = element_text(color = priceColor, size = 13, face = "bold"),
      legend.position = c(0.15, 0.9)
    ) +
    labs(x = "Year", y = "Population within annual limits (millions)", fill = " ") +
    ggtitle(mtitle)
  
  return(p)
}

# Read annual limit trend data
foldout <- paste0("/Yourfolder/inputdata/")
annual_re <- readRDS(file = paste0(foldout, "annuallimit_trend.Rdata"))

var0 <- "pm25v0"
mtitle <- "a) Trend for PM25"
pm25_slope <- slope_plot(annual_re[[2]], paste0(var0, "_longterm_exposed_prop_pop"), df0 = annual_pol, Y2 = paste0(var0, "_pw"), mtitle = mtitle)

var0 <- "pm10v0"
mtitle <- "b) Trend for PM10"
pm10_slope <- slope_plot(annual_re[[2]], paste0(var0, "_longterm_exposed_prop_pop"), df0 = annual_pol, Y2 = paste0(var0, "_pw"), mtitle = mtitle)

var0 <- "no2pred"
mtitle <- "c) Trend for NO2"
no2_slope <- slope_plot(annual_re[[2]], paste0(var0, "_longterm_exposed_prop_pop"), df0 = annual_pol_s, Y2 = paste0(var0, "_pw"), mtitle = mtitle)

var0 <- "o3_8hpred"
mtitle <- "d) Trend for O3"
o3_slope <- slope_plot_o3(annual_re[[2]], paste0(var0, "_longterm_exposed_prop_pop"), df0 = annual_pol, Y2 = paste0(var0, "_pw"), mtitle = mtitle)

library(grid)
foldout <- paste0("//Yourfolder/docs/scriptforplot/figure/")
if (!file_test("-d", foldout)) {
  dir.create(foldout, recursive = TRUE)
}
pdf(paste0(foldout, "Figure5_popwithinannuallimit.pdf"), width = 12, height = 12)
# create a 2X2 grid
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))

# Define the plots
plot11 <- pm25_slope
plot21 <- pm10_slope
plot31 <- no2_slope
plot41 <- o3_slope

# Add the plots to the grid
print(plot11, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot21, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(plot31, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(plot41, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
dev.off()
