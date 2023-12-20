###############################################################################
###
### Population exposure to multiple air pollutants and its compound episodes in Europe
### December 2023
### Zhaoyue Chen
### 
################################################################################
library(sf)
library(raster)
library(ncdf4)
library(data.table)
library(dplyr)
library(terra)

# Read compound days major data
foldout <- paste0("/Yourfolder/docs/scriptforNC/")
df2 <- readRDS(file = paste0(foldout, "compounddays_major.Rdata"))

library(ggplot2)
library(cols4all)

# Define Economist-style color palette for compound days
economist_colors <- c(c4a('tableau.summer')[3:7], "grey")

# Create stacked bar time series plot for compound days
p <- ggplot(df2[!(df2$variable %in% c("pop", "two_events_days_pw")), ], aes(fill = variable, y = value, x = year)) + 
  geom_bar(position = "stack", stat = "identity") + 
  facet_wrap(facets = ~ region, ncol = 5) + 
  xlim(2002, 2019) +
  scale_fill_manual(values = economist_colors, labels = c("Four Pol Days", "Three Pol Days", "PM25-PM10 Days", "PM25-NO2 Days", "PM25-O3 Days", "Rest Days")) +
  labs(x = "Year", y = "Compound Unclean Air Days", fill = "Compound Days") +
  theme_minimal()

# Read three compound days major data
foldout <- paste0("/Yourfolder/inputdata/")
df1 <- readRDS(file = paste0(foldout, "threecompounddays_major.Rdata"))

# Define another color palette for three compound days
economist_colors <- c(c4a('brewer.reds'))

# Create stacked bar time series plot for three compound days
p1 <- ggplot(df1[!(df1$variable %in% c("pop", "two_events_days_pw", "three_events_days_pw", "four_events_days_pw")), ], aes(fill = variable, y = value, x = year)) + 
  geom_bar(position = "stack", stat = "identity") + 
  facet_wrap(facets = ~ region, ncol = 5) + 
  xlim(2002, 2019) +
  scale_fill_manual(values = economist_colors, labels = c("PM25-PM10-NO2 Days", "PM25-PM10-O3 Days", "PM25-O3-NO2 Days", "PM10-O3-NO2 Days")) +
  labs(x = "Year", y = "Three Pol Compound Days", fill = "Three Compound Days") +
  theme_minimal()

library(grid)
foldout <- paste0("/Yourfolder/docs/scriptforplot/figure/")
if (!file_test("-d", foldout)) {
  dir.create(foldout, recursive = TRUE)
}

pdf(paste0(foldout, "Figure5_CEs.pdf"), width = 10, height = 6)
# create a 2X1 grid
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))

# Add the plots to the grid
print(p, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
dev.off()
