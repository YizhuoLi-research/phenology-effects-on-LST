#####   ggOceanMaps #####
library(sf)
library(ggOceanMaps)
library(ggspatial)

custom_colors <- c("-50" = "#660000",
                   "-40" = "#990000",
                   "-30" = "#CC3333",
                   "-20" = "#FF6633",
                   "-10" = "#FF9933",
                   "0"   = "#FFFFFF",
                   "10"  = "#66CCFF",
                   "20"  = "#3399CC",
                   "30"  = "#006699",
                   "40"  = "#003366",
                   "50"  = "#000033")
p <- basemap(limits =c(0,0,30,90), land.col = NA,  lon.interval = NULL,
             lat.interval = NULL,
             land.border.col = "black", land.size = 1) +
  layer_spatial(merged_diff_raster_2) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 28),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    panel.grid.major = element_line(size = 0.25, linetype = "dashed", color = "grey70"),
    panel.grid.minor = element_line(size = 0.25, linetype = "dashed", color = "grey70"),
    axis.ticks = element_blank(),   
    axis.title = element_blank(),
    legend.position = "bottom") +
    scale_fill_gradientn(colours = c("#990000", "#FF6633","#FFFFFF",
                                   "#66CCFF", "#3399CC","#006699", "#003366", "#000033" ), 
                       na.value = "transparent",
                       name = "ΔLST (℃)",
                       values = rescale(c(-20,-10,0,10,20,30,40,50)),
                       breaks = c(-20, 0, 20, 40, 50), 
                       limits=c(-20,50),
                       guide = guide_colorbar(title.position = "bottom",
                                              title.hjust = 0.5,
                                              barwidth = 60,
                                              barheight = 1.2,
                                              title.vjust = 0.5, 
                                              ticks = TRUE,
                                              ticks.colour = "white",
                                              ticks.linewidth = 3.0/.pt))   
p
ggsave(
  filename = "./0.figure/Fig.1_polar_legend.tiff",
  plot = p,  width = 15,  height = 15,  units = "in",  dpi = 300)
