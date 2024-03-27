#######################################################
# LOAD REQUIRED PACKAGES AND SET UP PREFERENCES
#######################################################
rm(list = ls())
options(java.parameters = "- Xmx16g")
gc()
#options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
# clear the R environment


if (!require("pacman"))
  install.packages("pacman")

pacman::p_load(
  tidyverse,
  dismo,
  raster,
  here,
  corrplot,
  Hmisc,
  patchwork,
  ecospat,
  # kuenm,
  gridSVG,
  gridExtra,
  grid,
  ENMeval,
  spThin,
  viridis,
  viridisLite,
  mapdata,
  maptools,
  scales,
  geosphere,
  rgdal,
  ggtext,
  rJava,
  rgeos,
  sp,
  sf,
  ggspatial,
  ecospat,
  rnaturalearth,
  rnaturalearthdata,
  megaSDM,
  caret, 
  terra,
  geodata,
  usdm,
  tidyterra,
  DHARMa,
  car,
  ggeffects,
  devtools,
  caret,
  factoextra,
  ggfortify,
  tcltk2,
  igraph,
  blockCV
)

if(!require("InformationValue")){
  devtools::install_github("selva86/InformationValue")
}

if(!require("megaSDM")){
  devtools::install_github("brshipley/megaSDM", build_vignettes = TRUE)
}

if(!require("ggConvexHull")){
  devtools::install_github("cmartin/ggConvexHull")
}

# Change ggplot theme
theme_set(
  theme_classic() +
    theme(
      panel.border = element_rect(colour = "black",
                                  fill = NA),
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(margin = unit(c(2, 0, 0, 0),
                                                "mm")),
      axis.title.y = element_text(margin = unit(c(0, 4, 0, 0),
                                                "mm")),
      legend.position = "none"
    )
)

# Set the theme for the maps
theme_opts <- list(
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = 'white', colour = NA),
    plot.background = element_rect(),
    axis.line = element_blank(),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    axis.title.x = element_text(colour = "black"),
    axis.title.y = element_text(colour = "black"),
    plot.title = element_text(colour = "black"),
    panel.border = element_rect(fill = NA),
    legend.key = element_blank()
  )
)
