library(stringr)
library(zonebuilder)
library(tmap)
library(raster)
library(exactextractr)
library(tmaptools)

# download gpkg from https://communitiesopendata-communities.hub.arcgis.com/datasets/4da63019f25546aa92a922a5ea682950_0/explore?location=52.533125%2C-2.489482%2C7.17
ind_dep <- st_read("LSOA_IMD2025_WGS84_-4854136717238973930.gpkg") |> 
  st_transform(4326)

# pick out cities interested in
cities <- c("Manchester", "Bristol", "Oxford", "Sheffield")

# list for the clock board polygons to be added
clocks <- list()
for (c in cities){
  
  # use tmap tools function to get coords of city centre
  coords <- geocode_OSM(c)
  cc <- st_point(coords$coords) |> 
    st_sfc(crs = 4326) |> 
    st_cast("POINT")
  
  ## draw the zones based on centre point. zb_zone automatically gets the boundary box of the city from tmaptools
  clock_zones <- zb_zone(cc)
  
  # intersect the ind polygons based on the zones
  ext_poly <- ind_dep[clock_zones,]
  
  # get the extent of zones
  bb <- st_bbox(clock_zones)
  
  # create raster
  r <- raster(nrows = 100, ncols = 100,xmn = bb[1], xmx = bb[3], ymn = bb[2], ymx = bb[4])
  # define crs
  crs(r) <- 4326
  
  # grid the imd data
  gridded <- terra::rasterize(ext_poly,r,field = "IMDDecil", fun = "mean")
  
  # take the mean of all grid cells in each clock board zone and return as a df
  clock_ind <- exact_extract(gridded,clock_zones, fun = "mean", force_df = TRUE)
  
  # order is the same so simply create a column with the IMD Decile
  clock_zones$`IMD Decile` <- clock_ind$mean
  
  ##define tmap mode
  tmap_mode("plot")
  
  # pick out a palette, this one is colour blind freindly
  pal <- cols4all::c4a("gmt.drywet", n = max(ind_dep$IMDDecil),reverse = TRUE)
  # define breaks that cover the full range so the scale is the same for each plot
  bks <- seq(1,max(ind_dep$IMDDecil))
  # only want the legend on the final plot which will go top right so do a logic test
  if(which(c==cities)<NROW(cities)){
    ## plot clock board
    tm1 <- tm_shape(clock_zones) + 
      tm_polygons(fill = "IMD Decile", fill.scale = tm_scale_intervals(values = pal, breaks = bks),col_alpha = 0.1) +
      tm_text("label", size = 4/5, col = "black", col_alpha = 0.5) + tm_legend(show = FALSE, position = c(0.92,0.988))+
      tm_layout(frame = FALSE)+
      tm_title(c, fontface = "bold", position = c(0.02,0.988))
    
  } else {
    
    ## plot  clock board
    tm1 <- tm_shape(clock_zones) + 
      tm_polygons(fill = "IMD Decile", fill.scale = tm_scale_intervals(values = pal, breaks = bks),col_alpha = 0.1) +
      tm_text("label", size = 4/5, col = "black") + tm_legend(position = c(0.9,0.988))+
      tm_layout(frame = FALSE)+
      tm_title(c, fontface = "bold", position = c(0.02,0.988))
    
  }
  
  # assign the name of the city to the object
  assign(c,tm1)
  clock_zones$city <- c
  clocks[[c]] <- clock_zones
  
}

# arrange plots
tm2 <- tmap_arrange(Manchester, Sheffield, Bristol, Oxford)

# balanced dpi with width and height so the text is readable but not overlapping
tmap_save(tm2,"all_clocks.png", width = 5000, height = 4000, dpi = 320)

# get the data for the cities to look for patterns
all_clocks <- do.call(rbind,clocks)
