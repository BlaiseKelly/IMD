library(openxlsx)
library(sf)
library(dplyr)

# define LA name
la_name = "Bristol"

# read in all LAs
LAs = st_read("https://github.com/BlaiseKelly/stats19_stats/releases/download/LA_boundaries/LA.gpkg")

# pick out LA shape
city_shp = filter(LAs, grepl(la_name,LAD22NM)) |> 
  st_transform(4326)

# get lsoa boundaries
lsoa_boundaries_21 = st_read("https://github.com/BlaiseKelly/stats19_stats/releases/download/boundaries-v1.0/lsoa_boundaries.gpkg") |> 
  st_transform(4326)

# find centre points to neatly intersect with city shape
lsoa_centroids = st_centroid(lsoa_boundaries_21)

# find LSOAs in city shape
city_lsoa = lsoa_centroids[city_shp,]

# pick out LA LSOAs
city_imd = filter(ind_dep,LSOA21CD %in% city_lsoa$LSOA11CD)

# get LSOA population data
gb_pop <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2022revisednov2025tomid2024/sapelsoasyoa20222024.xlsx",
                    sheet = "Mid-2024 LSOA 2021",startRow = 4)

# population for LA
city_pop_lsoa <- gb_pop |> 
  dplyr::select(LSOA.2021.Code, Total) |> 
  filter(LSOA.2021.Code %in% city_lsoa$LSOA11CD)

# create df for describing each Decile
decile_match = c("Least deprived 10%","Less deprived 10-20%","Less deprived 20-30%","Less deprived 30-40%","Less deprived 40-50%",
                 "More deprived 10-20%","More deprived 20-30%","More deprived 30-40%","More deprived 40-50%","Most deprived 10%")  

# add the numbers
decile_match$IMDDecil = seq(1,10,1)

# join with IMD and calculate totals and pc breakdown for each decile
city_imd_pop = city_imd |> 
  left_join(city_pop_lsoa, by = c("LSOA21CD" = "LSOA.2021.Code")) |> 
  st_set_geometry(NULL) |> 
  group_by(IMDDecil) |> 
  summarise(pop = sum(Total)) |> 
  mutate(imd_pc = pop/sum(pop))

# join with IMD and calculate totals and pc breakdown for each decile
city_imd_pop = city_imd |> 
  left_join(city_pop_lsoa, by = c("LSOA21CD" = "LSOA.2021.Code")) |> 
  st_set_geometry(NULL) |> 
  group_by(IMDDecil) |> 
  summarise(pop = sum(Total)) |> 
  mutate(imd_pc = pop/sum(pop)*100) |> 
  left_join(decile_match,by = "IMDDecil")

# sum up % of most and least deprived population
city_imd_ML = city_imd_pop |> 
  mutate(ML = str_sub(imd_decile, 1,1)) |> 
  group_by(ML) |> 
  summarise(pc = sum(imd_pc),
            pop = sum(pop))


