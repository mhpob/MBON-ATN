library(dplyr)
library(sf)
library(lwgeom)
library(units)

stations <- st_read(
    'Data and Imports/deployed_sites.csv',
    options = c(
        'X_POSSIBLE_NAMES=longitude',
        'Y_POSSIBLE_NAMES=latitude'
    ),
    crs = 4326
)

coast <- st_read('Data and Imports/mapping/Mid-Atlantic/matl_states_land.shp')

t1c <- stations |> 
        filter(sites == 'T-1C')

# mb1_split <- st_nearest_points(
#     coast |> 
#         st_combine() |> 
#         st_make_valid(),
#      t1c |> 
#         st_transform(st_crs(coast))
# ) |> 
#     st_transform(4326) |> 
#     st_geod_segmentize(set_units(1, km)) |> 
#     st_transform(32618) |> 
#     st_linesubstring(0, 1/2, 1) |> 
#     st_endpoint() |> 
#     st_transform(4326)

mb1_transect <- matrix(c(-75.08107, 38.336), 1, 2) |> 
    st_point() |> 
    st_sfc(crs = 4326) |> 
    c(st_geometry(t1c)) |> 
    st_combine() |> 
    st_cast('LINESTRING') |> 
    st_geod_segmentize(set_units(1, km)) |> 
    st_transform(32618) |> 
    st_linesubstring(0, 1/2, 1) |> 
    st_endpoint() |> 
    st_transform(4326)

        

t1_a5 <- stations |> 
    filter(
        sites %in% c('T-1C', 'A-5C')
    ) |> 
    st_combine() |> 
    st_cast('LINESTRING') |>
    st_geod_segmentize(set_units(1, km)) |> 
    st_transform(32618) 

mb2 <- st_linesubstring(t1_a5, 0, 1/3, 1) |> 
    st_endpoint() |> 
    st_transform(4326)
mb3 <- st_linesubstring(t1_a5, 1/3, 2/3, 1) |> 
    st_endpoint() |> 
    st_transform(4326)


a5_t2 <- stations |> 
    filter(
        sites %in% c('A-5C', 'T-2C')
    ) |> 
    st_combine() |> 
    st_cast('LINESTRING') |>
    st_geod_segmentize(set_units(1, km)) |> 
    st_transform(32618) 

mb4 <- st_linesubstring(a5_t2, 0, 1/3, 1) |> 
    st_endpoint() |> 
    st_transform(4326)
mb5 <- st_linesubstring(a5_t2, 1/3, 2/3, 1) |> 
    st_endpoint() |> 
    st_transform(4326)

mbon_sites <- c(mb1_transect, mb2, mb3, mb4, mb5) |> 
  st_as_sf() |> 
  mutate(
    sites = paste0('MBON', 1:5),
    latitude = st_coordinates(x)[,2],
    longitude = st_coordinates(x)[,1],
    historical_data_type = NA,
    proposed_data_type = "Biotelemetry"
    ) |> 
    rename(geometry = 'x') |> 
    rbind(stations |> 
        filter(sites %in% c("T-1C", "A-5C", "T-2C", "T-3C")))

st_write(mbon_sites, "Data and Imports/proposed_sites.csv")

# mbon_sites |>
#   st_distance() 

####
# Closest to land MB1 - T1C = 5493m
# Transect MB1 - T1C = 5760m
# T1C - MB2 = 6571m
# MB2 - MB3 = 6571m
# MB3 - A5C = 6571m
# A5C - MB4 = 6338m
# MB4 - MB5 = 6338m
# MB5 - T2C = 6338m
# T2C - T3C = 13519