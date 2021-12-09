library(dplyr); library(sf)

md_coast <- read_sf('data and imports/mapping/mid-atlantic/matl_states_land.shp') %>%
  filter(!is.na(STATE_NAME)) %>%
  st_transform(4326)

atl_coast <- read_sf('data and imports/mapping/natural earth/ne_10m_coastline.shp')%>%
  st_transform(4326)

bathy <- read_sf('data and imports/mapping/bathymetry/test.gpkg') %>% 
                 # query = 'select * from test where elev_m > -50') %>%
  st_transform(4326) %>%
  filter(as.numeric(st_length(.))> 30000) 
# %>%
  # st_crop(xmin = -75.15, xmax = -74.6, ymin = 38.2, ymax = 38.5)

wea <- st_read('data and imports/mapping/offshore wind layers.gdb')
# , 
          # query = "select * from BOEM_Wind_Leases_as_of_Aug_3_2021 where State = 'Maryland'")

sites <- read.csv('data and imports/deployed_sites.csv') %>%
  st_as_sf(coords = c('longitude', 'latitude'),
           crs = 4326)



library(ggplot2); library(ggspatial); library(ragg)

## Make a one-column map
main <-
  ggplot() +
  geom_sf(data = bathy, color = 'gray', size = 0.1) +
  geom_label(data = data.frame(
    lab = c('10', '20', '20', '30', '40', '40'),
    x = c(-75.1, -74.839, -74.85, -74.695, -74.595, -74.399),
    y = c(38.27, 38.26, 38.15, 38.425, 38.2, 38.30)
  ),
  aes(x = x, y = y, label = lab),
  label.size = 0, label.padding = unit(0.1, "lines"), size = 1) +
  geom_sf(data = md_coast, size = 0.1) +
  geom_sf(data = wea, fill = NA, color = 'gray') +
  geom_sf(data = sites, aes(alpha = exp_type,
                            size = exp_type,
                            shape = inst_type)) +
  scale_alpha_manual(values = c(0.2, 1), guide = 'none') +
  scale_size_manual(values = c(2, 4), guide = 'none') +
  coord_sf(xlim = c(-75.15, -74.34),
           ylim = c(38.14, 38.46)) +
  annotation_scale(text_cex = 0.5, height = unit(1, 'mm')) +
  annotation_north_arrow(location = 'tl') +
  labs(x = NULL, y = NULL, shape = 'Instrument') +
  theme_bw() +
  theme(axis.text = element_text(size = 12 / .pt),
        axis.text.y = element_text(angle = 45, vjust = 0),
        panel.grid = element_line(size = 0.1),
        axis.ticks = element_line(size = 0.1),
        plot.margin = unit(c(0, 0, 0, 0), 'mm'),
        legend.position = c(0.9, 0.82),
        legend.background = element_rect(fill=alpha('white', 0.7)))

inset <- ggplotGrob(
  ggplot() +
    geom_sf(data = bathy, color = 'lightgray', size = 0.1) + 
    geom_sf(data = wea, fill = NA, size = 0.1) +
    geom_sf(data = atl_coast, size = 0.1) +
    coord_sf(xlim = c(-77.5, -71),
             ylim = c(35.5, 41)) +
    annotate('rect', xmin = -75.15, xmax = -74.34, ymin = 38.14, ymax = 38.46,
             fill = NA, color = 'red', size  = 0.1)+
    theme_void() +
    theme(panel.background = element_rect(fill = 'white'))
)



agg_tiff("figures/mbon-atn_map.tif",
         # General ratio: 1065x536
         width = 6.5, height = 3.271,
         units = 'in', compression = 'lzw', res = 600)


main +
  annotation_custom(inset,
                    xmin = -74.47, xmax = -74.31,
                    ymin = 38.1, ymax = 38.3)

dev.off()

