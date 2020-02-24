library(dataRetrieval)
library(tidyverse)
library(sf)

#which gages in the Delaware have specific conductance?
drb_extent <- st_read('drb_extent/DRB_Extent.shp') %>% 
  st_transform(4326)
drb_bbox <- drb_extent %>% st_bbox() %>% 
  round(digits = 2)

prms_streams <- st_read('delaware_PRMS_streams.geojson')

drb_bbox_sites <- readNWISdata(service="site",
                               bBox = drb_bbox,
                               parameterCd = "00095",
                               #siteOutput = "expanded",
                               startDate = "2018-10-01")
drb_bbox_sites_sf <- st_as_sf(drb_bbox_sites, coords = c("dec_long_va", "dec_lat_va"),
                              crs = 4326)
#intersect with polygon
intersection <- st_intersects(drb_bbox_sites_sf, drb_extent$geometry)
drb_poly_sites <- drb_bbox_sites_sf %>% filter(apply(intersection, 1, any)) 
#1.8m observations here -- might take a while
drb_poly_data <- readNWISuv(siteNumbers = drb_poly_sites$site_no,
                            parameterCd = "00095",
                            startDate = "2018-10-01")
saveRDS(drb_poly_data, 'drb_poly_data.rds')

length(unique(drb_poly_data$site_no))

#which gages have largest range of values?
min_max_summary <- drb_poly_data %>% group_by(site_no) %>% 
  summarize(n=n(), min=min(X_00095_00000), max = max(X_00095_00000)) %>% 
  mutate(range = max - min)

#map of gages with top 15 highest SC ranges
top15_range <- top_n(min_max_summary, n = 15, wt = range)
top15_sites <- drb_poly_sites %>% filter(site_no %in% top15_range$site_no) %>% 
  st_crop(ymin = 39.6, ymax=80, xmax=0, xmin=-90)
plot(drb_extent$geometry)
plot(prms_streams$geometry, col = 'blue', add=TRUE)
plot(top15_sites$geometry, col = "red", add = TRUE)

#plot the time series
top15_data <- drb_poly_data %>% filter(site_no %in% top15_sites$site_no) %>% 
  filter(dateTime > as.Date('2020-01-01'))
ggplot(top15_data, aes(dateTime, X_00095_00000)) +
  geom_line() +
  facet_wrap('site_no', scales = "free_y")
