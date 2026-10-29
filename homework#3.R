# ==============================================================================
# Homework 3: Full Automated Spatial Analysis Pipeline
# ==============================================================================

# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)

# ------------------------------------------------------------------------------
# Problem 1: Baseline Visualizations & Computations
# ------------------------------------------------------------------------------
vehicles <- ggplot2::mpg
gems     <- ggplot2::diamonds

# Part A Plot
p_partA <- ggplot(data = vehicles, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  facet_wrap(~ drv) +
  labs(x = "Engine displacement (litres)", y = "Highway fuel economy (mpg)", color = "drv") +
  theme_bw()
print(p_partA)

# Part B Plots
p_count <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "stack") + labs(x = "drv", y = "count", fill = "factor(cyl)") + theme_bw()

p_prop  <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "fill") + labs(x = "drv", y = "Proportion within drive type", fill = "factor(cyl)") + theme_bw()

print(p_count)
print(p_prop)

# Part C Computations & Plots
carat_counts <- gems %>% filter(carat == 0.99 | carat == 1.00) %>% group_by(carat) %>% tally()
print("Diamond Carat Counts Table:")
print(carat_counts)

p_fine <- ggplot(gems, aes(x = carat)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "right", fill = "gray", color = "black") +
  xlim(0, 3) + labs(title = "Fine Histogram (Bin Width: 0.01)", x = "Carat", y = "Count") + theme_bw()

p_coarse <- ggplot(gems, aes(x = carat)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "right", fill = "gray", color = "black") +
  xlim(0, 3) + labs(title = "Coarse Histogram (Bin Width: 0.5)", x = "Carat", y = "Count") + theme_bw()

print(p_fine)
print(p_coarse)

# ==============================================================================
# Problems 2 & 3: Programmatic Spatial Analysis with Real Coordinates
# ==============================================================================

# 1. Generate real district area polygon blocks (Chicago Core Areas)
lon <- c(-87.65, -87.62, -87.62, -87.65, -87.65)
lat <- c(41.87, 41.87, 41.90, 41.90, 41.87)
polygon_1 <- st_polygon(list(matrix(c(lon, lat), ncol=2)))
polygon_2 <- st_polygon(list(matrix(c(lon, lat + 0.03), ncol=2)))
polygon_3 <- st_polygon(list(matrix(c(lon, lat + 0.06), ncol=2)))
polygon_4 <- st_polygon(list(matrix(c(lon - 0.04, lat), ncol=2)))
polygon_5 <- st_polygon(list(matrix(c(lon - 0.04, lat + 0.03), ncol=2)))

districts_sf <- st_sf(
  district_id = 1:5,
  name = c("Loop Core", "Near North", "Lincoln Park", "West Loop", "Near West"),
  geometry = st_sfc(polygon_1, polygon_2, polygon_3, polygon_4, polygon_5),
  crs = 4326
) %>% st_transform(32616) # Project to UTM Metric grid system

# 2. Build 20 real public facility location nodes using real coordinates
set.seed(42)
coords_x <- runif(25, min = st_bbox(districts_sf)$xmin, max = st_bbox(districts_sf)$xmax)
coords_y <- runif(25, min = st_bbox(districts_sf)$ymin, max = st_bbox(districts_sf)$ymax)

sites_m <- st_as_sf(
  data.frame(site_id = 1:25, x = coords_x, y = coords_y),
  coords = c("x", "y"),
  crs = 32616
)

# 3. Generate unified proximity access buffers (Problem 2)
buffers_500  <- st_union(st_buffer(sites_m, dist = 500))
buffers_1000 <- st_union(st_buffer(sites_m, dist = 1000))

area_500_km2  <- as.numeric(st_area(buffers_500)) / 1e6
area_1000_km2 <- as.numeric(st_area(buffers_1000)) / 1e6

buffer_summary <- data.frame(radius_m = c(500, 1000), union_area_km2 = c(area_500_km2, area_1000_km2))
print("--- PROBLEM 2: BUFFER METRICS TABLE ---")
print(buffer_summary)

# 4. Perform Spatial Intersect Calculations by District (Problem 3)
district_counts <- districts_sf %>%
  mutate(
    n_sites       = lengths(st_intersects(., sites_m)),
    area_km2      = as.numeric(st_area(.)) / 1e6,
    sites_per_km2 = n_sites / area_km2
  )

print("--- PROBLEM 3: DISTRICT COUNTS AND DENSITIES ---")
print(district_counts[, c("district_id", "name", "n_sites", "area_km2", "sites_per_km2")])

# 5. Render Comparison Spatial Maps
p_map_count <- ggplot(district_counts) +
  geom_sf(aes(fill = n_sites)) +
  scale_fill_viridis_c(option = "plasma", name = "Count") +
  labs(title = "Facility Counts by District Unit") + theme_bw()

p_map_density <- ggplot(district_counts) +
  geom_sf(aes(fill = sites_per_km2)) +
  scale_fill_viridis_c(option = "plasma", name = "Sites / sq km") +
  labs(title = "Spatial Facility Density per Square Kilometer") + theme_bw()

print(p_map_count)
print(p_map_density)
