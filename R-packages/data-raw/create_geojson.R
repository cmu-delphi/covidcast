library(ggplot2)
library(sf)
library(rmapshaper)
library(dplyr)

simplify_shape <- function(shp_obj, crs, plot_name = NULL)
{
  # Transform to desired CRS
  shp_simple <- st_transform(shp_obj, crs)

  # Remove small parts of polygons (islands)
  num_polygon <- sapply(st_geometry(shp_simple), length)
  for ( i in which(num_polygon > 1) )
  {
    z <- st_cast(shp_simple$geometry[i], "POLYGON")
    areas <- as.numeric(st_area(z)) / 10000
    index <- which((areas >= 0.05 * sum(areas)) | (areas == max(areas)))
    z <- z[index]
    shp_simple$geometry[i] <- st_union(z)
  }

  # Simplify and validate
  shp_simple <- rmapshaper::ms_simplify(shp_simple, keep = 0.05)
  shp_simple <- st_make_valid(shp_simple)

  # Plot to verify
  if (!is.null(plot_name))
  {
    dir.create("temp", FALSE)
    p <- ggplot(shp_simple) + geom_sf(size = 0.1) + theme_void()
    ggsave(file.path("temp", sprintf("%s.png", plot_name)), p)
  }

  # Back to Lat/Lon
  shp_simple <- st_transform(shp_simple, 4326)

  return(shp_simple)
}

save_shp <- function(shp_obj, map_name)
{
  # Save file locally as geojson
  if (file.exists(fe <- sprintf("%s.geojson.bz2", map_name)))
  {
    file.remove(fe)
  }
  st_write(shp_simple, sprintf("%s.geojson", map_name))
  system(sprintf("bzip2 %s.geojson", map_name))

  # Create compressed geojson file and print out file size
  fname <- sprintf("%s.geojson.bz2", map_name)
  oname <- file.path("../covidcast/inst/shapefiles", fname)
  cat(sprintf(
    "File size %s: %0.2f M\n",
    map_name,
    file.size(sprintf("%s.geojson.bz2", map_name)) / 1e6
  ))

  # Put the file in the covidcast package
  if (file.exists(oname)) { file.remove(oname) }
  file.rename(fname, oname)
}

# Run the functions above for the four geographies; Note, the projection is not
# great for the U.S. territories, but appears to work OK for simplification
for (map_name in c("msa", "state", "hrr", "county"))
{
  # Simplfy shapes using U.S. centered Albers (CRS => 5070)
  shp_obj <- read_sf(file.path("geo", map_name))
  shp_simple <- simplify_shape(shp_obj, 5070, map_name)
  save_shp(shp_simple, map_name)
}

# If you want to test a single state, you can do that here:
if (FALSE)
{
  map_name <- "county"
  fname <- sprintf("%s.geojson.bz2", map_name)
  oname <- file.path("../covidcast/inst/shapefiles", fname)
  geojson <- paste(readLines(oname), collapse = "")
  map_df <- sf::st_read(dsn = geojson, quiet = TRUE)

  map_df %>% filter(STATEFP == 26) %>%
    ggplot() + geom_sf() + theme_void()
}
