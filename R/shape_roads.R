#' loadShape_US_Roads import and load a US Primary Roads shapefile
#'
#' @return
#' @export
#'
#' @examples
load_shape_us_roads<-function(shape_us_roads_dir,shape_us_roads_filename){
#Shape_US_Roads_dir = "C:/Users/Mohtadi.Nadjar/heatmapR/Template/shape/roads/us/tl_2016_us_primaryroads"
#Shape_US_Roads_filename = "tl_2016_us_primaryroads"
Shape_US_Roads <- readOGR(shape_us_roads_dir,shape_us_roads_filename)
Shape_US_Roads <- spTransform(Shape_US_Roads, osm())
Shape_US_Roads@data$id <-rownames(Shape_US_Roads@data)
Shape_US_Roads.df <- tidy(Shape_US_Roads) #fortify
Shape_US_Roads.df <- merge(Shape_US_Roads.df, Shape_US_Roads@data, by="id")
rm(Shape_US_Roads)
rm(Shape_US_Roads_dir)
rm(Shape_US_Roads_filename)
return(Shape_US_Roads.df)
}


