#' loadShape_US_ZCTA import and load a US ZCTA shapefile
#'
#' @return
#' @export
#'
#' @examples
load_shape_us_zcta<-function(shape_us_zcta_dir,shape_us_zcta_filename){
#shape_us_zcta_dir = "C:/Users/Mohtadi.Nadjar/heatmapR/Template/shape/zcta/us/cb_2015_us_zcta510_500k"
#shape_us_zcta_filename = "cb_2015_us_zcta510_500k"
Shape_US_ZCTA = readOGR(shape_us_zcta_dir,shape_us_zcta_filename)
Shape_US_ZCTA <- spTransform(Shape_US_ZCTA, osm())#we need to use a mercator projection: osm()
Shape_US_ZCTA.df <- tidy(Shape_US_ZCTA, region="ZCTA5CE10")
rm(Shape_US_ZCTA)
rm(Shape_US_ZCTA_dir)
rm(Shape_US_ShapeZCTA_filename)
return(Shape_US_ZCTA.df)
}
