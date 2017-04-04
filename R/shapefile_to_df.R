#' shapefile_to_df import and load a boundaries shapefile
#'
#'@param shape_dir Path to the directory where the shapefile is store
#'@param shape_filename Shapefile file name.
#'@param identifier Unique boundary identifier to describe the polygon. identifier = "ZCTA5CE10". Canada identifier = "CFSAUID"
#'@param show_data Retrieve the attribute data as a global variable(see: spacial_object.data). Only use show_data if you need to check the available attributes to be used in boundary_identifier.
#'
#' @return a spacial object as a data.frame
#' @export
#'
#' @examples
#' #Canadian FSA
#' Download shapefile(.shp) from http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm
#' unzip gfsa000b11a_e.zip in C:/SHAPEFILES/Forward_sortation_areas
#' shape_dir = "C:/SHAPEFILES/Forward_sortation_areas"
#' shape_filename = "gfsa000b11a_e"
#' identifier = "CFSAUID"
#' shape_boundaries<-shapefile_to_df(shape_dir, shape_filename,identifier)
#'
#' #North America Major Roads
#' Download shapefile(.shp) from https://catalog.data.gov/dataset/usgs-small-scale-dataset-north-american-atlas-roads-200406-shapefile
#' unzip road00l_shp_nt00308.tar.gz in C:/SHAPEFILES/Major_Roads
#' shape_dir = "C:/SHAPEFILES/Major_Roads"
#' shape_filename = "road_l"
#' boundary_identifier = "ROAD_L_"
#' shape_roads<-shapefile_to_df(shape_dir, shape_filename,identifier)
#'
#' #USA ZCTA
#' Download shapefile(.shp) from http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_zcta510_500k.zip
#' unzip cb_2015_us_zcta510_500k.zip in C:/SHAPEFILES/ZCTA
#' shape_dir = "C:/SHAPEFILES/ZCTA"
#' shape_filename = "cb_2015_us_zcta510_500k"
#' identifier = "ZCTA5CE10"
#' shape_boundaries<-shapefile_to_df(shape_dir, shape_filename,identifier)
#'
#' #USA Primary Roads
#' Download shapefile(.shp) from http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_zcta510_500k.zip
#' unzip tl_2016_us_primaryroads.zip in C:/SHAPEFILES/us_primaryroads
#' shape_dir = "C:/SHAPEFILES/us_primaryroads"
#' shape_filename = "tl_2016_us_primaryroads"
#' identifier = "FULLNAME"
#' shape_roads<-shapefile_to_df(shape_dir, shape_filename,identifier)
#'
#' @import gpclib
#' @importFrom rgdal readOGR
#' @importFrom sp spTransform
#' @importFrom broom tidy
#' @importFrom OpenStreetMap osm
#'
shapefile_to_df<-function(shape_dir,shape_filename, identifier, show_data = FALSE){
  spacial_object = readOGR(shape_dir,shape_filename)
  spacial_object <- spTransform(spacial_object, osm())#we need to use a mercator projection: osm()
  if(isTRUE(show_data))
  {
    spacial_object.data<<-spacial_object@data
  }
  spacial_object.df <- tidy(spacial_object, region=identifier)
  return(spacial_object.df)
}

