#' shapefile_to_df import and load a boundaries shapefile
#'
#'@param shape_boundaries_dir Unique boundary identifier. US boundary_identifier = "ZCTA5CE10". Canada boundary_identifier = "CFSAUID"
#'@param shape_boundaries_filename Unique boundary identifier. US boundary_identifier = "ZCTA5CE10". Canada boundary_identifier = "CFSAUID"
#'@param boundary_identifier Unique boundary identifier. US boundary_identifier = "ZCTA5CE10". Canada boundary_identifier = "CFSAUID"
#'
#' @return a spacial object as a data.frame
#' @export
#'
#' @examples
#'
#' @importFrom rgdal readOGR
#' @importFrom sp spTransform
#' @importFrom broom tidy
#' @importFrom OpenStreetMap osm
#'
shapefile_to_df<-function(shape_boundaries_dir,shape_boundaries_filename, boundary_identifier=NULL, show_data = FALSE){
  spacial_object = readOGR(shape_boundaries_dir,shape_boundaries_filename)
  spacial_object <- spTransform(spacial_object, osm())#we need to use a mercator projection: osm()
  if(show_data == TRUE)
  {
    spacial_object.data<<-spacial_object@data
  }
  spacial_object.df <- tidy(spacial_object, region=boundary_identifier)
  return(spacial_object.df)
}

