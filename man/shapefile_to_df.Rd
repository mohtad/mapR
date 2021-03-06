% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shapefile_to_df.R
\name{shapefile_to_df}
\alias{shapefile_to_df}
\title{shapefile_to_df import and load a boundaries shapefile}
\usage{
shapefile_to_df(shape_dir, shape_filename, identifier, show_data = FALSE)
}
\arguments{
\item{shape_dir}{Path to the directory where the shapefile is store}

\item{shape_filename}{Shapefile file name.}

\item{identifier}{Unique boundary identifier to describe the polygon. identifier = "ZCTA5CE10". Canada identifier = "CFSAUID"}

\item{show_data}{Retrieve the attribute data as a global variable(see: spacial_object.data). Only use show_data if you need to check the available attributes to be used in boundary_identifier.}
}
\value{
a spacial object as a data.frame
}
\description{
shapefile_to_df import and load a boundaries shapefile
}
\examples{
#Canadian FSA
Download shapefile(.shp) from http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm
unzip gfsa000b11a_e.zip in C:/SHAPEFILES/Forward_sortation_areas
shape_dir = "C:/SHAPEFILES/Forward_sortation_areas"
shape_filename = "gfsa000b11a_e"
identifier = "CFSAUID"
shape_boundaries<-shapefile_to_df(shape_dir, shape_filename,identifier)

#North America Major Roads
Download shapefile(.shp) from https://catalog.data.gov/dataset/usgs-small-scale-dataset-north-american-atlas-roads-200406-shapefile
unzip road00l_shp_nt00308.tar.gz in C:/SHAPEFILES/Major_Roads
shape_dir = "C:/SHAPEFILES/Major_Roads"
shape_filename = "road_l"
boundary_identifier = "ROAD_L_"
shape_roads<-shapefile_to_df(shape_dir, shape_filename,identifier)

#USA ZCTA
Download shapefile(.shp) from http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_zcta510_500k.zip
unzip cb_2015_us_zcta510_500k.zip in C:/SHAPEFILES/ZCTA
shape_dir = "C:/SHAPEFILES/ZCTA"
shape_filename = "cb_2015_us_zcta510_500k"
identifier = "ZCTA5CE10"
shape_boundaries<-shapefile_to_df(shape_dir, shape_filename,identifier)

#USA Primary Roads
Download shapefile(.shp) from http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_zcta510_500k.zip
unzip tl_2016_us_primaryroads.zip in C:/SHAPEFILES/us_primaryroads
shape_dir = "C:/SHAPEFILES/us_primaryroads"
shape_filename = "tl_2016_us_primaryroads"
identifier = "FULLNAME"
shape_roads<-shapefile_to_df(shape_dir, shape_filename,identifier)

}
