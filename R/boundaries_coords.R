#'
#' Retrieve a list of boundaries' spacial coordinates(lat/long and mercator) from a shapefile
#'
#' @description Retrieve a list of boundaries with their spacial coordinates(lat/long and mercator) and a computed haversine distance in miles from the map center. Boundaries are defined in a shapefile dataframe(see method: shape_boundaries) as a list of vertices coordinates. The boundaries spacial coordinates is the center of gravity of its vertices.
#'
#' @param shape_boundaries List of vertices coordinates as dataframe. Each boundaries is defined as a set of vertices. The value is given using shape_boundaries method
#' @param map_center Spacial coordinates of a map center. The haversine distance in miles is computed if map_center is defined.
#'
#' @return list of spacial coordinates(lat/long and mercator) and a distance as data.frame
#' @export
#'
#' @examples
#' shape_boundaries<-shape_boundaries(shape_boundaries_dir, shape_boundaries_filename,boundary_identifier)
#' map_center = data.frame(long = c(-94.823686), lat = c(39.121296) )
#' boundaries_coords<-boundaries_coords.shape(shape_boundaries, map_center)
#'
#' @importFrom geosphere mercator distm distHaversine
#' @importFrom dplyr group_by summarise
#'
boundaries_coords.shape<-function(shape_boundaries,map_center=NULL){

  shape_boundaries_by_id <- group_by(shape_boundaries, id)
  boundaries_i <- summarise(shape_boundaries_by_id, mer_lat=mean(lat), mer_long = mean(long))

  #the zipcode coordinates are mercator coordinates, so we need to compute the spherical long/lat coordinate
  longlat = apply(boundaries_i,1,function(x){
    mer_lat=as.numeric(x['mer_lat']);
    mer_long=as.numeric(x['mer_long']);
    result<-mercator(c(mer_long,mer_lat),TRUE)
    if(!is.null(map_center)){
      long = result[1]
      lat = result[2]
      dist = distm (c(map_center$long, map_center$lat), c(long,lat), fun = distHaversine)*0.000621371;
      result<- cbind(result,dist)
    }
    return(result)
  })
  boundaries_i$long = longlat[1,]
  boundaries_i$lat = longlat[2,]
  if(!is.null(map_center)){
    boundaries_i$dist_to_center = longlat[3,]
  }
#  if(!is.null(map_center)){
#    boundaries_i$dist_to_center = apply(boundaries_i,1,function(x){
#      lat=as.numeric(x['lat']);
#      long=as.numeric(x['long']);
#      distm (c(map_center$long, map_center$lat), c(long,lat), fun = distHaversine)*0.000621371;
#  })
#  }
  return (boundaries_i)
}

#'
#' Retrieve a list of boundaries' spacial mercator coordinates for a list of boundaries lat/long coordinates.
#'
#' @description Retrieve a list of mercator boundaries with their spacial coordinates(lat/long and mercator) and a computed haversine distance in miles from the map center.
#'
#' @param data List of Longitude/latitude coordinates of the boundaries. The Dataset is a data.frame containing a set of three columns (id, long, lat). 'id' is a unique representation of the boundary(ex: ZCTA, FSA). 'id' type is character.
#' @param map_center Spacial coordinates of a map center. The haversine distance in miles is computed if map_center is defined.
#'
#'
#' @return list of spacial coordinates(lat/long and mercator) and a distance as data.frame
#' @export
#'
#' @examples
#' data = data.frame(long = c(-94.823686, -94), lat = c(39.121296, 39) )
#' map_center = data.frame(long = c(-94.823686), lat = c(39.121296) )
#' boundaries_coords<-boundaries_coords.df(data, map_center)
#'
#' @importFrom geosphere mercator distm distHaversine
#'
boundaries_coords.df<-function(data,map_center=NULL){
  #the zipcode coordinates are sperical long/lat coordinates, so we need to compute the mercator coordinates.
  boundaries_i<-data
  mer = apply(boundaries_i,1,function(x){
    lat=as.numeric(x['lat']);
    long=as.numeric(x['long']);
    result<-mercator(c(long,lat))
    if(!is.null(map_center)){
      dist = distm (c(map_center$long, map_center$lat), c(long,lat), fun = distHaversine)*0.000621371;
      result<- cbind(result,dist)
    }
    return(result)

  })
  boundaries_i$mer_long = mer[1,]
  boundaries_i$mer_lat = mer[2,]
  if(!is.null(map_center)){
    boundaries_i$dist_to_center = mer[3,]
  }

#  if(!is.null(map_center)){
#    boundaries_i$dist_to_center = apply(boundaries_i,1,function(x){
#      lat=as.numeric(x['lat']);
#      long=as.numeric(x['long']);
#      distm (c(map_center$long, map_center$lat), c(long,lat), fun = distHaversine)*0.000621371;
#    })
#  }
  return (boundaries_i)
}

#'
#' Internal method(do not use). Remove the overlapping boundaries
#'
#' @description Internal method(do not use). Dirty way to detect overlapping boundaries and only return one. Used in boundary label creation in heatmap.
#'
#' @param boundaries_coords List of boundaries and coordinates as a dataframe (id, lat, long)
#' @param distance distance in miles. If two boundaries are within 'distance', then one of the two boundaries is filtered out.
#'
#' @return List of boundaries and coordinates as a dataframe
#'
#' @examples
#'
#' @importFrom geosphere distm
#'
boundaries_coords_filter<-function(boundaries_coords, distance){
  n<-nrow(boundaries_coords)
  v<-numeric(n)
  for(i in 1:(n-1)) {
    m = i+1
    for(j in m:n){
      dist<-distm (c(boundaries_coords[i,]$long, boundaries_coords[i,]$lat), c(boundaries_coords[j,]$long,boundaries_coords[j,]$lat), fun = distHaversine)*0.000621371;
      if(is.na(dist)){
        print(paste(i,'-',j,'-',n))
      }
      if(dist<=distance){
        v[length(v)+1]<-j
      }
    }
  }
  boundaries_coords = boundaries_coords[-v,]
  return (boundaries_coords)
}

