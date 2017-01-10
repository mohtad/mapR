#' Retrieve a list of ZCTAs with their spacial coordinates and a computed haversine distance in miles from the map center.
#'
#' @return list of US ZCTAs as data.frame
#' @export
#'
#' @examples
load_us_zcta<-function(us_zcta_path,map_center=NULL){
  #US_ZCTA<- read.csv("C:/Users/Mohtadi.Nadjar/heatmapR/Template/zcta_coordinates/2015_Gaz_zcta_national.txt", sep = "", colClasses=c("GEOID"="character"))
  US_ZCTA<- read.csv(us_zcta_path, sep = "", colClasses=c("GEOID"="character"))
  US_ZCTA$GEOID<-as.character(US_ZCTA$GEOID)
  US_ZCTA = data.frame(code=US_ZCTA$GEOID,lat=US_ZCTA$INTPTLAT,long=US_ZCTA$INTPTLONG,stringsAsFactors = FALSE)
  mer = apply(US_ZCTA,1,function(x){
    lat=as.numeric(x['lat']);
    long=as.numeric(x['long']);
    mer<-mercator(c(long,lat))
  })
  US_ZCTA$mer_lat = mer[2,]
  US_ZCTA$mer_long = mer[1,]
  rm(mer)

  if(!is.null(map_center)){
    US_ZCTA$dist_to_center = apply(US_ZCTA,1,function(x){
      lat=as.numeric(x['lat']);
      long=as.numeric(x['long']);
      distm (c(map_center$long, map_center$lat), c(long,lat), fun = distHaversine)*0.000621371;
  })
  }
  return (US_ZCTA)
}
