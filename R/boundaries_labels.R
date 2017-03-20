#' us_zcta_names within 25miles radius of the center of the map. remove close zipcodes.
#'
#' @return
#'
#' @examples
us_zcta_names<-function(US_ZCTA){
  #US_ZCTA.25=US_ZCTA[US_ZCTA$dist_to_center<25,]
  n<-nrow(US_ZCTA)
  v<-numeric(n)
  for(i in 1:(n-1)) {
    m = i+1
    for(j in m:n){
      dist<-distm (c(US_ZCTA[i,]$long, US_ZCTA[i,]$lat), c(US_ZCTA[j,]$long,US_ZCTA[j,]$lat), fun = distHaversine)*0.000621371;
      if(is.na(dist)){
        print(paste(i,'-',j,'-',n))
      }
      if(dist<=1.5){
        v[length(v)+1]<-j
      }
    }
  }
  US_ZCTA = US_ZCTA[-v,]
  return (US_ZCTA)
}
