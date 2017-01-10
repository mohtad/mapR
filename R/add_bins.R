#' add Bins
#'
#' @return
#' @export
#'
#' @examples
add_bins<-function(data, metric, nbins=6, br=NULL, lab=NULL){
  if(!is.null(br) && !is.null(lab)){
    breakss<-br
    labelss<-lab
  }
  else{
  cuts<-bins(data[metric][,1],target.bins =nbins, minpts = 1)
  cuts$breaks <- bins.getvals(cuts)
  breakss<-numeric(nbins+1)
  labelss<-character(nbins)
  for(i in 1:nbins){

    low = attr(cuts$breaks, "binlo")[i]
    hi=attr(cuts$breaks, "binhi")[i]
    if(low==hi){
      labelss[i]<-as.character(low)
    }
    else{
      labelss[i]<-paste(c(low, hi), collapse = " to ")
    }
    if(i==1){
      breakss[1]<-low-1

      breakss[2]<-hi
    }else{
      breakss[i+1]<-hi
    }
  }
  }
  return(cut(data[metric][,1], breaks=breakss, labels=labelss))
}
# 2-1426775051
