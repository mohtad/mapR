#'
#' Create Bins from a vector of numbers.
#'
#' @description Create Bins from a vector of numbers.
#'
#' @param x Vector of number.
#' @param nbins Number of bins to be generated. If 'bins' parameter is provided value, nbins is not used.
#' @param bins List containing the breaks intervals and the bins' labels. If 'bins' parameter is not provided value, nbins is used and bins are automatically generated. Bins contain two vectors: breaks and labels.
#'
#' @return vector of bins as string associated to x numbers.
#'
#' @examples
#' #auto generate the bins
#' data<-data.frame(x =floor(exp(rnorm(200000 * 1.3))))
#' data$bin <- create_bins(data$x, nbins=6)
#'
#' #pre-define the bins
#' data<-data.frame(x =floor(exp(rnorm(200000 * 1.3))))
#' data$bin <- create_bins(data$x, nbins=NULL, bins=list(breaks=c(-1,0,1,35,61,92,130), labels=c("0","1","2 to 35","36 to 61","62 to 92","93 to 130")))
#'
#' @export
#'
#' @importFrom binr bins bins.getvals
#'
create_bins<-function(x, nbins, bins = NULL){
  if(!is.null(bins)){
    breaks<-bins$breaks
    labels<-bins$labels
  }
  else
    {
  cuts<-bins(x,target.bins =nbins, minpts = 1)
  cuts$breaks <- bins.getvals(cuts)
  breaks<-numeric(nbins+1)
  labels<-character(nbins)
  for(i in 1:nbins){

    low = attr(cuts$breaks, "binlo")[i]
    hi=attr(cuts$breaks, "binhi")[i]
    if(low==hi){
      labels[i]<-as.character(low)
    }
    else{
      labels[i]<-paste(c(low, hi), collapse = " to ")
    }
    if(i==1){
      breaks[1]<-low-1

      breaks[2]<-hi
    }else{
      breaks[i+1]<-hi
    }
  }
  }
  return(cut(x, breaks=breaks, labels=labels))
}

