#' A small function which return a ggplot instance but without background
#'
#' @param plot a ggplot instance
#' @keywords lioche rcpack ggplot background
#' @export

remove_ggplot_background = function(plot){
  p$theme = list(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  return (p)
}

#'Return a layout of (nrow, ncol) plots composed of histograms with corresponding boxplot below, for each variable.
#'
#'@param data a dataframe containing the data
#'@param nrow the number of rows, default is 2
#'@param ncol the number of colums, default is 2
#'@param boxframe does the boxplot have a border, default is true
#'@param color the color of the bar in the histogram, default is black
#'
#'@keywords lioche rcpack histogram boxplot draw
#'@export

draw_hist_boxplots = function(data, nrow=2, ncol=2, boxframe=T, color=rgb(0,0,0)){


  layout(mat=matrix(seq(nrow*ncol*2), 2*nrow, 1*ncol, byrow=F), heights = rep(c(4,1.5), ncol))
  drawPlots = function(coname){

    par(mar=c(0, 2, 2, 2))
    hist(data[[coname]], axes=T, main=coname, col = color, xlab="", ylab="Count", xaxt="n")
    par(mar=c(3, 2.3, 0, 2.3))
    boxplot(data[[coname]], horizontal = T, frame=boxframe)

  }

  invisible(sapply(colnames(data), function (x) drawPlots(x)))

}
