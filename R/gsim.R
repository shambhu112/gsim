

#' Create an initial data.frame for simulation
#'
#' The gsim() function creates a data.frame with target_rows based on an existing dataset
#' or a net_new solution without an existing dataset
#' @param data (optional) base dataset. default is NULL in which case a new data.frame will be created.
#' @param target_rows (optional) . default is set to 1000 rows
#' @param seed (optional) default is 12345. Initial seed
#' @return If data is NULL then a new data.frame is created with sequential index. If data is smaller than target_rows,
#'        the dataset is expanded with \code{expand_dataset} behaviour. If data is larger than random subset is choosen
#'        to match target_rows
#' @examples
#' gsim(data = mtcars , target_rows = 20)
#' gsim(target_rows = 2000)
#' @import  dplyr

gsim <- function(data = NULL , target_rows = 1000 , seed = 12345){
  set.seed(seed)
  if(is.null(data)){
    if(is.null(target_rows)) # Default to 1000 rows
      target_rows <- 1000
    data <- data.frame(index = seq(1:target_rows))
  }
  else{
    #if we have a seed dataset then check if we need to expand it or not.
    #If we want to use as it is then target_rows should be null
    if(is.null(target_rows))
      target_rows <- nrow(data)
    rows <- nrow(data)

    if(target_rows > rows)
      data <- expand_dataset(data ,target_rows )


    if(target_rows < rows)
      data <- reduce_dataset(data ,target_rows )

  }
  return(data)
}

#' This functionality is not yet implemented
#' @noRd
expand_dataset <- function(.data ,  target_rows ){
  # TODO : write the expand function here
  msg_warning(" Expanding dataset feature not implemented. target_rows will be same as source data = {nrow(data)}")
  .data
}

#' reduces size of dataset provided by random subsetting
#' @param .data input data
#' @param target_rows the reduced nrow(data) needed
reduce_dataset <- function(.data ,  target_rows){
  d <- .data[sample(nrow(.data) , target_rows ),]
  return(d)
}
