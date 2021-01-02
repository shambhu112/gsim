

#' Create a **gsim** table object
#'

gsim <- function(data = NULL , target_rows = NULL ){

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


expand_dataset <- function(.data ,  target_rows ){
  # TODO : write the expand function here
  msg_warning(" Expanding dataset feature not implemented. target_rows will be same as source data = {nrow(data)}")
  .data
}

reduce_dataset <- function(.data ,  target_rows){
  d <- .data[sample(nrow(.data) , target_rows ),]
  return(d)
}
