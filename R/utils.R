msg.debug <- TRUE

#' @import cli
msg <- function(...){
  cli::cli_alert(...)
}

msg_d <- function(...){
  if(msg.debug)
    cli::cli_alert(...)
}


msg_success <- function(...){
  cli::cli_alert_success(...)
}


msg_warning <- function(...){
  cli::cli_alert_warning(...)
}


msg_error <- function(...){
  cli::cli_alert_danger(...)
}


norm_helper <- function(.data , amplify_by = 1.3 , mu , sd , round_to_digits){
  x <-rnorm(nrow(.data) * amplify_by , mean = mu , sd = sd)
  x <- round(x ,digits = round_to_digits)
  x
}

rename_last_col <- function(.data , newname , x){
  .data <-  .data %>% dplyr::mutate(!! newname := NA)
  .data[,ncol(.data)] <- x
  return(.data)
}



check <- function(condition , ...){
  if(!condition){
    msg_error("Condition Failed {condition} " )
    stop("")
  }
}
