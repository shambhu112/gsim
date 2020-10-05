msg.debug <- TRUE
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
