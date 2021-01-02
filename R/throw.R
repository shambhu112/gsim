#' @import charlatan

throw_jobs <- function(locale =  "en_US" , n = 10){
  jobs <- charlatan::ch_job(locale = locale, n = n)
  as.list(jobs)
}

#' @import charlatan
throw_states <- function(source , splits , n ){

}

#' @import charlatan
throw_names <- function(source ,  n ){
  names <- charlatan::ch_name(n)
}

