
#' Returns a normally distributed vector with mean and SD , with min and max
#' @param .data (required) the source data
#' @param mu (required) the mean
#' @param sd (required) standard deviation
#' @param  round_to_digits (optional) default is 0.
#' @param min (optional) min cut of
#' @param maximum (optional) max vut of
#' @param colname (optional) the col name for vector added to .data. Recommeded to set else will set at num1
#' @return appended dataframe with a numeric vector added as the last col
#' @examples
#'  gs_norm(data , colname = "age" , mu = 45 , sd = 20 , min = 18 , max =65 )

gs_norm <- function(.data, colname, mu, sd, round_to_digits = 0,
                    min = NULL, max = NULL) {
  ds_created <- FALSE
  i <- 0
  x <- NULL

  while (!ds_created) { # loop till max / min conditions are satisfied
    i <- i + 1
    amplify <- (1 + (.3 * i)) # create 1.3 times more rows as we will discard < min and >max later on
    x <- norm_helper(.data, amplify_by = amplify, mu, sd, round_to_digits = 0)
    if (!is.null(min)) {
      x <- subset(x, x >= min)
    }

    if (!is.null(max)) {
      x <- subset(x, x <= max)
    }

    ifelse(length(x) < nrow(.data),
           ds_created <- FALSE,
           {
             x <- sample(x, nrow(.data), replace = FALSE)
             ds_created <- TRUE
           }
    )

    stopifnot(i < 100)
  }
  .data <- rename_last_col(.data, colname, x)
  .data
}

#' @title Wrapper for faux rnorm_pre
#' @param .data (required) the source data
#' @param corr_col	the  correlated column name
#' @param mu desired mean of returned vector
#' @param sd desired SD of returned vector
#' @param r desired correlation between existing and returned observations
#' @param empirical logical. If true, mu, sd and r specify the empirical not population mean, sd and covariance

gs_norm_corr <- function(.data, colname, corr_col, mu = 0, sd = 1, r = 0,
                         empirical = FALSE) {
  src <- as.numeric(.data[[corr_col]])

  x <- faux::rnorm_pre(x = src, mu = mu, sd = sd, r = r, empirical = empirical)
  .data <- rename_last_col(.data, colname, x)
  .data
}



#' Splits a set of factors as per the splits provided
#'
#' Proving fcts = c("M" , "F") with splits = c(.3,.7) will randomly allocate 30% rows as Males and 70% as femails
#' @param .data (required) the source data
#' @param  colname (required) the column name for this created col
#' @param  fcts (required) the factors that need to be expanded into rows based on splits
#' @param  splits (required) the % splits for factors. should add to 1.0
#' @return the appended dataframe with a numeric vector added as the last col
#' @examples
#' gs_fct(.data = mtcars , colname = "sex", fcts = c("M", "F"), splits = c(.3, .7))
gs_fct <- function(.data, colname, fcts, splits) {
  stopifnot(sum(as.numeric(splits)) == 1)
  x <- expand_fct_with_splits(
    source_fct = fcts,
    splits = splits,
    n = nrow(.data)
  )

  .data <- rename_last_col(.data, colname, x)
  .data
}

gs_id_randomid <- function(.data, colname, bytes = 16) {
  n <- nrow(.data)
  x <- ids::random_id(n, bytes)
  .data <- rename_last_col(.data, colname, x)
  .data
}

gs_id_uuid <- function(.data, colname, bytes = 16) {
  n <- nrow(.data)
  x <- ids::uuid(n, bytes)
  .data <- rename_last_col(.data, colname, x)
  .data
}

#' @title Creates sequencial ids with starting point at start and n = nrows(.data)
#' @param .data (required) the source data
#' @param  colname(required) the column name for this created col
#' @param  start (optional) default = 1
#' @return appended dataframe with a numeric vector added as the last col
#' @examples
#' gs_id_seq(data, fcts = c("M", "F"), splits = c(.5, .5), colname = "sex")
#' @export
gs_id_seq <- function(.data, colname, start = 1) {
  n <- nrow(.data)
  x <- seq(from = start, length.out = nrow(.data))
  .data <- rename_last_col(.data, colname, x)
  .data
}


#' Creates names
#' @param .data (required) the source data
#' @param  colname(required) the column name for this created col
#' @return appended dataframe with a char  vector added as the last col
#' @export

gs_chr_names <- function(.data, colname) {
  n <- nrow(.data)
  x <- charlatan::ch_name(n)
  .data <- rename_last_col(.data, colname, x)
  .data
}
