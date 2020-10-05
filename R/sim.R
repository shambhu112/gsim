
sim_numeric_normal <- function(.data , distribution = "normal" ,
                        cols_based_on = NULL,
                        mu ,
                        sd
                        ){
#TODO : implement
                    }

#
# 1. Creates n unique Job Titles according to country and language i.e locale
# 2. attaches these n unique job titles to all rows in the dataset as per the split percentages
# Another approach to create jobs is to use throw_fct_jobs

sim_fct <- function(.data , fcts = list("M" , "F") , splits = list(.5,.5) , colname = "fct1"){
  thefct <- expand_fct_with_splits(source_fct = fcts ,
                         splits = splits ,
                         n = nrow(.data))

  .data$.thefct <- thefct
  names(.data)[names(.data) == ".thefct"] <- colname
  return(.data)
}

