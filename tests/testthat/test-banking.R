
context("Testing gsim with  Banking Case ")

# Configure this test to fit your need

test_that(
  "gsim-bank",{
    # Create new datset

    states <- list("New York", "Connecticut" , "California" , "Maine" , "Alabama" , "Texas")
    proportions <- list(.32 ,.05 , .15 , .03 ,  .07 ,  .38)

    jobs <- dplyr::tribble(
      ~job, ~proportion,
      "admin",   1,
      "b",   2,
      "c",   3
    )

    #  jobs <- c('admin','blue-collar','entrepreneur','housemaid','management','retired','self-employed','services','student','technician','unemployed','unknown')
    #  job_pro <- (.15 , .20 , .1 , .05 ,.02 , .20 , .3 , .02 , )

    bank <- gsim(target_rows = 5000) %>%
      gs_id_seq(colname = "id" , start = 100000) %>%
      gs_chr_names(colname = "full_name") %>%
      gs_fct(fcts = states , splits = proportions , colname = "states") %>% #states
      #   gs_fct(fct = jobs , splits = list(.1,.2,.3,.25,.15) , colname = "jobs") %>% # job
      gs_norm(mu = 45 , sd = 20 , colname = "age" , min = 18 , max =73) %>%  #age
      gs_norm_corr(corr_col = "age" ,mu = 44000 , sd = 20000 , r = .7 , colname = "income" )

    testthat::expect_equal(nrow(bank) , 5000)
    testthat::expect_lt(max(bank$age) , 73 + 1)


  }
)
