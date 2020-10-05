context("Testign gsim with full dataset creating in Banking Case ")

# Configure this test to fit your need


test_that(
  "gsim-bank",{
    # Create new datset

    states <- list("New York", "Connecticut" , "California" , "Maine" , "Alabama" , "Texas")
    proportions <- list(.32 ,.05 , .15 , .03 ,  .07 ,  .38)

    jobs <- throw_jobs( n = 5)

    # list(.1,.2,.3,.2,.1)

    ee <- expand_fct_with_splits(source_fct = jobs , splits = list(.1,.2,.3,.2,.2) , n = 5000)

    bank <- gsim(target_rows = 5000) %>%
            sim_fct(fcts = states , splits = proportions , colname = "states") %>% #states
            sim_fct(fct = jobs , splits = list(.1,.2,.3,.25,.15) , colname = "jobs") # job


    expect_equal(length(colnames(bank)) , 3)

  }
)
