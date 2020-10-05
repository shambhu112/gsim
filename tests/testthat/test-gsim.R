context("Testign gsim ")

# Configure this test to fit your need
test_that(
  "gsim",{
    # Test - new dataset
    df <- gsim(target_rows = 100)
    testthat::expect_equal(nrow(df),100)


    # based on mtcars - Expand Dataset
    df <- gsim(data = mtcars , target_rows = 1000)
    testthat::expect_equal(nrow(df),nrow(mtcars))


    # based on mtcars - reduce dataset
    df <- gsim(data = mtcars , target_rows = 7)
    testthat::expect_equal(nrow(df),7)

    # create job titles
    df<- gsim(target_rows = 100) %>%
            sim_txt_jobs(n =100)


  }
)
