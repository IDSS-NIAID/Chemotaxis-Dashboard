test_that("process_uploaded_data works with built-in data", {
  # Create a data frame mimicking the structure of uploaded files
  file_names <- c(
    "19000101_CH1_nl_Buffer.csv",
    "19000101_CH2_nl_fMLF8.csv"
  )

  uploaded_files <- data.frame(
    name = file_names,
    datapath = sapply(file_names, function(x) system.file("extdata", x, package = "ChemotaxisDashboard"))
  )

  # Process the data
  processed_data <- process_uploaded_data(uploaded_files, ledge_upper = 0, ledge_lower = 1)

  # Add assertions to check the output
  expect_true(is.list(processed_data))
  expect_equal(names(processed_data), c("chanSummary", "trackRaw"))
  expect_s3_class(processed_data$chanSummary, "data.frame")
  expect_s3_class(processed_data$trackRaw, "data.frame")
})
