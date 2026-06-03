test_that("emptyAppDataObject returns a reactive values object", {
  appData <- emptyAppDataObject()
  expect_s3_class(appData, "reactivevalues")
})
