test_that("app state round trips through the share link", {
  state <- list(
    settings = list(SNR = 3, normMz = 208.113985, normMeth = "TIC", smooth = TRUE),
    dir = "C:/data/experiment",
    features = c(1L, 5L)
  )

  encoded <- encodeAppState(state)
  expect_false(grepl("[[:space:]]", encoded))

  query <- paste0("?state=", utils::URLencode(encoded, reserved = TRUE))
  decoded <- appDefaultsFromUrl(list(SNR = 1, normMz = 1, normMeth = "TIC"),
                                query)

  expect_true(decoded$active)
  expect_equal(decoded$defaults$SNR, 3)
  expect_equal(decoded$defaults$normMz, 208.113985)
  expect_equal(decoded$defaults$smooth, TRUE)
  expect_equal(decoded$defaults$dir, "C:/data/experiment")
  expect_equal(decoded$features, c(1L, 5L))
})

test_that("invalid share links are ignored", {
  defaults <- list(SNR = 7)
  for (query in c("", "?foo=bar", "?state=not-base64", "?state=W10=")) {
    decoded <- appDefaultsFromUrl(defaults, query)
    expect_false(decoded$active)
    expect_equal(decoded$defaults$SNR, 7)
    expect_length(decoded$features, 0)
  }
})
