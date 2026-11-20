test_that("the table shows readable numbers", {
  stats <- data.frame(
    mz = c(208.11398512, 500.1234567),
    pEC50 = c(1.234567, 2.345678),
    R2 = c(0.987654321, 0.5),
    "Z'" = c(0.6666666, 0.1),
    SNR = c(3.14159, 12.987),
    log2FC = c(2.34567, -1.23456),
    check.names = FALSE
  )

  shown <- displayStats(stats)

  # m/z keeps its decimals, SNR only one
  expect_equal(shown$mz, c(208.1140, 500.1235))
  expect_equal(shown$SNR, c(3.1, 13.0))
  expect_equal(shown$pEC50, c(1.23, 2.35))
  expect_equal(shown$R2, c(0.988, 0.500))
  expect_equal(shown[["Z'"]], c(0.67, 0.10))
  expect_equal(shown$log2FC, c(2.35, -1.23))

  # the values behind the table are untouched
  expect_equal(stats$mz[1], 208.11398512)

  # nothing happens for an empty table or unknown columns
  expect_equal(nrow(displayStats(data.frame())), 0L)
})

test_that("the columns of the table have readable names", {
  expect_equal(columnLabel("mz"), "m/z [Da]")
  expect_equal(columnLabel("R2"), "R2 (fit)")
  expect_equal(columnLabel("FZ"), "Z (fit)")
  expect_equal(columnLabel("CRS"), "CRS (0-100)")
  # unknown columns keep their name
  expect_equal(columnLabel("something"), "something")
})

test_that("the column help is a JS array of quoted strings", {
  help <- c("", "m/z of the feature [Da]", 'the "Q" value')
  js <- columnHelpJs(help)

  expect_match(js, '^"", "m/z of the feature \\[Da\\]", ', )
  # quotes inside a text are escaped
  expect_match(js, '\\\\"Q\\\\"')
})

test_that("the help texts are collected per column", {
  stats <- data.frame(mz = 1, pEC50 = 2, unknown = 3, check.names = FALSE)
  help <- tableColumnHelp(stats)

  # one entry per column plus the checkbox column
  expect_length(help, 4L)
  expect_equal(unname(help[1]), "")
  expect_equal(unname(help[2]), "m/z of the feature [Da]")
  expect_equal(unname(help[4]), "")
})
