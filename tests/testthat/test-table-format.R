test_that("the table shows readable numbers", {
  stats <- data.frame(
    mz = c(208.11398512, 500.1234567),
    pEC50 = c(1.234567, 2.345678),
    R2 = c(0.987654321, 0.5),
    FZ = c(0.6666666, 0.1),
    FV = c(1.23456, 0.999),
    SNR = c(3.14159, 12.987),
    log2FC = c(2.34567, -1.23456),
    CRS = c(99.87654, 12.34567),
    check.names = FALSE
  )

  shown <- displayStats(stats)

  # m/z keeps its decimals, SNR only one
  expect_equal(shown$mz, c(208.1140, 500.1235))
  expect_equal(shown$SNR, c(3.1, 13.0))
  expect_equal(shown$pEC50, c(1.23, 2.35))
  expect_equal(shown$R2, c(0.988, 0.500))
  expect_equal(shown$FZ, c(0.67, 0.10))
  expect_equal(shown$FV, c(1.23, 1.00))
  expect_equal(shown$log2FC, c(2.35, -1.23))
  expect_equal(shown$CRS, c(99.88, 12.35))

  # the column names stay exactly as they are
  expect_equal(names(shown), names(stats))

  # the values behind the table are untouched
  expect_equal(stats$mz[1], 208.11398512)

  # nothing happens for an empty table
  expect_equal(nrow(displayStats(data.frame())), 0L)
})
