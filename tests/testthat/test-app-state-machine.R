test_that("every state is known and every rule points to a known state", {
  expect_true(all(infoStates %in% names(infoStateRules)))

  for (state in names(infoStateRules)) {
    sources <- infoStateRules[[state]]
    expect_true(all(sources %in% infoStates), info = state)
  }
})

test_that("a state may be set again", {
  for (state in infoStates) {
    expect_true(isAllowedTransition(state, state), info = state)
  }
})

test_that("the regular flow is allowed", {
  expect_true(isAllowedTransition("initial", "dir_set"))
  expect_true(isAllowedTransition("dir_set", "loaded"))
  expect_true(isAllowedTransition("loaded", "processed"))
  expect_true(isAllowedTransition("loaded", "fitErrorRecal"))
  expect_true(isAllowedTransition("processed", "dir_set"))
})

test_that("jumping is reported", {
  expect_false(isAllowedTransition("initial", "processed"))
  expect_false(isAllowedTransition("processed", "loaded"))
  expect_false(isAllowedTransition("dir_set", "fitErrorRecal"))
})

test_that("unknown states are rejected", {
  expect_false(isInfoState("nonsense"))
  expect_false(isAllowedTransition("dir_set", "nonsense"))

  appData <- new.env()
  appData$info_state <- "dir_set"
  expect_error(setInfoState(appData, "nonsense"), "unknown state")
})

test_that("setInfoState stores the state and warns on unexpected changes", {
  # the app uses a reactiveValues object, which also passes by reference
  appData <- new.env()
  appData$info_state <- "dir_set"

  expect_silent(setInfoState(appData, "loaded"))
  expect_equal(appData$info_state, "loaded")

  appData$info_state <- "processed"
  expect_warning(setInfoState(appData, "loaded"), "unexpected state change")
  # the state is set anyway, so the app does not get stuck
  expect_equal(appData$info_state, "loaded")
})
