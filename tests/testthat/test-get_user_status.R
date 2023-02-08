
## require(httptest)
## start_capturing(simplify = FALSE)
## user_ids <- c("100001851", "1000911883183050754", "1001986308343033856", "1020397482793107458", "1000853231122214912")
## get_user_status(user_ids)
## stop_capturing()

with_mock_api({
  test_that("expected behavior", {
    skip_if(!dir.exists("api.twitter.com"))
    user_ids <- c("100001851", "1000911883183050754", "1001986308343033856", "1020397482793107458", "1000853231122214912")
    expect_true(is.data.frame(get_user_status(user_ids)))
    expect_error(get_user_status(user_ids), NA)
    expect_true(length(get_user_status(user_ids)) == 5)
    expect_true(ncol(get_user_status(user_ids)) == 5)
  })
})
