context("basic functionality")
test_that("functions return valid structures", {

  amazon <- amazon_ranges()
  expect_equal(length(amazon), 4)
  expect_is(normalize_ipv4(amazon), "character")

  azure <- azure_ranges()
  expect_equal(length(azure), 2)
  expect_is(normalize_ipv4(azure), "character")

  digitalocean <- digitalocean_ranges()
  expect_true(length(digitalocean)>90)
  expect_is(normalize_ipv4(digitalocean), "character")

  google <- google_ranges()
  expect_equal(length(google), 2)
  expect_is(normalize_ipv4(google), "character")

  rackspace <- rackspace_ranges()
  expect_true(length(rackspace)>200)
  expect_is(normalize_ipv4(rackspace), "character")

  softlayer <- softlayer_ranges()
  expect_true(length(softlayer)>100)
  expect_is(normalize_ipv4(softlayer), "character")

})
