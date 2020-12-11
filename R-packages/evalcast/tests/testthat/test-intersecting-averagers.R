test_that("intersecting averagers works", {
  
  tib <- tibble(fct = rep(letters[1:2], each=5), val = c(1:5,1:5))
  tib2 <- tib
  tib2$val[10] <- 6
  tib2 <- tib2[-3,]
  ltib <- bind_rows(tib, tib2, .id = "grp")
  
  expect_message(ia <- intersect_averagers(ltib, "grp", c("fct","val")))
  expect_equal(ia[0,], ltib[0,])
  expect_equal(nrow(ia), 16L)
  
  ltib <- bind_cols(ltib, ugh = 1:nrow(ltib))
  expect_error(intersect_averagers(ltib, "grp", c("fct","val","ugh")))
  expect_error(intersect_averagers(ltib, c("grp","fct"), c("val","ugh")))
  expect_error(intersect_averagers(ltib, "grp", c("grp","val","ugh")))
})
