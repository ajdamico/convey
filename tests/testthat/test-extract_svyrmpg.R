context("rmpg output survey.design and svyrep.design")
library(vardpoor)
data(eusilc)

des_eusilc <- survey:::svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
des_eusilc <- convey_prep(des_eusilc)
des_eusilc_rep <- as.svrepdesign(des_eusilc, type= "bootstrap")
des_eusilc_rep <- convey_prep(des_eusilc_rep)

a1 <- svyrmpg(~eqIncome, design = des_eusilc)
a2 <- survey:::svyby(~eqIncome, by = ~db040, design = des_eusilc, FUN = svyrmpg, deff = FALSE)

b1 <- svyrmpg(~eqIncome, design = des_eusilc_rep)

b2 <- survey:::svyby(~eqIncome, by = ~db040, design = des_eusilc_rep, FUN = svyrmpg,deff = FALSE)


test_that("output svyarpr",{
  expect_is(coef(a1),"numeric")
  expect_is(coef(a2), "numeric")
  expect_is(coef(b1),"numeric")
  expect_is(coef(b2),"numeric")
  expect_equal(coef(a1), coef(b1))
  expect_equal(coef(a2), coef(b2))
  expect_is(SE(a1),"numeric")
  expect_is(SE(a2), "numeric")
  expect_is(SE(b1),"numeric")
  expect_is(SE(b2),"numeric")
  expect_less_than(confint(a1)[1], coef(a1))
  expect_more_than(confint(a1)[2],coef(a1))
  expect_less_than(confint(b1)[,1], coef(b1))
  expect_more_than(confint(b1)[2], coef(b1))
  expect_equal(sum(confint(a2)[,1]<= coef(a2)),length(coef(a2)))
  expect_equal(sum(confint(a2)[,2]>= coef(a2)),length(coef(a2)))
  expect_equal(sum(confint(b2)[,1]<= coef(b2)),length(coef(b2)))
  expect_equal(sum(confint(b2)[,2]>= coef(b2)),length(coef(b2)))
})
