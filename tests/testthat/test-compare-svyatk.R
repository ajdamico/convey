# compares with vardpoor output

# load libraries
library(laeken)
library(survey)


# function from the IC2 library 1.0-1
# removed from CRAN but available at
# https://cran.r-project.org/src/contrib/Archive/IC2/
calcAtkinson <- function(x, w = NULL, epsilon = 1)
{
  if (epsilon < 0)
    return(NULL)
  if (!is.numeric(x))
    return(NULL)
  xNA <- sum(as.numeric(is.na(x)))
  weighted <- FALSE
  wNA <- NULL
  if (is.null(w))
    w <- rep(1, length(x))
  else
  {
    if (!is.numeric(w))
      return(NULL)
    weighted <- TRUE
    wNA <- sum(as.numeric(is.na(w)))
  }
  df <- cbind("x" = x, "w" = w)
  df <- df[complete.cases(df), , drop = FALSE]
  if (nrow(df) == 0)
    return (NULL)
  if (any(df[, "x"] < 0) || sum(df[, "x"]) == 0)
    return(NULL)
  if (any(df[, "x"] == 0) && epsilon == 1)
    return(NULL)
  index <- 0
  names(index) <- "Atk"
  names(epsilon) <- "epsilon"
  Atk <- list(
    ineq =   list(index = index,
                  parameter = epsilon),
    nas =    list(
      xNA = xNA,
      wNA = wNA,
      totalNA = length(x) - nrow(df)
    )
  )
  class(Atk) <- "ICI"
  if (nrow(df) == 1)
    return(Atk)
  if (epsilon == 1)
  {
    w <- df[, "w"] / sum(df[, "w"])
    xMean <- weighted.mean(df[, "x"], w)
    index <- 1 - (prod(exp(w * log(df[, "x"]))) / xMean)
  }
  else
  {
    xMean <- weighted.mean(df[, "x"], df[, "w"])
    x1 <- df[, "x"] / xMean
    w <- df[, "w"] / sum(df[, "w"])
    param <- 1 - epsilon
    index <- 1 - (weighted.mean((x1 ^ param), w) ^ (1 / param))
  }
  names(index) <- "Atk"
  Atk[["ineq"]][["index"]] <- index
  return(Atk)
}






# test context
context("svyatk comparison with IC2")

# collect and format data
data(eusilc)
names(eusilc) <- tolower(names(eusilc))

### convey calculations

# build survey design objects
des_eusilc <-
  svydesign(
    ids = ~ rb030 ,
    strata =  ~ db040 ,
    weights = ~ rb050 ,
    data = eusilc
  )
des_eusilc_rep <- as.svrepdesign(des_eusilc , type = "bootstrap")

# prepare survey design objects for convey
des_eusilc <- convey_prep(des_eusilc)
des_eusilc_rep <- convey_prep(des_eusilc_rep)

# filter positive incomes
des_eusilc <- subset(des_eusilc , eqincome > 0)
des_eusilc_rep <- subset(des_eusilc_rep , eqincome > 0)

# calculate estimates using convey
fun_atkw <- svyatk(~ eqincome , des_eusilc)
fun_atkw_rep <- svyatk(~ eqincome , des_eusilc_rep)

# collect point estimates from convey object
convest <- coef(fun_atkw)
attributes(convest) <- NULL

# collect SE estimates from convey object
convse <- SE(fun_atkw)
attributes(convse) <- NULL

### IC2 calculations

# compute point estimates
IC2est  <-
  calcAtkinson(x = eusilc$eqincome[eusilc$eqincome > 0], w = eusilc$rb050[eusilc$eqincome > 0])$ineq$index[[1]]

# perform tests
test_that("compare results convey vs vardpoor", {
  # compare point estimates
  expect_equal(IC2est[[1]] , convest)

  # # compare point estimates on domains
  # expect_equal( vardestd , convestd*100 )
  #
  # # compare SE estimates
  # expect_equal( varse , convse*100 )
  # expect_equal( varsed , convsed*100 )

})
