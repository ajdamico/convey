context("test-matrix makes sense across all functions' coefficients and standard errors")


test_that("coef and SE matrix values make sense", {
  skip_on_cran()

  library(laeken)
  data(eusilc)
  names(eusilc) <- tolower(names(eusilc))
  library(survey)



  des_eusilc <-
    svydesign(
      ids = ~ rb030,
      strata =  ~ db040,
      weights = ~ rb050,
      data = eusilc
    )
  des_eusilc_rep <-
    as.svrepdesign(des_eusilc , type = "bootstrap")

  des_eusilc <- subset(convey_prep(des_eusilc) , eqincome > 0)
  des_eusilc_rep <-
    subset(convey_prep(des_eusilc_rep) , eqincome > 0)

  out <- NULL

  for (this_fun in c(
  "svyrich" , "svywatts" , "svywattsdec" , "svyfgtdec" ,
    "svygei" ,
    "svygpg" ,
    "svyatk" ,
    "svyqsr" ,
    "svypoormed" ,
    "svyjdiv" ,
    "svyrmpg" ,
    "svyrmir" ,
    "svyisq" ,
    "svyiqalpha" ,
    "svyarpr" ,
    "svyarpt" ,
    "svyfgt" ,
    "svygini" ,
    "svyzenga"
  )) {
    final_fun <- FUN <- get(this_fun)

	 if( identical( FUN , svyrich ) ) final_fun <- function( ... ) FUN( ... , type_measure = "FGTT1" , g = 1 , abs_thresh = 10000 )
    if( identical( FUN , svywatts ) ) final_fun <- function( ... ) FUN( ... , abs_thresh = 10000 )
    if( identical( FUN , svywattsdec ) ) final_fun <- function( ... ) FUN( ... , abs_thresh = 10000 )
      if( identical( FUN , svyfgtdec ) ) final_fun <- function( ... ) FUN( ... , g = 2 , abs_thresh = 10000 )
   
    if (identical(FUN , svyrmpg))
      final_fun <- function(...)
        FUN(... , thresh = TRUE)
    if (identical(FUN , svyrmir))
      final_fun <-
        function(...)
          FUN(... , age = ~ age , med_old = TRUE)
    if (identical(FUN , svyisq))
      final_fun <- function(...)
        FUN(... , alpha = 0.2)
    if (identical(FUN , svyiqalpha))
      final_fun <- function(...)
        FUN(... , alpha = 0.5)
    if (identical(FUN , svyfgt))
      final_fun <-
        function(...)
          FUN(... , g = 0 , abs_thresh = 10000)
    if (identical(FUN , svygpg))
      final_fun <- function(...)
        FUN(... , sex = ~ rb090)
    if (identical(FUN , svygei))
      final_fun <- function(...)
        FUN(... , epsilon = 0.5)


    this_df <-
      data.frame(
        function_name = this_fun ,
        coef_lin = coef(final_fun( ~ eqincome , des_eusilc)) ,
        se_lin = SE(final_fun( ~ eqincome , des_eusilc))[1] ,
        coef_rep = coef(final_fun( ~ eqincome , des_eusilc_rep)) ,
        se_rep = SE(final_fun( ~ eqincome , des_eusilc_rep))[1] ,
        measure_of_uncertainty = "standard error"
      )

    rownames(this_df) <- NULL

    out <- rbind(out , this_df)

  }

  lor_lin <-
    svylorenz( ~ eqincome ,
               des_eusilc,
               seq(0, 1, .05),
               alpha = .01 ,
               plot = FALSE)
  lor_rep <-
    svylorenz( ~ eqincome ,
               des_eusilc_rep,
               seq(0, 1, .05),
               alpha = .01 ,
               plot = FALSE)

  this_df <-
    data.frame(
      function_name = "svylorenz" ,
      coef_lin = coef(lor_lin)["L(0.5)"] ,
      se_lin = SE(lor_lin)["L(0.5)"] ,
      coef_rep = coef(lor_rep)["L(0.5)"] ,
      se_rep = SE(lor_rep)["L(0.5)"] ,
      measure_of_uncertainty = "confidence interval length at median"
    )

  rownames(this_df) <- NULL

  out <- rbind(out , this_df)

  div_lin <-
    svyjdivdec( ~ eqincome , ~ rb090 , subset(des_eusilc, eqincome > 0))
  div_rep <-
    svyjdivdec( ~ eqincome , ~ rb090 , subset(des_eusilc_rep, eqincome > 0))

  for (i in 1:3) {
    this_df <-
      data.frame(
        function_name = paste("svyjdivdec" , c("total" , "within" , "between")[i]) ,
        coef_lin = coef(div_lin)[i] ,
        se_lin = attr(div_lin, 'var')[i, i] ,
        coef_rep = coef(div_rep)[i] ,
        se_rep = attr(div_rep, 'var')[i, i] ,
        measure_of_uncertainty = "variance"
      )

    rownames(this_df) <- NULL

    out <- rbind(out , this_df)

  }

  dec_lin <-
    svygeidec( ~ eqincome , ~ rb090 , des_eusilc , epsilon = .5)
  dec_rep <-
    svygeidec( ~ eqincome , ~ rb090 , des_eusilc_rep , epsilon = .5)

  for (i in 1:3) {
    this_df <-
      data.frame(
        function_name = paste("svygeidec" , c("total" , "within" , "between")[i]) ,
        coef_lin = coef(dec_lin)[i] ,
        se_lin = attr(dec_lin, 'var')[i, i] ,
        coef_rep = coef(dec_lin)[i] ,
        se_rep = attr(dec_rep, 'var')[i, i] ,
        measure_of_uncertainty = "variance"
      )

    rownames(this_df) <- NULL

    out <- rbind(out , this_df)

  }


  out <-
    out[c(
      "function_name" ,
      "measure_of_uncertainty" ,
      "coef_lin" ,
      "se_lin" ,
      "coef_rep" ,
      "se_rep"
    )]

  names(out) <-
    c(
      "function_name" ,
      "measure_of_uncertainty__mou" ,
      "linearized_coefficient" ,
      "linearized_mou" ,
      "replication_coefficient" ,
      "replication_mou"
    )

  # matrix of coefficients and standard errors for all convey functions
  print(out)

  # test that coefficients for all functions are equal!
  isTRUE(expect_true(
    all.equal(out$linearized_coefficient , out$replication_coefficient)
  ))

  # test that the difference between the measure of uncertainty is a small fraction of replication designs
  expect_true(all(
    abs(out$replication_mou - out$linearized_mou) / out$replication_coefficient < 0.05
  ))

  # test that the difference between the measure of uncertainty is a small fraction of linearized designs
  expect_true(all(
    abs(out$replication_mou - out$linearized_mou) / out$linearized_coefficient < 0.05
  ))

})
