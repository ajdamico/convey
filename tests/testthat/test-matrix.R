context("test-matrix makes sense across all functions' coefficients and standard errors")


test_that("coef and SE matrix values make sense",{
  skip_on_cran()

  library(laeken)
  data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
  library(survey)



  des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
  des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )

  des_eusilc <- subset( convey_prep(des_eusilc) , eqincome > 0 )
  des_eusilc_rep <- subset( convey_prep(des_eusilc_rep) , eqincome > 0 )

  out <- NULL

  for( this_fun in c( "svygei" , "svygpg" , "svyatk" , "svyamato" , "svyqsr" , "svypoormed" , "svyjdiv" , "svyzenga" , "svyrmpg" , "svyrmir" , "svyisq" , "svyiqalpha" , "svyarpr" , "svyarpt" , "svyfgt" , "svygini" ) ){

    final_fun <- FUN <- get( this_fun )

    if( identical( FUN , svyrmpg ) ) final_fun <- function( ... ) FUN( ... , thresh = TRUE )
    if( identical( FUN , svyrmir ) ) final_fun <- function( ... ) FUN( ... , age = ~ age , med_old = TRUE )
    if( identical( FUN , svyisq ) ) final_fun <- function( ... ) FUN( ... , alpha = 0.2 )
    if( identical( FUN , svyiqalpha ) ) final_fun <- function( ... ) FUN( ... , alpha = 0.5 )
    if( identical( FUN , svyfgt ) ) final_fun <- function( ... ) FUN( ... , g = 0 , abs_thresh = 10000 )
    if( identical( FUN , svygpg ) ) final_fun <- function( ... ) FUN( ... , sex = ~ rb090 )
    if( identical( FUN , svygei ) ) final_fun <- function( ... ) FUN( ... , epsilon = 0.5 )


    this_df <-
      data.frame(
        function_name = this_fun ,
        coef_lin = coef( final_fun( ~ eqincome , des_eusilc ) ) ,
        se_lin = SE( final_fun( ~ eqincome , des_eusilc ) )[1] ,
        coef_rep = coef( final_fun( ~ eqincome , des_eusilc_rep ) ) ,
        se_rep = SE( final_fun( ~ eqincome , des_eusilc_rep ) )[1] ,
        measure_of_uncertainty = "standard error"
      )

    rownames( this_df ) <- NULL

    out <- rbind( out , this_df )

  }

  zc_lin <- svyzengacurve( ~eqincome , des_eusilc,  alpha = .01 , plot = FALSE )
  zc_rep <- svyzengacurve( ~eqincome , des_eusilc_rep,  alpha = .01 , plot = FALSE )

  this_df <-
    data.frame(
      function_name = "svyzengacurve" ,
      coef_lin = zc_lin$quantiles[5] ,
      se_lin = zc_lin$quantiles[5] - zc_lin$CIs[9] ,
      coef_rep = zc_rep$quantiles[5] ,
      se_rep = zc_rep$quantiles[5] - zc_rep$CIs[9] ,
      measure_of_uncertainty = "confidence interval length at median"
    )

  rownames( this_df ) <- NULL

  out <- rbind( out , this_df )

  lor_lin <- svylorenz( ~eqincome , des_eusilc, seq(0,1,.05), alpha = .01 , plot = FALSE )
  lor_rep <- svylorenz( ~eqincome , des_eusilc_rep, seq(0,1,.05), alpha = .01 , plot = FALSE )

  this_df <-
    data.frame(
      function_name = "svylorenz" ,
      coef_lin = lor_lin$quantiles[5] ,
      se_lin = lor_lin$quantiles[5] - lor_lin$CIs[9] ,
      coef_rep = lor_rep$quantiles[5] ,
      se_rep = lor_rep$quantiles[5] - lor_rep$CIs[9] ,
      measure_of_uncertainty = "confidence interval length at median"
    )

  rownames( this_df ) <- NULL

  out <- rbind( out , this_df )

  div_lin <- svyjdivdec( ~eqincome , ~rb090 , subset(des_eusilc, eqincome > 0) )
  div_rep <- svyjdivdec( ~eqincome , ~rb090 , subset(des_eusilc_rep, eqincome > 0) )

  for( i in 1:3 ){

    this_df <-
      data.frame(
        function_name = paste( "svyjdivdec" , c( "total" , "within" , "between" )[ i ] ) ,
        coef_lin = div_lin$eqincome[i] ,
        se_lin = attr(div_lin,'var')[i] ,
        coef_rep = div_rep$eqincome[i] ,
        se_rep = attr(div_rep,'var')[i] ,
        measure_of_uncertainty = "variance"
      )

    rownames( this_df ) <- NULL

    out <- rbind( out , this_df )

  }

  dec_lin <- svygeidec( ~eqincome , ~rb090 , des_eusilc , epsilon = .5 )
  dec_rep <- svygeidec( ~eqincome , ~rb090 , des_eusilc_rep , epsilon = .5 )

  for( i in 1:3 ){

    this_df <-
      data.frame(
        function_name = paste( "svygeidec" , c( "total" , "within" , "between" )[ i ] ) ,
        coef_lin = dec_lin$eqincome[i] ,
        se_lin = attr(dec_lin,'var')[i] ,
        coef_rep = dec_rep$eqincome[i] ,
        se_rep = attr(dec_rep,'var')[i] ,
        measure_of_uncertainty = "variance"
      )

    rownames( this_df ) <- NULL

    out <- rbind( out , this_df )

  }


  out <- out[ c( "function_name" , "measure_of_uncertainty" , "coef_lin" , "se_lin" , "coef_rep" , "se_rep" ) ]

  names( out ) <- c( "function_name" , "measure_of_uncertainty__mou" , "linearized_coefficient" , "linearized_mou" , "replication_coefficient" , "replication_mou" )

  # matrix of coefficients and standard errors for all convey functions
  print( out )

  # test that coefficients for all functions are equal!
  isTRUE( expect_true( all.equal( out$linearized_coefficient , out$replication_coefficient ) ) )

  # test that the difference between the measure of uncertainty is a small fraction of replication designs
  expect_true( all( abs( out$replication_mou - out$linearized_mou ) / out$replication_coefficient < 0.05 ) )

  # test that the difference between the measure of uncertainty is a small fraction of linearized designs
  expect_true( all( abs( out$replication_mou - out$linearized_mou ) / out$linearized_coefficient < 0.05 ) )

}
)

