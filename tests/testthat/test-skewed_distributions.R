library(survey)
library(convey)


test_that("functions work on weird distributions" ,{
skip_on_cran()

no.na <- function( z , value = FALSE ){ z[ is.na( z ) ] <- value ; z }

all_funs <- c( "svyrmir" , "svyqsr" , "svyarpt" , "svyarpt" , "svyatk" , "svyfgt" , "svygini" , "svygpg" , "svyiqalpha" , "svyisq" , "svyzenga" , "svypoormed" , "svyrenyi" , "svygei" , "svyrmpg" , "svyzengacurve" , "svylorenz" , "svyjdiv" , "svyamato" , "svyafc" , "svybmi" , "svysst" , "svysen" , "svybcc" , "svyrich" , "svychu" , "svywatts" )

for( n in c( 50 , 1000 ) ){

	set.seed( n )

	dist_frame <-
		data.frame(
			wt_binom_two = rbinom( n , 1 , .5 ) ,
			wt_binom = rbinom( n , 1 , .5 ) ,
			wt_unif = runif( n ) ,
			beta_050_050 = rbeta( n , 0.5 , 0.5 ) ,
			beta_500_100 = rbeta( n , 5 , 1 ) ,
			beta_100_300 = rbeta( n , 1 , 3 ) ,
			beta_200_200 = rbeta( n , 2 , 2 ) ,
			beta_200_500 = rbeta( n , 2 , 5 ) ,
			pois1 = rpois( n , 1 ) ,
			pois100 = rpois( n , 100 ) ,
			sex = ifelse( rbinom( n , 1 , 0.5 ) , "male" , "female" ) ,
			age = sample( 0:99 , n , replace = TRUE )
		)

	dist_frame[ -which( names( dist_frame ) == 'sex' ) ] <- sapply( dist_frame[ -which( names( dist_frame ) == 'sex' ) ] , as.numeric )


	for( this_function in all_funs ){

		FUN <- get( this_function )
	
		unwtd_des <- convey_prep( svydesign( ~ 1 , data = dist_frame ) )
		binom_des <- convey_prep( svydesign( ~ 1 , data = dist_frame , weight = ~ wt_binom ) )
		unif_des <- convey_prep( svydesign( ~ 1 , data = dist_frame , weight = ~ wt_unif ) )

		unwtd_rep <- convey_prep( as.svrepdesign( unwtd_des , type = 'bootstrap' ) )
		binom_rep <- convey_prep( as.svrepdesign( binom_des , type = 'bootstrap' ) )
		unif_rep <- convey_prep( as.svrepdesign( unif_des , type = 'bootstrap' ) )


		for( this_prefix in c( 'unwtd' , 'binom' , 'unif' ) ){

			lin_des <- get( paste0( this_prefix , "_des" ) )
			rep_des <- get( paste0( this_prefix , "_rep" ) )

			for( this_formula in list( ~ beta_050_050 , ~ beta_500_100 , ~ beta_100_300 , ~ beta_200_200 , ~ beta_200_500 , ~ pois1 , ~ pois100 ) ){

				# function-specific parameters here

				if( any( unlist( lapply( list( svygei , svyrenyi , svyafc , svyamato , svybmi , svyatk , svyjdiv , svyzenga ) , function( z ) identical( z , FUN ) ) ) ) ){

					lin_des <- subset( lin_des , no.na( lin_des$variables[ , as.character( this_formula )[2] ] > 0 ) )
					rep_des <- subset( rep_des , no.na( rep_des$variables[ , as.character( this_formula )[2] ] > 0 ) )

				}

				if( any( unlist( lapply( list( svybmi , svyafc , svybcc ) , function( z ) identical( z , FUN ) ) ) ) ){

					lin_params_list <- list( as.formula( paste("~",as.character(this_formula)[2],"+wt_binom_two")) , lin_des )
					rep_params_list <- list( as.formula( paste("~",as.character(this_formula)[2],"+wt_binom_two")) , rep_des )


				} else {

					lin_params_list <- list( this_formula , lin_des )
					rep_params_list <- list( this_formula , rep_des )

				}


				if( identical( FUN , svyfgt ) ){

					lin_params_list <- c( lin_params_list , list( g=0, type_thresh= "abs", abs_thresh=10000 ) )
					rep_params_list <- c( rep_params_list , list( g=0, type_thresh= "abs", abs_thresh=10000 ) )

				}

				if( identical( FUN , svyafc ) ){

					lin_params_list <- c( lin_params_list , list( k = .5 , g = 0, cutoffs = list( 10000, 5000 ) ) )
					rep_params_list <- c( rep_params_list , list( k = .5 , g = 0, cutoffs = list( 10000, 5000 ) ) )

				}

				if( identical( FUN , svygpg ) ){

					lin_params_list <- c( lin_params_list , list( sex = ~sex ) )
					rep_params_list <- c( rep_params_list , list( sex = ~sex ) )

				}

				if( identical( FUN , svyiqalpha ) | identical( FUN , svyisq ) ){

					lin_params_list <- c( lin_params_list , list( alpha = 0.5 ) )
					rep_params_list <- c( rep_params_list , list( alpha = 0.5 ) )

				}

				if( identical( FUN , svyrmir ) ){

					lin_params_list <- c( lin_params_list , list( age = ~age ) )
					rep_params_list <- c( rep_params_list , list( age = ~age ) )

				}

			  if( identical( FUN , svybmi ) ){

			    lin_params_list <- c( lin_params_list )
			    rep_params_list <- c( rep_params_list )

			  }

			  if( identical( FUN , svysen ) ){

			    lin_params_list <- c( lin_params_list , abs_thresh = 10000 )
			    rep_params_list <- c( rep_params_list , abs_thresh = 10000 )

			  }

			  if( identical( FUN , svysst ) ){

			    lin_params_list <- c( lin_params_list , abs_thresh = 10000 )
			    rep_params_list <- c( rep_params_list , abs_thresh = 10000 )

			  }

			  if( identical( FUN , svybcc ) ){

			    lin_params_list <- c( lin_params_list , list( cutoffs = list( 10000, 5000 ) ) )
			    rep_params_list <- c( rep_params_list , list( cutoffs = list( 10000, 5000 ) ) )

			  }

			  if( identical( FUN , svyrich ) ){

			    lin_params_list <- c( lin_params_list , list( type_measure = "FGTT1", g=1, type_thresh= "abs", abs_thresh=30000 ) )
			    rep_params_list <- c( rep_params_list , list( type_measure = "FGTT1", g=1, type_thresh= "abs", abs_thresh=30000 ) )

			  }

			  if( identical( FUN , svychu ) ){

			    lin_params_list <- c( lin_params_list , list( g=1, type_thresh= "abs", abs_thresh=10000 ) )
			    rep_params_list <- c( rep_params_list , list( g=1, type_thresh= "abs", abs_thresh=10000 ) )

			  }

			  if( identical( FUN , svywatts ) ){

			    lin_params_list <- c( lin_params_list , list( type_thresh= "abs", abs_thresh=10000 ) )
			    rep_params_list <- c( rep_params_list , list( type_thresh= "abs", abs_thresh=10000 ) )

			  }

				if( this_prefix == 'unwtd' ) wt_vec <- seq( nrow( dist_frame ) )
				if( this_prefix == 'binom' ) wt_vec <- which( dist_frame$wt_binom > 0 )
				if( this_prefix == 'unif' ) wt_vec <- which( dist_frame$wt_unif > 0 )


				if(
					(
						( identical( svyrmpg , FUN ) | identical( svypoormed , FUN ) ) &
						coef( svyarpt( this_formula , lin_des ) ) < min( dist_frame[ wt_vec , as.character( this_formula )[2]] )
					) |
					(
						identical( svyqsr , FUN ) &
						identical( this_formula , ~pois1 )
					)
				){

					expect_error( do.call( FUN , lin_params_list ) )

				} else {

						lin_res <- do.call( FUN , lin_params_list )
						rep_res <- do.call( FUN , rep_params_list )

						print( paste( "testing functions work on weird distributions" , this_function , this_prefix , as.character( this_formula )[2] ) )

						# result should give some non-missing number
						expect_that( coef( lin_res ) , is.numeric )
						expect_that( coef( lin_res ) , function( w ) all( !is.na( w ) ) )

						# coefficients should be equal
						expect_equal( coef( lin_res ) , coef( rep_res ) )

						# difference between SEs for the designs should be less than 25% of the coef
						# but only for larger designs, since small ones can be unstable
						if ( n > 50 & all( coef( lin_res ) * .5 > SE( lin_res ) ) ){
							expect_true( all( abs( SE( lin_res ) - SE( rep_res ) ) <= max( coef( lin_res ) ) * 0.3 ) )
						}

					

				}

			}
		}

	}
}


})