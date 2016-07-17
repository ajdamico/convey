library(survey)
library(convey)

no.na <- function( z , value = FALSE ){ z[ is.na( z ) ] <- value ; z } 

# svyrmir

all_funs <- list( svyqsr , svyarpt , svyarpt , svyatk , svyfgt , svygini , svygpg , svyiqalpha , svyisq , svyzenga , svypoormed  , svyrenyi , svygei  , svyrmpg  , svyzengacurve , svylorenz )


for( n in c( 50 , 1000 ) ){

	set.seed( n )

	dist_frame <-
		data.frame( 
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

	for( FUN in all_funs ){

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
				
				if( identical( FUN , svygei ) | identical( FUN , svyrenyi ) ){
		
					lin_des <- subset( lin_des , no.na( lin_des$variables[ , as.character( this_formula )[2] ] > 0 ) )
					rep_des <- subset( rep_des , no.na( rep_des$variables[ , as.character( this_formula )[2] ] > 0 ) ) 

				}

				lin_params_list <- list( this_formula , lin_des )
				rep_params_list <- list( this_formula , rep_des )
				
					
				
				if( identical( FUN , svyfgt ) ){
					
					lin_params_list <- c( lin_params_list , list( g=0, type_thresh= "abs", abs_thresh=10000 ) )
					rep_params_list <- c( rep_params_list , list( g=0, type_thresh= "abs", abs_thresh=10000 ) )
					
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
		
				if( 
					(
						( identical( svyrmpg , FUN ) | identical( svypoormed , FUN ) ) & 
						coef( svyarpt( this_formula , lin_des ) ) < min( dist_frame[ , as.character( this_formula )[2]] ) 
					) |
					(
						identical( svyqsr , FUN ) &
						identical( this_formula , ~pois1 )
					)
				){
					
					expect_error( do.call( FUN , lin_params_list ) )
					
				} else {

					test_that(paste( "functions work on weird distributions" , this_prefix , as.character( this_formula )[2] ) ,{

						
						lin_res <- do.call( FUN , lin_params_list )
						rep_res <- do.call( FUN , rep_params_list )
						
						# result should give some non-missing number
						expect_that( coef( lin_res ) , is.numeric )  
						expect_that( coef( lin_res ) , function( w ) all( !is.na( w ) ) )  
						
						# coefficients should be equal
						expect_equal( coef( lin_res ) , coef( rep_res ) )
						
						# difference between SEs for the designs should be less than 25% of the coef
						# but only for larger designs, since small ones can be unstable
						if ( n > 50 & all( coef( lin_res ) * .5 > SE( lin_res ) ) ){
							expect_true( all( abs( SE( lin_res ) - SE( rep_res ) ) <= coef( lin_res ) * 0.25 ) )
						}
						
					})
					
				}
					
			}
		}
		
	}
}

