#' Atkinson index
#'
#' Estimate the Atkinson index, a measure of inequality
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param epsilon a parameter that determines the sensivity towards inequality in the bottom of the distribution. Defaults to epsilon = 1.
#' @param na.rm Should cases with missing values be dropped?
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Djalma Pessoa, Anthony Damico and Guilherme Jacob
#'
#' @seealso \code{\link{svygei}}
#'
#' @references Matti Langel (2012). Measuring inequality in finite population sampling.
#' PhD thesis: Universite de Neuchatel,
#' URL \url{https://doc.rero.ch/record/29204/files/00002252.pdf}.
#'
#' Martin Biewen and Stephen Jenkins (2002). Estimation of Generalized Entropy
#' and Atkinson Inequality Indices from Complex Survey Data. \emph{DIW Discussion Papers},
#' No.345,
#' URL \url{https://www.diw.de/documents/publikationen/73/diw_01.c.40394.de/dp345.pdf}.
#' @keywords survey
#'
#' @examples
#' library(survey)
#' library(vardpoor)
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
#'
#' # linearized design
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep(des_eusilc)
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#'
#' # database-backed design
#' library(MonetDBLite)
#' library(DBI)
#' dbfolder <- tempdir()
#' conn <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
#' dbWriteTable( conn , 'eusilc' , eusilc )
#'
#' dbd_eusilc <-
#' 	svydesign(
#' 		ids = ~rb030 ,
#' 		strata = ~db040 , 
#' 		weights = ~rb050 ,
#' 		data="eusilc",
#' 		dbname=dbfolder,
#' 		dbtype="MonetDBLite"
#' 	)
#' 
#' dbd_eusilc <- convey_prep( dbd_eusilc )
#'
#' # linearized design
#' svyatk( ~eqincome , design = des_eusilc, epsilon = .5 )
#' svyatk( ~eqincome , design = subset(des_eusilc, eqincome > 0), epsilon = 1 )
#' svyatk( ~eqincome , design = des_eusilc, epsilon = 2 )
#'
#' # database-backed linearized design
#' svyatk( ~eqincome , design = dbd_eusilc, epsilon = .5 )
#' svyatk( ~eqincome , design = subset(dbd_eusilc, eqincome > 0), epsilon = 1 )
#' svyatk( ~eqincome , design = dbd_eusilc, epsilon = 2 )
#'
#' # replicate-weighted design
#' svyatk( ~eqincome , design = des_eusilc_rep, epsilon = .5 )
#' svyatk( ~eqincome , design = subset(des_eusilc_rep, eqincome > 0), epsilon = 1 )
#' svyatk( ~eqincome , design = des_eusilc_rep, epsilon = 2 )
#'
#' # linearized design using a variable with missings
#' svyatk( ~py010n , design = des_eusilc, epsilon = .5 )
#' svyatk( ~py010n , design = des_eusilc, epsilon = .5, na.rm = TRUE )
#' svyatk( ~py010n , design = subset(des_eusilc, py010n > 0 | is.na(py010n)), epsilon = 1 )
#' svyatk( ~py010n , design = subset(des_eusilc, py010n > 0 | is.na(py010n)), epsilon = 1, na.rm = TRUE )
#' svyatk( ~py010n , design = des_eusilc, epsilon = 2 )
#' svyatk( ~py010n , design = des_eusilc, epsilon = 2, na.rm = TRUE )
#'
#' # database-backed linearized design using a variable with missings
#' svyatk( ~py010n , design = dbd_eusilc, epsilon = .5 )
#' svyatk( ~py010n , design = dbd_eusilc, epsilon = .5, na.rm = TRUE )
#' svyatk( ~py010n , design = subset(dbd_eusilc, py010n > 0 | is.na(py010n)), epsilon = 1 )
#' svyatk( ~py010n , design = subset(dbd_eusilc, py010n > 0 | is.na(py010n)), epsilon = 1, na.rm = TRUE )
#' svyatk( ~py010n , design = dbd_eusilc, epsilon = 2 )
#' svyatk( ~py010n , design = dbd_eusilc, epsilon = 2, na.rm = TRUE )
#'
#' # replicate-weighted design using a variable with missings
#' svyatk( ~py010n , design = des_eusilc_rep, epsilon = .5 )
#' svyatk( ~py010n , design = des_eusilc_rep, epsilon = .5, na.rm = TRUE )
#' svyatk( ~py010n , design = subset(des_eusilc_rep, py010n > 0 | is.na(py010n)), epsilon = 1 )
#' svyatk( ~py010n , design = subset(des_eusilc_rep, py010n > 0 | is.na(py010n)), epsilon = 1, na.rm = TRUE )
#' svyatk( ~py010n , design = des_eusilc_rep, epsilon = 2 )
#' svyatk( ~py010n , design = des_eusilc_rep, epsilon = 2, na.rm = TRUE )
#'
#' # subsetting
#' svyatk( ~eqincome , design = subset(des_eusilc, db040 == "Styria"), epsilon = .5 )
#' svyatk( ~eqincome , design = subset(des_eusilc, eqincome > 0 & db040 == "Styria"), epsilon = 1 )
#' svyatk( ~eqincome , design = subset(des_eusilc, db040 == "Styria"), epsilon = 2 )
#'
#' svyatk( ~eqincome , design = subset(dbd_eusilc, db040 == "Styria"), epsilon = .5 )
#' svyatk( ~eqincome , design = subset(dbd_eusilc, eqincome > 0 & db040 == "Styria"), epsilon = 1 )
#' svyatk( ~eqincome , design = subset(dbd_eusilc, db040 == "Styria"), epsilon = 2 )
#'
#' svyatk( ~eqincome , design = subset(des_eusilc_rep, db040 == "Styria"), epsilon = .5 )
#' svyatk( ~eqincome , design = subset(des_eusilc_rep, eqincome > 0 & db040 == "Styria"), epsilon = 1 )
#' svyatk( ~eqincome , design = subset(des_eusilc_rep, db040 == "Styria"), epsilon = 2 )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' @export
svyatk <-
	function(formula, design, ...) {

		if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

		if( 'epsilon' %in% names( list(...) ) && list(...)[["epsilon"]] <= 0 ) stop( "epsilon= must be positive." )

		UseMethod("svyatk", design)

	}


#' @rdname svyatk
#' @export
svyatk.survey.design <-
	function ( formula, design, epsilon = 1, na.rm = FALSE, ... ) {

		incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

		if (na.rm) {
			nas <- is.na(incvar)
			design <- design[nas == 0, ]
			if (length(nas) > length(design$prob))
			incvar <- incvar[nas == 0]
			else incvar[nas > 0] <- 0
		}

		w <- 1/design$prob
		if ( any( is.na(incvar [w != 0]) ) ) {
			rval <- NA
			variance <- as.matrix(NA)
			colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
			class(rval) <- "cvystat"
			attr(rval, "var") <- variance
			attr(rval, "statistic") <- "atkinson"
			attr(rval,"epsilon")<- epsilon
			return(rval)
		}

		if ( any(incvar[w != 0] <= 0) ){
			warning("The function is defined for strictly positive incomes only.  Discarding observations with zero or negative incomes.")
			nps <- incvar <= 0
			design <- design[!nps]
			if (length(nps) > length(design$prob)) incvar <- incvar[!nps] else incvar[ nps ] <- 0
		}

		w <- 1/design$prob

		rval <- NULL
		rval <- calc.atkinson( x = incvar, weights = w, epsilon = epsilon )

		if ( is.na(rval) ) {
			variance <- as.matrix(NA)
			colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
			class(rval) <- "cvystat"
			attr(rval, "var") <- variance
			attr(rval, "statistic") <- "atkinson"
			attr(rval,"epsilon")<- epsilon
			return(rval)
		}

		if ( epsilon != 1 ) {

			v <-
				( ( epsilon ) / ( 1 - epsilon ) ) *
				U_fn( incvar , w , 1 )^( -1 ) *
				U_fn( incvar , w , 1 - epsilon )^( 1 / ( 1 - epsilon ) ) *
				U_fn( incvar , w , 0 )^( -1 / ( 1 - epsilon ) ) +

				U_fn( incvar , w , 0 )^( -epsilon / ( 1 - epsilon ) ) *
				U_fn( incvar , w , 1 - epsilon )^( 1 / ( 1 - epsilon ) ) *
				U_fn( incvar , w , 1 )^( -2 ) *
				incvar -

				( 1 / ( 1 - epsilon ) ) *
				U_fn( incvar , w , 0 )^( -epsilon / ( 1 - epsilon ) ) *
				U_fn( incvar , w , 1 )^( -1 ) *
				U_fn( incvar , w , 1 - epsilon )^( epsilon / ( 1 - epsilon ) ) *
				incvar^( 1 - epsilon )

		} else {

			v <-
				( rval - 1 ) *
				U_fn( incvar , w , 0 )^( -1 ) *
				( 1 - U_fn( incvar , w , 0 )^( -1 ) * T_fn( incvar[w != 0] , w[ w != 0 ] , 0 ) ) +

				( 1 - rval ) * U_fn( incvar , w , 1 )^( -1 ) * incvar +
				( rval - 1 ) * U_fn( incvar , w , 0 )^( -1 ) *
				log( incvar )

		}

		v[w == 0] <- 0

		variance <- survey::svyrecvar(v/design$prob, design$cluster, design$strata, design$fpc, postStrata = design$postStrata)

		colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
		class(rval) <- "cvystat"
		attr(rval, "var") <- variance
		attr(rval, "statistic") <- "atkinson"
		attr(rval,"epsilon")<- epsilon

		rval
	}


#' @rdname svyatk
#' @export
svyatk.svyrep.design <-
	function(formula, design, epsilon = 1, na.rm=FALSE, ...) {

		incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

		if(na.rm){
			nas<-is.na(incvar)
			design<-design[!nas,]
			df <- model.frame(design)
			incvar <- incvar[!nas]
		}

		ws <- weights(design, "sampling")

		if ( any( incvar[ws != 0] <= 0, na.rm = TRUE ) ) {

			warning( "The function is defined for strictly positive incomes only.  Discarding observations with zero or negative incomes.")
			nps <- incvar <= 0
			nps[ is.na(nps) ] <- TRUE
			design <- design[ !nps ]
			if (length(nps) > length(design$prob)) incvar <- incvar[ !nps ] else incvar[ nps ] <- 0

		}

		ws <- weights(design, "sampling")
		rval <- calc.atkinson( x = incvar, weights = ws, epsilon = epsilon)
		ww <- weights(design, "analysis")
		qq <- apply(ww, 2, function(wi) calc.atkinson(incvar, wi, epsilon = epsilon))

		if ( any(is.na(qq))) {

			variance <- as.matrix(NA)
			colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
			class(rval) <- "cvystat"
			attr(rval, "var") <- variance
			attr(rval, "statistic") <- "atkinson"
			attr(rval,"epsilon")<- epsilon

			return(rval)

		} else {

			variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

			variance <- as.matrix( variance )

		}

		colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
		class(rval) <- "cvystat"
		attr(rval, "var") <- variance
		attr(rval, "statistic") <- "atkinson"
		attr(rval,"epsilon")<- epsilon
		return(rval)

	}


#' @rdname svyatk
#' @export
svyatk.DBIsvydesign <-
	function (formula, design, ...) {

		if (!( "logical" %in% class(attr(design, "full_design"))) ){

			full_design <- attr( design , "full_design" )

			full_design$variables <-
				survey:::getvars(
					formula,
					attr( design , "full_design" )$db$connection,
					attr( design , "full_design" )$db$tablename,
					updates = attr( design , "full_design" )$updates,
					subset = attr( design , "full_design" )$subset
				)

			attr( design , "full_design" ) <- full_design

			rm( full_design )

		}

		design$variables <-
			survey:::getvars(
				formula,
				design$db$connection,
				design$db$tablename,
				updates = design$updates,
				subset = design$subset
			)

		NextMethod("svyatk", design)
	}





calc.atkinson <-
	function( x, weights, epsilon ) {

		x <- x[ weights != 0 ]

		weights <- weights[ weights != 0 ]

		if ( epsilon == 1 ) {

			result.est <-
				1 -
				U_fn( x , weights , 0 ) *
				U_fn( x , weights , 1 )^( -1 ) *
				exp( T_fn( x , weights , 0 ) / U_fn( x , weights , 0 ) )

		} else {

			result.est <-
				1 -
				( U_fn( x , weights , 0 )^( -epsilon / ( 1 - epsilon ) ) ) *
				U_fn( x , weights , 1 - epsilon )^( 1 / ( 1 - epsilon ) )  / U_fn( x , weights , 1 )

		}

		result.est
	}
