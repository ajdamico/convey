#' Atkinson index
#'
#' Estimate the Atkinson index, a measure of inequality
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param epsilon a parameter that determines the sensivity towards inequality in the bottom of the distribution. Defaults to epsilon = 1.
#' @param na.rm Should cases with missing values be dropped?
#' @param ... future expansion
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Guilherme Jacob, Djalma Pessoa and Anthony Damico
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
#' library(laeken)
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
#'
#' # linearized design
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep(des_eusilc)
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep(des_eusilc_rep)
#'
#'
#' # subset all designs to positive income and non-missing records only
#' des_eusilc_pos_inc <- subset( des_eusilc , eqincome > 0 )
#' des_eusilc_rep_pos_inc <- subset( des_eusilc_rep , eqincome > 0 )
#'
#'
#' # linearized design
#' svyatk( ~eqincome , des_eusilc_pos_inc, epsilon = .5 )
#' svyatk( ~eqincome , des_eusilc_pos_inc )
#' svyatk( ~eqincome , des_eusilc_pos_inc, epsilon = 2 )
#'
#' # replicate-weighted design
#' svyatk( ~eqincome , des_eusilc_rep_pos_inc, epsilon = .5 )
#' svyatk( ~eqincome , des_eusilc_rep_pos_inc )
#' svyatk( ~eqincome , des_eusilc_rep_pos_inc, epsilon = 2 )
#'
#'
#' # subsetting
#' svyatk( ~eqincome , subset(des_eusilc_pos_inc, db040 == "Styria"), epsilon = .5 )
#' svyatk( ~eqincome , subset(des_eusilc_pos_inc, db040 == "Styria") )
#' svyatk( ~eqincome , subset(des_eusilc_pos_inc, db040 == "Styria"), epsilon = 2 )
#'
#' svyatk( ~eqincome , subset(des_eusilc_rep_pos_inc, db040 == "Styria"), epsilon = .5 )
#' svyatk( ~eqincome , subset(des_eusilc_rep_pos_inc, db040 == "Styria") )
#' svyatk( ~eqincome , subset(des_eusilc_rep_pos_inc, db040 == "Styria"), epsilon = 2 )
#'
#' \dontrun{
#'
#' # linearized design using a variable with missings (but subsetted to remove negatives)
#' svyatk( ~py010n , subset(des_eusilc, py010n > 0 | is.na(py010n)), epsilon = .5 )
#' svyatk( ~py010n , subset(des_eusilc, py010n > 0 | is.na(py010n)), epsilon = .5 , na.rm=TRUE )
#'
#' # replicate-weighted design using a variable with missings (but subsetted to remove negatives)
#' svyatk( ~py010n , subset(des_eusilc_rep, py010n > 0 | is.na(py010n)), epsilon = .5 )
#' svyatk( ~py010n , subset(des_eusilc_rep, py010n > 0 | is.na(py010n)), epsilon = .5 , na.rm=TRUE )
#'
#'
#' # database-backed design
#' library(RSQLite)
#' library(DBI)
#' dbfile <- tempfile()
#' conn <- dbConnect( RSQLite::SQLite() , dbfile )
#' dbWriteTable( conn , 'eusilc' , eusilc )
#'
#' dbd_eusilc <-
#' 	svydesign(
#' 		ids = ~rb030 ,
#' 		strata = ~db040 ,
#' 		weights = ~rb050 ,
#' 		data="eusilc",
#' 		dbname=dbfile,
#' 		dbtype="SQLite"
#' 	)
#'
#' dbd_eusilc <- convey_prep( dbd_eusilc )
#'
#'
#' # subset all designs to positive income and non-missing records only
#' dbd_eusilc_pos_inc <- subset( dbd_eusilc , eqincome > 0 )
#'
#'
#' # database-backed linearized design
#' svyatk( ~eqincome , dbd_eusilc_pos_inc, epsilon = .5 )
#' svyatk( ~eqincome , dbd_eusilc_pos_inc )
#' svyatk( ~eqincome , dbd_eusilc_pos_inc, epsilon = 2 )
#'
#' svyatk( ~eqincome , subset(dbd_eusilc_pos_inc, db040 == "Styria"), epsilon = .5 )
#' svyatk( ~eqincome , subset(dbd_eusilc_pos_inc, db040 == "Styria") )
#' svyatk( ~eqincome , subset(dbd_eusilc_pos_inc, db040 == "Styria"), epsilon = 2 )
#'
#' # database-backed linearized design using a variable with missings
#' # (but subsetted to remove negatives)
#' svyatk( ~py010n , subset(dbd_eusilc, py010n > 0 | is.na(py010n)), epsilon = .5 )
#' svyatk( ~py010n , subset(dbd_eusilc, py010n > 0 | is.na(py010n)), epsilon = .5 , na.rm=TRUE )
#'
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
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
			class(rval) <- c( "cvystat" , "svystat" )
			attr(rval, "var") <- variance
			attr(rval, "statistic") <- "atkinson"
			attr(rval,"epsilon")<- epsilon
			return(rval)
		}

		if ( any(incvar[w != 0] <= 0) ) stop( "The Atkinson Index is defined for strictly positive variables only.  Negative and zero values not allowed." )

		w <- 1/design$prob

		rval <- NULL
		rval <- calc.atkinson( x = incvar, weights = w, epsilon = epsilon )

		if ( is.na(rval) ) {
			variance <- as.matrix(NA)
			colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
			class(rval) <- c( "cvystat" , "svystat" )
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
		class(rval) <- c( "cvystat" , "svystat" )
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

		if ( any( incvar[ws != 0] <= 0, na.rm = TRUE ) ) stop( "The Atkinson Index is defined for strictly positive variables only.  Negative and zero values not allowed." )

		ws <- weights(design, "sampling")
		rval <- calc.atkinson( x = incvar, weights = ws, epsilon = epsilon)
		ww <- weights(design, "analysis")
		qq <- apply(ww, 2, function(wi) calc.atkinson(incvar, wi, epsilon = epsilon))

		if ( any(is.na(qq))) {

			variance <- as.matrix(NA)
			colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
			class(rval) <- c( "cvystat" , "svrepstat" )
			attr(rval, "var") <- variance
			attr(rval, "statistic") <- "atkinson"
			attr(rval,"epsilon")<- epsilon

			return(rval)

		} else {

			variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

			variance <- as.matrix( variance )

		}

		colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
		class(rval) <- c( "cvystat" , "svrepstat" )
		attr(rval, "var") <- variance
		attr(rval, "statistic") <- "atkinson"
		attr(rval,"epsilon")<- epsilon
		return(rval)

	}


#' @rdname svyatk
#' @export
svyatk.DBIsvydesign <-
	function (formula, design, ...) {

	  design$variables <- getvars( formula, design$db$connection, design$db$tablename, updates = design$updates, subset = design$subset )

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
