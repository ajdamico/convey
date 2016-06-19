#' @importFrom stats approxfun coef deriv model.frame model.matrix na.pass printCoefmat qnorm terms terms.formula update weights
#' @importFrom methods is

# each of the functions below are from the survey library v3.30-3
# written by Thomas Lumley and copied here under the same GPL-3 license
# these functions were copied following the direction in this thread:
# https://stat.ethz.ch/pipermail/r-devel/2013-August/thread.html#67180

computeQuantiles <- function(xx, w, p) {
    if (any(is.na(xx)))
        return(NA * p)

	if( sum( w ) == 0 ) return( NA )

    oo <- order(xx)
    cum.w <- cumsum(w[oo])/sum(w)
    cdf <- approxfun(cum.w, xx[oo], method = "constant", f = 1, yleft = min(xx),
        yright = max(xx), ties = min)
    cdf(p)
}

checkConnection <- 
	function (dbconnection, error = TRUE) {
		
		if (is(dbconnection, "DBIConnection")) {
			if (!DBI::dbIsValid(dbconnection)) 
				if (error) 
					stop("Database connection is closed")
				else return(FALSE)
		} else {
		
		}
		invisible(TRUE)
	}

getvars <- function (formula, dbconnection, tables, db.only = TRUE, updates = NULL, 
    subset = NULL) {
    checkConnection(dbconnection)
    if (is.null(formula)) 
        return(NULL)
    if (inherits(formula, "formula")) {
        var0 <- all.vars(formula)
    }
    else if (is.character(formula)) {
        var0 <- formula
    }
    else {
        return(formula)
    }
    infilter <- updatesInfilter(var0, updates)
    if (db.only) {
        in.db <- infilter$varlist
    }
    else {
        query <- sub("@tab@", tables, "select * from @tab@ limit 1")
        if (is(dbconnection, "DBIConnection")) 
            oneline <- DBI::dbGetQuery(dbconnection, query)
        else oneline <- RODBC::sqlQuery(dbconnection, query)
        in.db <- infilter$varlist[infilter$varlist %in% names(oneline)]
    }
    query <- paste("select", paste(in.db, collapse = ", "), "from", 
        tables)
    if (is(dbconnection, "DBIConnection")) 
        df <- DBI::dbGetQuery(dbconnection, query)
    else df <- RODBC::sqlQuery(dbconnection, query)
    if (!is.null(subset)) 
        df <- df[subset, , drop = FALSE]
    df <- updatesOutfilter(df, var0, infilter$history, updates)
    is.string <- sapply(df, is.character)
    if (any(is.string)) {
        for (i in which(is.string)) df[[i]] <- as.factor(df[[i]])
    }
    df
}

updatesInfilter <-
	function (varlist, updates) {
		if (is.null(updates)) 
			return(list(varlist = varlist))
		n <- length(updates)
		v <- vector("list", n)
		for (i in n:1) {
			if (any(idx <- (varlist %in% names(updates[[i]])))) {
				v[[i]] <- varlist[idx]
				ups <- match(v[[i]], names(updates[[i]]))
				varlist <- unique(c(varlist[!idx], do.call(c, lapply(updates[[i]][ups], 
					"[[", "inputs"))))
			}
		}
		list(varlist = varlist, history = v)
	}

updatesOutfilter <-
	function (df, varlist, history, updates) {
		if (is.null(updates)) 
			return(df)
		if (all(sapply(history, length) == 0)) 
			return(df)
		n <- length(updates)
		for (i in 1:n) {
			if (mi <- length(history[[i]])) {
				outputs <- vector("list", mi)
				for (j in 1:mi) {
					idx.j <- match(history[[i]][j], names(updates[[i]]))
					outputs[[j]] <- eval(updates[[i]][[idx.j]]$expression, 
					  df)
				}
				names(outputs) <- history[[i]]
				if (any(mod <- names(df) %in% names(outputs))) {
					df <- df[, !mod, drop = FALSE]
				}
				df <- cbind(df, outputs)
			}
		}
		df[, names(df) %in% varlist, drop = FALSE]
	}

	
model.frame.survey.design<-function(formula,...,drop=TRUE){
  formula$variables
}
model.frame.svyrep.design<-function(formula,...){
  formula$variables
}
