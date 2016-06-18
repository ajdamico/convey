#' @importFrom stats approxfun coef deriv model.frame model.matrix na.pass printCoefmat qnorm terms terms.formula update weights

# each of the functions below are from the survey library v3.30-3
# written by Thomas Lumley and copied here under the same GPL-3 license
# these functions were copied following the direction in this thread:
# https://stat.ethz.ch/pipermail/r-devel/2013-August/thread.html#67180

computeQuantiles <- function(xx, w, p = quantiles) {
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
			if (!dbIsValid(dbconnection)) 
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
            oneline <- dbGetQuery(dbconnection, query)
        else oneline <- sqlQuery(dbconnection, query)
        in.db <- infilter$varlist[infilter$varlist %in% names(oneline)]
    }
    query <- paste("select", paste(in.db, collapse = ", "), "from", 
        tables)
    if (is(dbconnection, "DBIConnection")) 
        df <- dbGetQuery(dbconnection, query)
    else df <- sqlQuery(dbconnection, query)
    if (!is.null(subset)) 
        df <- df[subset, , drop = FALSE]
    df <- updatesOutfilter(df, var0, infilter$history, updates)
    is.string <- sapply(df, is.character)
    if (any(is.string)) {
        for (i in which(is.string)) df[[i]] <- as.factor(df[[i]])
    }
    df
}


model.frame.survey.design<-function(formula,...,drop=TRUE){
  formula$variables
}
model.frame.svyrep.design<-function(formula,...){
  formula$variables
}

