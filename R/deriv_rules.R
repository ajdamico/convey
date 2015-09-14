

# Auxiliary functions Implement the rules for Influence functions in Osier's
# paper

#' @rdname funs
#' @export
# 1. influence function of a constant: formula (28)
iconst <- function(formula, design) {
    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    list(value = 0, lin = rep(0, length(incvar)))
}

#' @rdname funs
#' @export
# 1.  infuence function of a total: formula (34)
itot <- function(formula, design) {
    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    value <- coef(svytotal(x = formula, design = design))
    lin <- incvar
    list(value = value, lin = lin)
}



## derivation rules for influence functions of functionals linear combination of
## functionals: formula (29) a, b - scalars T, S - lists with two components:
## value and lin IF - list with with two components Fprime - real function
#' @rdname funs
#' @export

cl_inf <- function(a, b, T, S) {
    lin <- a * T$lin + b * S$lin
    value <- a * T$value + b * S$value
    list(value = value, lin = lin)
}

#' @rdname funs
#' @export
# product of of two functionals: formula (30)
prod_inf <- function(T, S) {
    
    value <- T$value * S$value
    lin <- T$value * S$lin + S$value * T$lin
    list(value = value, lin = lin)
}

#' @rdname funs
#' @export
# ratio of functionals: formula (31)
ratio_inf <- function(T, S) {
    value <- T$value/S$value
    lin <- (S$value * T$lin - T$value * S$lin)/((S$value)^2)
    list(value = value, lin = lin)
}

#' @rdname funs
#' @export
# composition of two functionals: formula (32)
comp_inf <- function(T, S) {
    itsm <- rep(S$value, length(T$lin))
    itsm * S$lin
}

#' @rdname funs
#' @export
# function of a functional: (33) F and Fprime are names function names
fun_par_inf <- function(S, F, Fprime, ...) {
    dots <- list(...)
    value <- do.call(F, c(x = S$value, dots))$value
    lin <- do.call(F, c(x = S$value, dots))$lin + do.call(Fprime, c(x = S$value, 
        dots)) * S$lin
    list(value = value, lin = lin)
}

## function of functionals T(M)= a{T1(M), T2(M),...} IT(M)= sum((da/dTj)* ITj(M))

# exemplo: razão de dois totais T= Y/X IT= 1/X*I(Y)-Y/X^2*I(X) Use deriv to get
# da/dTj

# expression for the function a list of object generated the linearization
# functions.


#' @rdname funs
#' @export
contrastinf <- function(exprlist, infunlist) {
    datalist <- lapply(infunlist, function(t) t$value)
    listlin <- lapply(infunlist, function(t) t$lin)
    if (!is.list(exprlist)) 
        exprlist <- list(contrast = exprlist)
    dexprlist <- lapply(exprlist, function(expr) deriv(expr, names(datalist))[[1]])
    value <- eval(exprlist$contrast, datalist)
    values_deriv <- lapply(dexprlist, function(dexpr) eval(do.call(substitute, list(dexpr, 
        datalist))))
    matval <- attr(values_deriv$contrast, "gradient")
    matlin <- matrix(NA, length(infunlist[[1]]$lin), ncol(matval))
    for (i in 1:length(listlin)) matlin[, i] <- listlin[[i]]
    IT_lin <- matlin %*% t(matval)
    list(value = value, lin = IT_lin)
}

###############################################################

svyarpr.survey.design1 <- function(formula, design, order = .50, percent =.6, h, ARPT, ncom, ...){
    ARPR<-fun_par_inf(ARPT, "icdf", "densfun", formula=formula ,design= design,
    ncom=ncom ,  comp= TRUE, htot=NULL, fun="F")
    list(value = ARPR$value, lin = ARPR$lin)
}


#' @rdname funs
#' @export
fun_par_inf<- function(S,F,Fprime,...){
  dots<- list(...)
  value<- do.call(F,c(x=S$value, dots))$value
  lin<- do.call(F,c(x=S$value,dots))$lin+
    do.call(Fprime,c(x=S$value,dots))*S$lin
  list(value= value, lin=lin)
}

 
