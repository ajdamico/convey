#' title here
#'
#' subtitle here
#'
#' @param exprlist what does exprlist need to be?
#' @param infunlist what does infunlist need to be?
#'
#' @details put details here
#'
#' @return what does this function return
#'
#' @author Djalma Pessoa and Anthony Damico
#'
#' @seealso do you need a seealso block?
#'
#' @references do you need a references block?
#' 
#' @keywords keywords here
#'
#' @examples
#'
#' # usage example here
#'
#' @export
contrastinf <- function(exprlist, infunlist) {
    datalist <- lapply(infunlist, function(thresh) thresh$value)
    listlin <- lapply(infunlist, function(thresh) thresh$lin)
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
