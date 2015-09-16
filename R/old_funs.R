# old svyarpr replaced

 svyarpr.survey.design<-
function (formula, design, order = 0.5, percent = 0.6, h, ARPT,
  ncom, ...)
{
  inc <- terms.formula(formula)[[2]]
  df <- model.frame(design)
  incvar <- df[[as.character(inc)]]
  w <- weights(design)
  ARPT_val <- ARPT$value
  lin_ARPT <- ARPT$lin
  poor <- (incvar < ARPT_val) * 1
  design <- update(design, poor = poor)
  ARPRC <- svymean(~poor, design = design)
  ARPRC <- coef(ARPRC)
  lin_ARPR <- icdf(formula = formula, design = design, ARPT_val,
    ncom = ncom, comp = TRUE)$lin + densfun(formula = formula,
      design = design, ARPT_val, htot = h, fun = "F") * lin_ARPT
  list(value = ARPRC, lin = lin_ARPR)
}

 ###

 iqalpha <- function(formula, design, alpha, h = NULL, ncom, comp, incvec = NULL,
   ...) {
   inc <- terms.formula(formula)[[2]]
   df <- model.frame(design)
   incvar <- df[[as.character(inc)]]
   w <- weights(design)
   ind <- names(w)
   q_alpha <- svyquantile(x = formula, design = design, quantiles = alpha, method = "constant")
   q_alpha <- as.vector(q_alpha)
   N <- sum(w)
   Fprime <- densfun(formula = formula, design = design, q_alpha, htot = h, fun = "F")
   iq <- -(1/(N * Fprime)) * ((incvar <= q_alpha) - alpha)
   if (!is.null(incvec)) {
     iq <- -(1/(N * Fprime)) * ((incvec <= q_alpha) - alpha)
     comp <- FALSE
   }
   if (comp) {
     names(iq) <- ind
     iq_comp <- complete(iq, ncom)
     res = iq_comp
   } else res <- iq
   list(value = q_alpha, lin = res)
 }

 svyarpt.survey.design <- function(formula, design, order = 0.5, percent = 0.6, h,
   ncom, comp, ...) {
   w <- weights(design)
   ind <- names(w)
   quant_val <- svyquantile(x = formula, design = design, quantiles = order, method = "constant")
   quant_val <- as.vector(quant_val)
   ARPT <- percent * quant_val
   lin_ARPT <- percent * iqalpha(formula = formula, design = design, alpha = order,
     h = h, ncom = ncom, comp = FALSE, incvec = NULL)$lin
   names(lin_ARPT) <- ind
   lin_ARPT_comp <- complete(lin_ARPT, ncom)
   if (comp)
     lin <- lin_ARPT_comp else lin <- lin_ARPT
   list(value = ARPT, lin = lin)
 }





