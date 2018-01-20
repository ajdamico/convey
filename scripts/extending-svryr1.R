## ------------------------------------------------------------------------
# S3 generic function
survey_gini <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), .svy = current_svy(), ...
) {
  UseMethod("survey_gini", .svy)
}

## ------------------------------------------------------------------------
survey_gini.tbl_svy <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), .svy = current_svy(), ...
) {
  if (missing(vartype)) vartype <- "se"
  vartype <- match.arg(vartype, several.ok = TRUE)
  .svy <- srvyr::set_survey_vars(.svy, x )
  
  out <- convey::svygini(~`__SRVYR_TEMP_VAR__`, na.rm = na.rm, design = .svy)
  out <- srvyr::get_var_est(out, vartype)
  out
}

## ------------------------------------------------------------------------
survey_gini.grouped_svy <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), .svy = current_svy(), ...
) {
  if (missing(vartype)) vartype <- "se"
  vartype <- match.arg(vartype, several.ok = TRUE)
  .svy <- srvyr::set_survey_vars(.svy, x)
  grps_formula <- survey::make.formula(group_vars(.svy))
  
  out <- survey::svyby(
    ~`__SRVYR_TEMP_VAR__`, grps_formula, convey::svygini, na.rm = na.rm, design = .svy
  )
  out <- srvyr::get_var_est(out, vartype, grps = group_vars(.svy))
  out
}

## ------------------------------------------------------------------------
# Example from ?convey::svygini
suppressPackageStartupMessages({
  library(srvyr)
  library(survey)
  library(convey)
  library(vardpoor)
})
data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )

# Setup for survey package
des_eusilc <- svydesign(
  ids = ~rb030, 
  strata = ~db040,  
  weights = ~rb050, 
  data = eusilc
)
des_eusilc <- convey_prep(des_eusilc)

# Setup for srvyr package
srvyr_eusilc <- eusilc %>% 
  as_survey(
    ids = rb030,
    strata = db040,
    weights = rb050
  ) %>%
  convey_prep()

## Ungrouped
# Calculate ungrouped for survey package
svygini(~eqincome, design = des_eusilc)

# With our new function
survey_gini(srvyr_eusilc$variables$eqincome, .svy = srvyr_eusilc)

# And finally, the more typical way through summarize
srvyr_eusilc %>% 
  summarize(eqincome = survey_gini(eqincome))

## Groups
# Calculate by groups for survey
survey::svyby(~eqincome, ~rb090, des_eusilc, convey::svygini)

# With our new function
survey_gini(srvyr_eusilc$variables$eqincome, .svy = group_by(srvyr_eusilc, rb090))

# And finally, the more typical way through summarize
srvyr_eusilc %>% 
  group_by(rb090) %>%
  summarize(eqincome = survey_gini(eqincome))


## ------------------------------------------------------------------------
# S3 generic function
survey_arpt <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), quantiles = 0.5,
  percent = 0.6, .svy = current_svy(), ...
) {
  UseMethod("survey_arpt", .svy)
}

## ------------------------------------------------------------------------
survey_arpt.tbl_svy <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), quantiles = 0.5,
  percent = 0.6, .svy = current_svy(), ...
) {
  if (missing(vartype)) vartype <- "se"
  vartype <- match.arg(vartype, several.ok = TRUE)
  .svy <- srvyr::set_survey_vars(.svy, x)
  attributes(.svy)$full_design <- srvyr::set_survey_vars(attributes(.svy)$full_design,x)
   out <- convey::svyarpt(~`__SRVYR_TEMP_VAR__` , na.rm = na.rm, design = .svy)
  out <- srvyr::get_var_est(out, vartype)
  out
}

## ------------------------------------------------------------------------


survey_arpt.grouped_svy <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), quantiles = 0.5,
  percent = 0.6, .svy = current_svy(), ...
) {
  if (missing(vartype)) vartype <- "se"
  vartype <- match.arg(vartype, several.ok = TRUE)
  .svy <- srvyr::set_survey_vars(.svy, x)
  
  attributes(.svy)$full_design <- srvyr::set_survey_vars(attributes(.svy)$full_design,x)
  grps_formula <- survey::make.formula(group_vars(.svy))
  
  out <- survey::svyby(
    ~`__SRVYR_TEMP_VAR__`, grps_formula, convey::svyarpt, na.rm = na.rm, design = .svy
  )
  out <- srvyr::get_var_est(out, vartype, grps = group_vars(.svy))
  out
}

## ------------------------------------------------------------------------
## Ungrouped
# Calculate ungrouped for survey package
svyarpt(~eqincome, design = des_eusilc)

# With our new function


survey_arpt.tbl_svy(srvyr_eusilc$variables$eqincome, .svy = srvyr_eusilc)


# And finally, the more typical way through summarize
srvyr_eusilc %>%  summarize(eqincome = survey_arpt(eqincome))

## Groups
# Calculate by groups for survey

survey::svyby(~eqincome, ~rb090, des_eusilc, convey::svyarpt) 

# And finally, the more typical way through summarize
srvyr_eusilc %>% 
  group_by(rb090) %>%
  summarize(eqincome = survey_arpt(eqincome))


## ------------------------------------------------------------------------
# S3 generic function
survey_arpr <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), quantiles = 0.5,
  percent = 0.6, .svy = current_svy(), ...
) {
  UseMethod("survey_arpr", .svy)
}

## ------------------------------------------------------------------------
survey_arpr.tbl_svy <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), quantiles = 0.5,
  percent = 0.6, .svy = current_svy(), ...
) {
  if (missing(vartype)) vartype <- "se"
  vartype <- match.arg(vartype, several.ok = TRUE)
  .svy <- srvyr::set_survey_vars(.svy, x)
  attributes(.svy)$full_design <- srvyr::set_survey_vars(attributes(.svy)$full_design,x)
   out <- convey::svyarpr(~`__SRVYR_TEMP_VAR__` , na.rm = na.rm, design = .svy)
  out <- srvyr::get_var_est(out, vartype)
  out
}

## ------------------------------------------------------------------------
survey_arpr.grouped_svy <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), quantiles = 0.5,
  percent = 0.6, .svy = current_svy(), ...
) {
  if (missing(vartype)) vartype <- "se"
  vartype <- match.arg(vartype, several.ok = TRUE)
  .svy <- srvyr::set_survey_vars(.svy, x)
  
  attributes(.svy)$full_design <- srvyr::set_survey_vars(attributes(.svy)$full_design,x)
  grps_formula <- survey::make.formula(group_vars(.svy))
  
  out <- survey::svyby(
    ~`__SRVYR_TEMP_VAR__`, grps_formula, convey::svyarpr, na.rm = na.rm, design = .svy
  )
  out <- srvyr::get_var_est(out, vartype, grps = group_vars(.svy))
  out
}

## ------------------------------------------------------------------------
## Ungrouped
# Calculate ungrouped for survey package
svyarpr(~eqincome, design = des_eusilc)

# With our new function


survey_arpr.tbl_svy(srvyr_eusilc$variables$eqincome, .svy = srvyr_eusilc)


# And finally, the more typical way through summarize
srvyr_eusilc %>%  summarize(eqincome = survey_arpr(eqincome))

## Groups
# Calculate by groups for survey

survey::svyby(~eqincome, ~rb090, des_eusilc, convey::svyarpr) 

# And finally, the more typical way through summarize
srvyr_eusilc %>% 
  group_by(rb090) %>%
  summarize(eqincome = survey_arpr(eqincome))


## ------------------------------------------------------------------------
# S3 generic function
survey_fgt <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), g, type_thresh ="abs", abs_thresh = NULL, quantiles = 0.5,
  percent = 0.6, .svy = current_svy(), ...) 
  {
  UseMethod("survey_fgt", .svy)
}

## ------------------------------------------------------------------------
survey_fgt.tbl_svy <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), g, type_thresh ="abs", abs_thresh = NULL, quantiles = 0.5,  percent = 0.6, .svy = current_svy(), ...
) {
  if (missing(vartype)) vartype <- "se"
  vartype <- match.arg(vartype, several.ok = TRUE)
  .svy <- srvyr::set_survey_vars(.svy, x)
  attributes(.svy)$full_design <- srvyr::set_survey_vars(attributes(.svy)$full_design,x)
   out <- convey::svyfgt(~`__SRVYR_TEMP_VAR__` , na.rm = na.rm, g=g, type_thresh = type_thresh, abs_thresh = abs_thresh, quantiles = quantiles, percent = percent, design = .svy)
  out <- srvyr::get_var_est(out, vartype)
  out
}

## ------------------------------------------------------------------------

survey_fgt.grouped_svy <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), g, type_thresh ="abs", abs_thresh =NULL , quantiles = 0.5,  percent = 0.6, .svy = current_svy(), ...
) {
  if (missing(vartype)) vartype <- "se"
  vartype <- match.arg(vartype, several.ok = TRUE)
  .svy <- srvyr::set_survey_vars(.svy, x)
  
  attributes(.svy)$full_design <- srvyr::set_survey_vars(attributes(.svy)$full_design,x)
  grps_formula <- survey::make.formula(group_vars(.svy))
  
  out <- survey::svyby(
    ~`__SRVYR_TEMP_VAR__`, grps_formula, convey::svyfgt, g = g, type_thresh = type_thresh, abs_thresh = abs_thresh, quantiles = quantiles, percent = percent, design = .svy )
  out <- srvyr::get_var_est(out, vartype, grps = group_vars(.svy))
  out
}

## ------------------------------------------------------------------------
## Ungrouped
# Calculate ungrouped for survey package
convey::svyfgt(~eqincome, des_eusilc, g=0, abs_thresh=10000 )

# With our new function

undebug(survey_fgt.tbl_svy)
survey_fgt.tbl_svy(srvyr_eusilc$variables$eqincome, g=0, abs_thresh = 10000, .svy = srvyr_eusilc)


# And finally, the more typical way through summarize
srvyr_eusilc %>%  summarize(eqincome = survey_fgt(eqincome, g = 0, abs_thresh=10000 ))

## Groups
# Calculate by groups for survey

survey::svyby(~eqincome, ~rb090, des_eusilc, convey::svyfgt, g = 0, abs_thresh=10000) 

# And finally, the more typical way through summarize
srvyr_eusilc %>% 
  group_by(rb090) %>%
  summarize(eqincome = survey_fgt(eqincome, g=0, abs_thresh=10000))


