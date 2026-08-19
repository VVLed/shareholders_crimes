library(data.table)
library(ggplot2)
library(fst)
library(fixest)
library(stringr); library(stringi)
options(scipen = 999)

extract_estimates <- function(x, regular_expression_1 = "roa_b", regular_expression_2 = "roa_adj_hist", model_id = "name") {
  data.table(
    roa_b1_slope = x$coefficients[str_detect(names(x$coefficients), regular_expression_1)][1],
    roa_hist_b2_slope = x$coefficients[str_detect(names(x$coefficients), regular_expression_2)][1],
    roa_b2_slope = x$coefficients[str_detect(names(x$coefficients), regular_expression_1)][2],
    
    roa_b1_se = x$se[str_detect(names(x$se), regular_expression_1)][1],
    roa_hist_b2_se = x$se[str_detect(names(x$se), regular_expression_2)][1],
    roa_b2_se = x$se[str_detect(names(x$se), regular_expression_1)][2],
    
    model = model_id
  )
}

create_formula <- function(dep_var) {
  as.formula(paste(dep_var, "~", 
                   paste(control_vars, collapse = " + "),
                   "|", 
                   paste(c("year", "inn"), collapse = " + ")))
}

path <- Sys.getenv("SHAREHOLDERS_CRIMES_PATH")
setwd(path)

# Load the data ====

llcs_panel_full <- read_fst("panel_disp_vs_strain.fst", as.data.table = TRUE)

# Transform the data ====

cols_to_transform <- grep("roa", colnames(llcs_panel_full), value = TRUE)
llcs_panel_full[, (cols_to_transform) := lapply(.SD, function (x) asinh(x)), .SDcols = cols_to_transform]

cols_to_log <- grep("market_share|total_assets|revenue", colnames(llcs_panel_full))
llcs_panel_full[, (cols_to_log) := lapply(.SD, function (x) log(x)), .SDcols = cols_to_log]

cols_to_square <- grep("age", colnames(llcs_panel_full), value = TRUE)
llcs_panel_full[, (paste0(cols_to_square, "_squared")) := lapply(.SD, function (x) x^2), .SDcols = cols_to_square]

setFixest_dict(c(
  pp_case_any = "Any claim",
  pp_case_satisfied = "Satisfied claim",
  pp_case_any_key = "Any key claim",
  pp_case_satisfied_key = "Satisfied key claim",
  roa_b1 = "ROA t-1",
  roa_b2 = "ROA t-2",
  roa_b3 = "ROA t-3",
  roa_adj_hist_b1 = "Historical ROA t-1",
  roa_adj_hist_b2 = "Historical ROA t-2",
  roa_adj_hist_b3 = "Historical ROA t-3",
  market_share_b1 = "Ln of market share t-1",
  market_share_b2 = "Ln of market share t-2",
  market_share_b3 = "Ln of market share t-3",
  total_assets_b1 = "Ln of assets t-1",
  total_assets_b2 = "Ln of assets t-2",
  total_assets_b3 = "Ln of assets t-3",
  revenue_b1 = "Ln of revenue t-1",
  revenue_b2 = "Ln of revenue t-2",
  revenue_b3 = "Ln of revenue t-3",
  lr_ind_year_b1 = "Litigation risk t-1",
  lr_ind_year_b2 = "Litigation risk t-2",
  lr_ind_year_b3 = "Litigation risk t-3",
  n_participants_b1 = "N of shareholders t-1",
  n_participants_b2 = "N of shareholders t-2",
  n_participants_b3 = "N of shareholders t-3",
  family_element_b1 = "Family owned t-1",
  family_element_b2 = "Family owned t-2",
  family_element_b3 = "Family owned t-3",
  age_b2 = "Firm age t-2",
  age_b2_squared = "Firm age squared t-2",
  age_b3 = "Firm age t-3",
  age_b3_squared = "Firm age squared t-3",
  age_b4 = "Firm age t-4",
  age_b4_squared = "Firm age squared t-4",
  zero_interest_loan_b1 = "Zero interest loan t-1",
  zero_interest_loan_b2 = "Zero interest loan t-2",
  zero_interest_loan_b3 = "Zero interest loan t-3",
  paid_zero_interest_b1 = "Paid zero interest loan t-1",
  paid_zero_interest_b2 = "Paid zero interest loan t-2",
  paid_zero_interest_b3 = "Paid zero interest loan t-3",
  paid_dividends_b1 = "Paid dividends t-1",
  paid_dividends_b2 = "Paid dividends t-2",
  paid_dividends_b3 = "Paid dividends t-3",
  year = "Year",
  okved_2dig = "Industry",
  inn = "Firm",
  reset = TRUE
))

# PP t-1 =====

# Filter the data ====

llcs_panel <- llcs_panel_full[roa_b1 != 0 & !is.na(roa_b1) & roa_b2 != 0 & !is.na(roa_b2) & !is.na(roa_adj_hist_b2) & 
                                !is.na(market_share_b2) & !is.na(total_assets_b2) & !is.na(revenue_b2) & !is.na(lr_ind_year_b2) & !is.na(age_b2) & !is.na(family_element_b2) &
                                revenue_b2 > 0 &
                                !is.na(market_share_b1) & !is.na(total_assets_b1) & !is.na(revenue_b1) & !is.na(lr_ind_year_b1) & !is.na(age_b1) & !is.na(family_element_b1) &
                                revenue_b1 > 0 &
                                sole_shareholder_only == 0 & year >= 2014, ] 

cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

control_vars <- c("roa_b1", "roa_b2", "roa_adj_hist_b2", "market_share_b1", 
                  "total_assets_b1", "revenue_b1", "lr_ind_year_b1", 
                  "n_participants_b1", "family_element_b1", "zero_interest_loan_b1",
                  "paid_zero_interest_b1", "market_share_b2", "total_assets_b2", 
                  "revenue_b2", "age_b2", "age_b2_squared", "lr_ind_year_b2", 
                  "n_participants_b2", "family_element_b2", "zero_interest_loan_b2",
                  "paid_zero_interest_b2")

model_roa_any <- feols(
  create_formula("pp_case_any"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied <- feols(
  create_formula("pp_case_satisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_key <- feols(
  create_formula("pp_case_any_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_key <- feols(
  create_formula("pp_case_satisfied_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c("model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

models_list <- mget(models_names)

results_lag1 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_lag1$model <- forcats::fct_relevel(results_lag1$model, "model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

results_lag1$order <- "1"

results_lag1$n_cases_any <- llcs_panel[pp_case_any > 0, .N]
results_lag1$n_cases_satisfied <- llcs_panel[pp_case_satisfied > 0, .N]
results_lag1$n_cases_any_key <- llcs_panel[pp_case_any_key > 0, .N]
results_lag1$n_cases_satisfied_key <- llcs_panel[pp_case_satisfied_key > 0, .N]

# Save models to save the regression table lately
first_order_model_roa_any <- copy(model_roa_any)
first_order_model_roa_satisfied <- copy(model_roa_satisfied)
first_order_model_roa_any_key <- copy(model_roa_any_key)
first_order_model_roa_satisfied_key <- copy(model_roa_satisfied_key)

gc()

# PP t-2 ====

# Filter the data ====
llcs_panel <- llcs_panel_full[roa_b2 != 0 & !is.na(roa_b2) & roa_b3 != 0 & !is.na(roa_b3) & !is.na(roa_adj_hist_b3) & 
                                !is.na(market_share_b3) & !is.na(total_assets_b3) & !is.na(revenue_b3) & !is.na(lr_ind_year_b3) & !is.na(age_b3) & !is.na(family_element_b3) &
                                revenue_b3 > 0 &
                                !is.na(market_share_b2) & !is.na(total_assets_b2) & !is.na(revenue_b2) & !is.na(lr_ind_year_b2) & !is.na(age_b2) & !is.na(family_element_b2) &
                                revenue_b2 > 0 &
                                sole_shareholder_only == 0 & year >= 2014, ] 

cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

control_vars <- c("roa_b2", "roa_b3", "roa_adj_hist_b3", "market_share_b2", 
                  "total_assets_b2", "revenue_b2", "lr_ind_year_b2", 
                  "n_participants_b2", "family_element_b2", "zero_interest_loan_b2",
                  "paid_zero_interest_b2", "market_share_b3", "total_assets_b3", 
                  "revenue_b3", "age_b3", "age_b3_squared", "lr_ind_year_b3", 
                  "n_participants_b3", "family_element_b3", "zero_interest_loan_b3",
                  "paid_zero_interest_b3")

model_roa_any <- feols(
  create_formula("pp_case_any"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied <- feols(
  create_formula("pp_case_satisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_key <- feols(
  create_formula("pp_case_any_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_key <- feols(
  create_formula("pp_case_satisfied_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c("model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

models_list <- mget(models_names)

results_lag2 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_lag2$model <- forcats::fct_relevel(results_lag2$model, "model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

results_lag2$order <- "2"

results_lag2$n_cases_any <- llcs_panel[pp_case_any > 0, .N]
results_lag2$n_cases_satisfied <- llcs_panel[pp_case_satisfied > 0, .N]
results_lag2$n_cases_any_key <- llcs_panel[pp_case_any_key > 0, .N]
results_lag2$n_cases_satisfied_key <- llcs_panel[pp_case_satisfied_key > 0, .N]

# Save regression tables ====

etable(first_order_model_roa_any, first_order_model_roa_satisfied, first_order_model_roa_any_key, first_order_model_roa_satisfied_key, model_roa_any, model_roa_satisfied, model_roa_any_key, model_roa_satisfied_key,
       se.below	= TRUE,
       fitstat = c("n", "r2", "aic"),
       signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.10),
#       keep = "ROA",
       tex = TRUE,
       file = paste0("plots_tables/regression_tables/first_second_order_models.tex"))

etable(first_order_model_roa_any, first_order_model_roa_satisfied, first_order_model_roa_any_key, first_order_model_roa_satisfied_key, model_roa_any, model_roa_satisfied, model_roa_any_key, model_roa_satisfied_key,
       se.below	= TRUE,
       fitstat = c("n", "r2", "aic", "my"),
       signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.10),
       keep = "ROA")

# Merge and save the results ====

pp_models_results <- rbind(results_lag1, results_lag2)

fwrite(pp_models_results, paste0("plots_tables/estimation_results/disp_strain_pp_models_results_eight_transformed_both_predictors.csv"))

# Run analysis for unstandardized variables ====

llcs_panel <- llcs_panel_full[roa_b1 != 0 & !is.na(roa_b1) & roa_b2 != 0 & !is.na(roa_b2) & !is.na(roa_adj_hist_b2) & 
                                !is.na(market_share_b2) & !is.na(total_assets_b2) & !is.na(revenue_b2) & !is.na(lr_ind_year_b2) & !is.na(age_b2) & !is.na(family_element_b2) &
                                revenue_b2 > 0 &
                                !is.na(market_share_b1) & !is.na(total_assets_b1) & !is.na(revenue_b1) & !is.na(lr_ind_year_b1) & !is.na(age_b1) & !is.na(family_element_b1) &
                                revenue_b1 > 0 &
                                sole_shareholder_only == 0 & year >= 2014, ] 

# cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
# llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

control_vars <- c("roa_b1", "roa_b2", "roa_adj_hist_b2", "market_share_b1", 
                  "total_assets_b1", "revenue_b1", "lr_ind_year_b1", 
                  "n_participants_b1", "family_element_b1", "zero_interest_loan_b1",
                  "paid_zero_interest_b1", "market_share_b2", "total_assets_b2", 
                  "revenue_b2", "age_b2", "age_b2_squared", "lr_ind_year_b2", 
                  "n_participants_b2", "family_element_b2", "zero_interest_loan_b2",
                  "paid_zero_interest_b2")

model_roa_any <- feols(
  create_formula("pp_case_any"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied <- feols(
  create_formula("pp_case_satisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_key <- feols(
  create_formula("pp_case_any_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_key <- feols(
  create_formula("pp_case_satisfied_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c("model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

models_list <- mget(models_names)

results_lag1 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_lag1$model <- forcats::fct_relevel(results_lag1$model, "model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

results_lag1$order <- "1"

results_lag1$n_cases_any <- llcs_panel[pp_case_any > 0, .N]
results_lag1$n_cases_satisfied <- llcs_panel[pp_case_satisfied > 0, .N]
results_lag1$n_cases_any_key <- llcs_panel[pp_case_any_key > 0, .N]
results_lag1$n_cases_satisfied_key <- llcs_panel[pp_case_satisfied_key > 0, .N]

# Save models to save the regression table lately
first_order_model_roa_any <- copy(model_roa_any)
first_order_model_roa_satisfied <- copy(model_roa_satisfied)
first_order_model_roa_any_key <- copy(model_roa_any_key)
first_order_model_roa_satisfied_key <- copy(model_roa_satisfied_key)

gc()

# PP t-2 ====

# Filter the data ====
llcs_panel <- llcs_panel_full[roa_b2 != 0 & !is.na(roa_b2) & roa_b3 != 0 & !is.na(roa_b3) & !is.na(roa_adj_hist_b3) & 
                                !is.na(market_share_b3) & !is.na(total_assets_b3) & !is.na(revenue_b3) & !is.na(lr_ind_year_b3) & !is.na(age_b3) & !is.na(family_element_b3) &
                                revenue_b3 > 0 &
                                !is.na(market_share_b2) & !is.na(total_assets_b2) & !is.na(revenue_b2) & !is.na(lr_ind_year_b2) & !is.na(age_b2) & !is.na(family_element_b2) &
                                revenue_b2 > 0 &
                                sole_shareholder_only == 0 & year >= 2014, ] 

cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

control_vars <- c("roa_b2", "roa_b3", "roa_adj_hist_b3", "market_share_b2", 
                  "total_assets_b2", "revenue_b2", "lr_ind_year_b2", 
                  "n_participants_b2", "family_element_b2", "zero_interest_loan_b2",
                  "paid_zero_interest_b2", "market_share_b3", "total_assets_b3", 
                  "revenue_b3", "age_b3", "age_b3_squared", "lr_ind_year_b3", 
                  "n_participants_b3", "family_element_b3", "zero_interest_loan_b3",
                  "paid_zero_interest_b3")

model_roa_any <- feols(
  create_formula("pp_case_any"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied <- feols(
  create_formula("pp_case_satisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_key <- feols(
  create_formula("pp_case_any_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_key <- feols(
  create_formula("pp_case_satisfied_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c("model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

models_list <- mget(models_names)

results_lag2 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_lag2$model <- forcats::fct_relevel(results_lag2$model, "model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

results_lag2$order <- "2"

results_lag2$n_cases_any <- llcs_panel[pp_case_any > 0, .N]
results_lag2$n_cases_satisfied <- llcs_panel[pp_case_satisfied > 0, .N]
results_lag2$n_cases_any_key <- llcs_panel[pp_case_any_key > 0, .N]
results_lag2$n_cases_satisfied_key <- llcs_panel[pp_case_satisfied_key > 0, .N]

# Save regression tables ====

etable(first_order_model_roa_any, first_order_model_roa_satisfied, first_order_model_roa_any_key, first_order_model_roa_satisfied_key, model_roa_any, model_roa_satisfied, model_roa_any_key, model_roa_satisfied_key,
       se.below	= TRUE,
       fitstat = c("n", "r2", "aic", "my"),
       signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.10),
       #       keep = "ROA",
       tex = TRUE,
       file = paste0("plots_tables/regression_tables/first_second_order_models_unstandard.tex"))

etable(first_order_model_roa_any, first_order_model_roa_satisfied, first_order_model_roa_any_key, first_order_model_roa_satisfied_key, model_roa_any, model_roa_satisfied, model_roa_any_key, model_roa_satisfied_key,
       se.below	= TRUE,
       fitstat = c("n", "r2", "aic", "my"),
       signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.10),
       keep = "ROA")



