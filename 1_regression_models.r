library(data.table)
library(ggplot2)
library(fst)
library(fixest)
library(stringr); library(stringi)
library(marginaleffects)
library(modelsummary) 

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

path <- Sys.getenv("PUBLIC_FINANCIALS_PATH")
path_d <- Sys.getenv("DRIVE_D")
setwd(path)

# Load the data ====

llcs_panel_full <- read_fst("data/arbitrazh_cases/panel_disp_vs_strain.fst", as.data.table = TRUE)

# Transform the data ====

cols_to_transform <- grep("roa", colnames(llcs_panel_full), value = TRUE)
llcs_panel_full[, (cols_to_transform) := lapply(.SD, function (x) asinh(x)), .SDcols = cols_to_transform]

cols_to_log <- grep("market_share|total_assets|revenue|^age", colnames(llcs_panel_full))
llcs_panel_full[, (cols_to_log) := lapply(.SD, function (x) log(x)), .SDcols = cols_to_log]

setFixest_dict(c(
  pp_case_any = "Any claim",
  pp_case_satisfied = "Satisfied claim",
  pp_case_unsatisfied = "Unsatisfied claim",
  pp_case_any_noncriminal = "Any non-criminal claim",
  pp_case_satisfied_noncriminal = "Satisfied non-criminal claim",
  pp_case_unsatisfied_noncriminal = "Unsatisfied non-criminal claim",
  pp_case_any_criminal = "Any criminal claim",
  pp_case_satisfied_criminal = "Satisfied criminal claim",
  pp_case_unsatisfied_criminal = "Unsatisfied criminal claim",
  roa_b1 = "ROA t-1",
  roa_b2 = "ROA t-2",
  roa_b3 = "ROA t-3",
  roa_b4 = "ROA t-4",
  roa_adj_hist_b1 = "Historical ROA t-1",
  roa_adj_hist_b2 = "Historical ROA t-2",
  roa_adj_hist_b3 = "Historical ROA t-3",
  roa_adj_hist_b4 = "Historical ROA t-4",
  market_share_b1 = "Ln of market share t-1",
  market_share_b2 = "Ln of market share t-2",
  market_share_b3 = "Ln of market share t-3",
  market_share_b4 = "Ln of market share t-4",
  total_assets_b1 = "Ln of assets t-1",
  total_assets_b2 = "Ln of assets t-2",
  total_assets_b3 = "Ln of assets t-3",
  total_assets_b4 = "Ln of assets t-4",
  revenue_b1 = "Ln of revenue t-1",
  revenue_b2 = "Ln of revenue t-2",
  revenue_b3 = "Ln of revenue t-3",
  revenue_b4 = "Ln of revenue t-4",
  lr_ind_year_b1 = "Litigation risk t-1",
  lr_ind_year_b2 = "Litigation risk t-2",
  lr_ind_year_b3 = "Litigation risk t-3",
  lr_ind_year_b4 = "Litigation risk t-4",
  n_participants_b1 = "N of shareholders t-1",
  n_participants_b2 = "N of shareholders t-2",
  n_participants_b3 = "N of shareholders t-3",
  n_participants_b4 = "N of shareholders t-4",
  family_element_b1 = "Family owned t-1",
  family_element_b2 = "Family owned t-2",
  family_element_b3 = "Family owned t-3",
  family_element_b4 = "Family owned t-4",
  age = "Ln of firm age",
  age_b2 = "Ln of firm age t-2",
  age_b3 = "Ln of firm age t-3",
  age_b4 = "Ln of firm age t-4",
  zero_interest_loan_b1 = "Zero interest loan t-1",
  zero_interest_loan_b2 = "Zero interest loan t-2",
  zero_interest_loan_b3 = "Zero interest loan t-3",
  zero_interest_loan_b4 = "Zero interest loan t-4",
  paid_zero_interest_b1 = "Paid zero interest loan t-1",
  paid_zero_interest_b2 = "Paid zero interest loan t-2",
  paid_zero_interest_b3 = "Paid zero interest loan t-3",
  paid_zero_interest_b4 = "Paid zero interest loan t-4",
  paid_dividends_b1 = "Paid dividends t-1",
  paid_dividends_b2 = "Paid dividends t-2",
  paid_dividends_b3 = "Paid dividends t-3",
  paid_dividends_b4 = "Paid dividends t-4",
  leverage_b1 = "Leverage t-1",
  leverage_b2 = "Leverage t-2",
  leverage_b3 = "Leverage t-3",
  leverage_b4 = "Leverage t-4",
  year = "Year",
  okved_2dig = "Industry",
  inn = "Firm",
  reset = TRUE
))


var_labels <- c(
  pp_case_any = "Any claim",
  pp_case_satisfied = "Satisfied claim",
  pp_case_unsatisfied = "Unsatisfied claim",
  pp_case_any_noncriminal = "Any non-criminal claim",
  pp_case_satisfied_noncriminal = "Satisfied non-criminal claim",
  pp_case_unsatisfied_noncriminal = "Unsatisfied non-criminal claim",
  pp_case_any_criminal = "Any criminal claim",
  pp_case_satisfied_criminal = "Satisfied criminal claim",
  pp_case_unsatisfied_criminal = "Unsatisfied criminal claim",
  roa_b1 = "ROA t-1",
  roa_b2 = "ROA t-2",
  roa_b3 = "ROA t-3",
  roa_b4 = "ROA t-4",
  roa_adj_hist_b1 = "Historical ROA t-1",
  roa_adj_hist_b2 = "Historical ROA t-2",
  roa_adj_hist_b3 = "Historical ROA t-3",
  roa_adj_hist_b4 = "Historical ROA t-4",
  market_share_b1 = "Ln of market share t-1",
  market_share_b2 = "Ln of market share t-2",
  market_share_b3 = "Ln of market share t-3",
  market_share_b4 = "Ln of market share t-4",
  total_assets_b1 = "Ln of assets t-1",
  total_assets_b2 = "Ln of assets t-2",
  total_assets_b3 = "Ln of assets t-3",
  total_assets_b4 = "Ln of assets t-4",
  revenue_b1 = "Ln of revenue t-1",
  revenue_b2 = "Ln of revenue t-2",
  revenue_b3 = "Ln of revenue t-3",
  revenue_b4 = "Ln of revenue t-4",
  lr_ind_year_b1 = "Litigation risk t-1",
  lr_ind_year_b2 = "Litigation risk t-2",
  lr_ind_year_b3 = "Litigation risk t-3",
  lr_ind_year_b4 = "Litigation risk t-4",
  n_participants_b1 = "N of shareholders t-1",
  n_participants_b2 = "N of shareholders t-2",
  n_participants_b3 = "N of shareholders t-3",
  n_participants_b4 = "N of shareholders t-4",
  family_element_b1 = "Family owned t-1",
  family_element_b2 = "Family owned t-2",
  family_element_b3 = "Family owned t-3",
  family_element_b4 = "Family owned t-4",
  age = "Ln of firm age",
  age_b2 = "Ln of firm age t-2",
  age_b3 = "Ln of firm age t-3",
  age_b4 = "Ln of firm age t-4",
  zero_interest_loan_b1 = "Zero interest loan t-1",
  zero_interest_loan_b2 = "Zero interest loan t-2",
  zero_interest_loan_b3 = "Zero interest loan t-3",
  zero_interest_loan_b4 = "Zero interest loan t-4",
  paid_zero_interest_b1 = "Paid zero interest loan t-1",
  paid_zero_interest_b2 = "Paid zero interest loan t-2",
  paid_zero_interest_b3 = "Paid zero interest loan t-3",
  paid_zero_interest_b4 = "Paid zero interest loan t-4",
  paid_dividends_b1 = "Paid dividends t-1",
  paid_dividends_b2 = "Paid dividends t-2",
  paid_dividends_b3 = "Paid dividends t-3",
  paid_dividends_b4 = "Paid dividends t-4",
  leverage_b1 = "Leverage t-1",
  leverage_b2 = "Leverage t-2",
  leverage_b3 = "Leverage t-3",
  leverage_b4 = "Leverage t-4",
  year = "Year",
  okved_2dig = "Industry",
  inn = "Firm"
)

# Baseline models ====

## PP first order =====

llcs_panel <- llcs_panel_full[roa_b1 != 0 & !is.na(roa_b1) & roa_b2 != 0 & !is.na(roa_b2) & !is.na(roa_adj_hist_b2) & 
                                !is.na(market_share_b2) & !is.na(total_assets_b2) & !is.na(revenue_b2) & !is.na(age_b2) & !is.na(family_element_b2) &
                                revenue_b2 > 0 & !is.na(leverage_b2) & !is.infinite(leverage_b2) &
                                !is.na(market_share_b1) & !is.na(total_assets_b1) & !is.na(revenue_b1) & !is.na(age_b1) & !is.na(family_element_b1) &
                                revenue_b1 > 0 & !is.na(leverage_b1) & !is.infinite(leverage_b1) &
                                sole_shareholder_only == 0 & year >= 2014, ] 

cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

control_vars <- c("roa_b1", "roa_b2", "roa_adj_hist_b2", "market_share_b1", 
                  "total_assets_b1", "revenue_b1", 
                  "n_participants_b1", "family_element_b1", "zero_interest_loan_b1",
                  "paid_zero_interest_b1", "leverage_b1", "total_assets_b2", 
                  "revenue_b2", "age_b2",  
                  "n_participants_b2", "family_element_b2", "zero_interest_loan_b2",
                  "paid_zero_interest_b2", "leverage_b2")

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

model_roa_unsatisfied <- feols(
  create_formula("pp_case_unsatisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_noncriminal <- feols(
  create_formula("pp_case_any_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_noncriminal <- feols(
  create_formula("pp_case_satisfied_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied_noncriminal <- feols(
  create_formula("pp_case_unsatisfied_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_criminal <- feols(
  create_formula("pp_case_any_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_criminal <- feols(
  create_formula("pp_case_satisfied_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied_criminal <- feols(
  create_formula("pp_case_unsatisfied_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c("model_roa_any", "model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal", "model_roa_satisfied_noncriminal","model_roa_unsatisfied_noncriminal", "model_roa_any_criminal", "model_roa_satisfied_criminal", "model_roa_unsatisfied_criminal")

models_list <- mget(models_names)

results_lag1 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_lag1$model <- forcats::fct_relevel(results_lag1$model, c("model_roa_any", "model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal", "model_roa_satisfied_noncriminal","model_roa_unsatisfied_noncriminal", "model_roa_any_criminal", "model_roa_satisfied_criminal", "model_roa_unsatisfied_criminal"))

results_lag1$order <- "1"

results_lag1$appr_lag <- 0

results_lag1$n_cases_any <- llcs_panel[pp_case_any > 0, .N]
results_lag1$n_cases_satisfied <- llcs_panel[pp_case_satisfied > 0, .N]
results_lag1$n_cases_unsatisfied <- llcs_panel[pp_case_unsatisfied > 0, .N]
results_lag1$n_cases_any_noncriminal <- llcs_panel[pp_case_any_noncriminal > 0, .N]
results_lag1$n_cases_satisfied_noncriminal <- llcs_panel[pp_case_satisfied_noncriminal > 0, .N]
results_lag1$n_cases_unsatisfied_noncriminal <- llcs_panel[pp_case_unsatisfied_noncriminal > 0, .N]
results_lag1$n_cases_any_criminal <- llcs_panel[pp_case_any_criminal > 0, .N]
results_lag1$n_cases_satisfied_criminal <- llcs_panel[pp_case_satisfied_criminal > 0, .N]
results_lag1$n_cases_unsatisfied_criminal <- llcs_panel[pp_case_unsatisfied_criminal > 0, .N]

## Save models to save the regression table lately ====
first_order_model_roa_any <- copy(model_roa_any)
first_order_model_roa_satisfied <- copy(model_roa_satisfied)
first_order_model_roa_unsatisfied <- copy(model_roa_unsatisfied)
first_order_model_roa_any_noncriminal <- copy(model_roa_any_noncriminal)
first_order_model_roa_satisfied_noncriminal <- copy(model_roa_satisfied_noncriminal)
first_order_model_roa_unsatisfied_noncriminal <- copy(model_roa_unsatisfied_noncriminal)
first_order_model_roa_any_criminal <- copy(model_roa_any_criminal)
first_order_model_roa_satisfied_criminal <- copy(model_roa_satisfied_criminal)
first_order_model_roa_unsatisfied_criminal <- copy(model_roa_unsatisfied_criminal)

gc()

## PP second order ====

llcs_panel <- llcs_panel_full[roa_b2 != 0 & !is.na(roa_b2) & roa_b3 != 0 & !is.na(roa_b3) & !is.na(roa_adj_hist_b3) & 
                                !is.na(market_share_b3) & !is.na(total_assets_b3) & !is.na(revenue_b3)  & !is.na(age_b3) & !is.na(family_element_b3) &
                                revenue_b3 > 0 & !is.na(leverage_b3) & !is.infinite(leverage_b3) &
                                !is.na(market_share_b2) & !is.na(total_assets_b2) & !is.na(revenue_b2) & !is.na(age_b2) & !is.na(family_element_b2) &
                                revenue_b2 > 0 & !is.na(leverage_b2) & !is.infinite(leverage_b2) &
                                sole_shareholder_only == 0 & year >= 2014, ] 

cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

control_vars <- c("roa_b2", "roa_b3", "roa_adj_hist_b3", "market_share_b2", 
                  "total_assets_b2", "revenue_b2", 
                  "n_participants_b2", "family_element_b2", "zero_interest_loan_b2",
                  "paid_zero_interest_b2", "leverage_b2", "market_share_b3", "total_assets_b3", 
                  "revenue_b3", "age_b3",  
                  "n_participants_b3", "family_element_b3", "zero_interest_loan_b3",
                  "paid_zero_interest_b3", "leverage_b3")

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

model_roa_unsatisfied <- feols(
  create_formula("pp_case_unsatisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_noncriminal <- feols(
  create_formula("pp_case_any_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_noncriminal <- feols(
  create_formula("pp_case_satisfied_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied_noncriminal <- feols(
  create_formula("pp_case_unsatisfied_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_criminal <- feols(
  create_formula("pp_case_any_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_criminal <- feols(
  create_formula("pp_case_satisfied_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied_criminal <- feols(
  create_formula("pp_case_unsatisfied_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c(c("model_roa_any", "model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal", "model_roa_satisfied_noncriminal","model_roa_unsatisfied_noncriminal", "model_roa_any_criminal", "model_roa_satisfied_criminal", "model_roa_unsatisfied_criminal"))

models_list <- mget(models_names)

results_lag2 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_lag2$model <- forcats::fct_relevel(results_lag2$model, c("model_roa_any", "model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal", "model_roa_satisfied_noncriminal","model_roa_unsatisfied_noncriminal", "model_roa_any_criminal", "model_roa_satisfied_criminal", "model_roa_unsatisfied_criminal"))

results_lag2$order <- "2"

results_lag2$appr_lag <- 0

results_lag2$n_cases_any <- llcs_panel[pp_case_any > 0, .N]
results_lag2$n_cases_satisfied <- llcs_panel[pp_case_satisfied > 0, .N]
results_lag2$n_cases_unsatisfied <- llcs_panel[pp_case_unsatisfied > 0, .N]
results_lag2$n_cases_any_noncriminal <- llcs_panel[pp_case_any_noncriminal > 0, .N]
results_lag2$n_cases_satisfied_noncriminal <- llcs_panel[pp_case_satisfied_noncriminal > 0, .N]
results_lag2$n_cases_unsatisfied_noncriminal <- llcs_panel[pp_case_unsatisfied_noncriminal > 0, .N]
results_lag2$n_cases_any_criminal <- llcs_panel[pp_case_any_criminal > 0, .N]
results_lag2$n_cases_satisfied_criminal <- llcs_panel[pp_case_satisfied_criminal > 0, .N]
results_lag2$n_cases_unsatisfied_criminal <- llcs_panel[pp_case_unsatisfied_criminal > 0, .N]

## Save models to save the regression table lately ====
second_order_model_roa_any <- copy(model_roa_any)
second_order_model_roa_satisfied <- copy(model_roa_satisfied)
second_order_model_roa_unsatisfied <- copy(model_roa_unsatisfied)
second_order_model_roa_any_noncriminal <- copy(model_roa_any_noncriminal)
second_order_model_roa_satisfied_noncriminal <- copy(model_roa_satisfied_noncriminal)
second_order_model_roa_unsatisfied_noncriminal <- copy(model_roa_unsatisfied_noncriminal)
second_order_model_roa_any_criminal <- copy(model_roa_any_criminal)
second_order_model_roa_satisfied_criminal <- copy(model_roa_satisfied_criminal)
second_order_model_roa_unsatisfied_criminal <- copy(model_roa_unsatisfied_criminal)

## PP third order ====

llcs_panel <- llcs_panel_full[roa_b3 != 0 & !is.na(roa_b3) & roa_b4 != 0 & !is.na(roa_b4) & !is.na(roa_adj_hist_b4) & 
                                !is.na(market_share_b4) & !is.na(total_assets_b4) & !is.na(revenue_b4)  & !is.na(age_b4) & !is.na(family_element_b4) &
                                revenue_b4 > 0 & !is.na(leverage_b4) & !is.infinite(leverage_b4) &
                                !is.na(market_share_b3) & !is.na(total_assets_b3) & !is.na(revenue_b3) & !is.na(age_b3) & !is.na(family_element_b3) &
                                revenue_b3 > 0 & !is.na(leverage_b3) & !is.infinite(leverage_b3) &
                                sole_shareholder_only == 0 & year >= 2014, ] 

cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

control_vars <- c("roa_b3", "roa_b4", "roa_adj_hist_b4", "market_share_b3", 
                  "total_assets_b3", "revenue_b3",  
                  "n_participants_b3", "family_element_b3", "zero_interest_loan_b3",
                  "paid_zero_interest_b3", "leverage_b3", "market_share_b4", "total_assets_b4", 
                  "revenue_b4", "age_b4", 
                  "n_participants_b4", "family_element_b4", "zero_interest_loan_b4",
                  "paid_zero_interest_b4", "leverage_b4")

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

model_roa_unsatisfied <- feols(
  create_formula("pp_case_unsatisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_noncriminal <- feols(
  create_formula("pp_case_any_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_noncriminal <- feols(
  create_formula("pp_case_satisfied_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied_noncriminal <- feols(
  create_formula("pp_case_unsatisfied_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_criminal <- feols(
  create_formula("pp_case_any_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_criminal <- feols(
  create_formula("pp_case_satisfied_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied_criminal <- feols(
  create_formula("pp_case_unsatisfied_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c(c("model_roa_any", "model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal", "model_roa_satisfied_noncriminal","model_roa_unsatisfied_noncriminal", "model_roa_any_criminal", "model_roa_satisfied_criminal", "model_roa_unsatisfied_criminal"))

models_list <- mget(models_names)

results_lag3 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_lag3$model <- forcats::fct_relevel(results_lag3$model, c("model_roa_any", "model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal", "model_roa_satisfied_noncriminal","model_roa_unsatisfied_noncriminal", "model_roa_any_criminal", "model_roa_satisfied_criminal", "model_roa_unsatisfied_criminal"))

results_lag3$order <- "3"

results_lag3$appr_lag <- 0

results_lag3$n_cases_any <- llcs_panel[pp_case_any > 0, .N]
results_lag3$n_cases_satisfied <- llcs_panel[pp_case_satisfied > 0, .N]
results_lag3$n_cases_unsatisfied <- llcs_panel[pp_case_unsatisfied > 0, .N]
results_lag3$n_cases_any_noncriminal <- llcs_panel[pp_case_any_noncriminal > 0, .N]
results_lag3$n_cases_satisfied_noncriminal <- llcs_panel[pp_case_satisfied_noncriminal > 0, .N]
results_lag3$n_cases_unsatisfied_noncriminal <- llcs_panel[pp_case_unsatisfied_noncriminal > 0, .N]
results_lag3$n_cases_any_criminal <- llcs_panel[pp_case_any_criminal > 0, .N]
results_lag3$n_cases_satisfied_criminal <- llcs_panel[pp_case_satisfied_criminal > 0, .N]
results_lag3$n_cases_unsatisfied_criminal <- llcs_panel[pp_case_unsatisfied_criminal > 0, .N]

# Save baseline regression tables ====

etable(first_order_model_roa_any, first_order_model_roa_satisfied, first_order_model_roa_unsatisfied, first_order_model_roa_any_noncriminal, first_order_model_roa_any_criminal, first_order_model_roa_satisfied_criminal, first_order_model_roa_unsatisfied_criminal, 
       second_order_model_roa_any, second_order_model_roa_satisfied, second_order_model_roa_unsatisfied, second_order_model_roa_any_noncriminal, second_order_model_roa_any_criminal, second_order_model_roa_satisfied_criminal, second_order_model_roa_unsatisfied_criminal, 
       model_roa_any, model_roa_satisfied, model_roa_unsatisfied, model_roa_any_noncriminal, model_roa_any_criminal, model_roa_satisfied_criminal, model_roa_unsatisfied_criminal,
       se.below	= TRUE,
       fitstat = c("n", "r2", "aic"),
       signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.10),
       #       keep = "ROA",
       tex = TRUE,
       file = file.path(
         path_d,
        "plots_tables",
        "regression_tables",
        "first_second_third_order_models.tex"
  ))



# Does not work on jetty; use stt
modelsummary(
  list(
    "2nd order: Satisfied"  = second_order_model_roa_satisfied,
    "2nd order: Unsatisfied" = second_order_model_roa_unsatisfied,
    
    "2nd order: Criminal" = second_order_model_roa_any_criminal,
    "2nd order: Non-criminal" = second_order_model_roa_any_noncriminal
  ),
  stars = TRUE,
  coef_rename = var_labels,
  output = file.path(
    path_d,
    "plots_tables",
    "regression_tables",
    "second_order_models.docx"
  ))

modelsummary(
  list(
    "1nd order: Satisfied"  = first_order_model_roa_satisfied,
    "1nd order: Unsatisfied" = first_order_model_roa_unsatisfied,
    
    "1nd order: Criminal" = first_order_model_roa_any_criminal,
    "1nd order: Non-criminal" = first_order_model_roa_any_noncriminal,
    
    "2nd order: Satisfied"  = second_order_model_roa_satisfied,
    "2nd order: Unsatisfied" = second_order_model_roa_unsatisfied,
    
    "2nd order: Criminal" = second_order_model_roa_any_criminal,
    "2nd order: Non-criminal" = second_order_model_roa_any_noncriminal,
    
    "3rd order: Satisfied"  = model_roa_satisfied,
    "3rd order: Unsatisfied" = model_roa_unsatisfied,
    
    "3rd order: Criminal" = model_roa_any_criminal,
    "3rd order: Non-criminal" = model_roa_any_noncriminal
  ),
  stars = TRUE,
  coef_rename = var_labels,
  output = file.path(
    path_d,
    "plots_tables",
    "regression_tables",
    "first_second_third_order_models.docx"
  ))

# Merge and save the results ====

pp_models_results <- rbind(results_lag1, results_lag2, results_lag3)

gc()

# Adjust for limitation periods =====

two_years_limperiod_topics <- c("invalid_Transaction_Transaction", "damages", "expulsion") # I assume that violations are exposed during review of financials statements
one_year_limperiod_topics <- c("shareholding")
three_months_limperiod_topics <- c("value", "documents_llc", "documents_director", "director", "invalid_Transaction_Decision", "invalid_Decision_other", "miscellaneous", "dividends")

assign_lagged_vars <- function(dt, cols, short_lag = 1, medium_lag = 2, long_lag = 3, default_lag = 1, topics_three_years, topics_one_year, topics_three_months){
  
  long_suffix <- paste0("_b", long_lag)
  medium_suffix <- paste0("_b", medium_lag)
  short_suffix <- paste0("_b", short_lag)
  default_suffix <- paste0("_b", default_lag)
  
  for (col in cols) {
    
    new_column <- paste0(col, "_b_appr")
    col_long_lag <- paste0(col, long_suffix)
    col_medium_lag <- paste0(col, medium_suffix)
    col_short_lag <- paste0(col, short_suffix)
    col_default <- paste0(col, default_suffix)
    
    dt[, (new_column) := fcase( 
      first_topic_satisfied_pp %chin% topics_three_years, get(col_long_lag),
      first_topic_satisfied_pp %chin% topics_one_year, get(col_medium_lag),
      first_topic_satisfied_pp %chin% topics_three_months, get(col_short_lag),
      default = get(col_default)
    )]
  }
  
  return(dt)
  
}

## PP first order ====

cols_to_lag <- c("roa", "roa_adj_hist", "market_share", "total_assets", "revenue",  "n_participants", "family_element", "zero_interest_loan", "paid_zero_interest", "leverage")

llcs_panel <- assign_lagged_vars(
  dt = llcs_panel_full, 
  cols = cols_to_lag, 
  short_lag = 1, 
  medium_lag = 2,
  long_lag = 3, 
  default_lag = 1, 
  topics_three_years = two_years_limperiod_topics, 
  topics_one_year = one_year_limperiod_topics,
  topics_three_months = three_months_limperiod_topics)

setorderv(llcs_panel, c("inn", "year"), c(1, 1))
llcs_panel[, inn_b1 := shift(inn, type = "lag", n = 1)]
cols_to_lag <- paste0(cols_to_lag, "_b_appr")
llcs_panel[, paste0(cols_to_lag, "_b1") := lapply(.SD, function(x) shift(x, type = "lag", n = 1)), .SDcols = cols_to_lag]
llcs_panel[inn != inn_b1, paste0(cols_to_lag, "_b1") := NA]

llcs_panel <- llcs_panel[roa_b_appr != 0 & !is.na(roa_b_appr) & roa_b_appr_b1 != 0 & !is.na(roa_b_appr_b1) & !is.na(roa_adj_hist_b_appr_b1) & 
                                !is.na(market_share_b_appr) & !is.na(total_assets_b_appr) & !is.na(revenue_b_appr)  & !is.na(age) & !is.na(family_element_b_appr) & !is.na(leverage_b_appr) & !is.infinite(leverage_b_appr) &
                                !is.na(market_share_b_appr_b1)  & !is.na(revenue_b_appr_b1)  & !is.na(age) & !is.na(family_element_b_appr_b1) & !is.na(leverage_b_appr_b1) & !is.infinite(leverage_b_appr_b1) &
                                sole_shareholder_only == 0 & year >= 2014, ] 

cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

control_vars <- c("roa_b_appr", "roa_b_appr_b1", "roa_adj_hist_b_appr_b1", "market_share_b_appr", "total_assets_b_appr", "revenue_b_appr",  "age", "family_element_b_appr", "leverage",
                  "market_share_b_appr_b1", "total_assets_b_appr_b1", "revenue_b_appr_b1", "family_element_b_appr_b1", "leverage_b_appr_b1")

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

model_roa_unsatisfied <- feols(
  create_formula("pp_case_unsatisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_noncriminal <- feols(
  create_formula("pp_case_any_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_noncriminal <- feols(
  create_formula("pp_case_satisfied_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied_noncriminal <- feols(
  create_formula("pp_case_unsatisfied_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_criminal <- feols(
  create_formula("pp_case_any_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_criminal <- feols(
  create_formula("pp_case_satisfied_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied_criminal <- feols(
  create_formula("pp_case_unsatisfied_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)


models_names <- c(c("model_roa_any", "model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal", "model_roa_satisfied_noncriminal","model_roa_unsatisfied_noncriminal", "model_roa_any_criminal", "model_roa_satisfied_criminal", "model_roa_unsatisfied_criminal"))

models_list <- mget(models_names)

results_lagappr1 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_lagappr1$model <- forcats::fct_relevel(results_lagappr1$model, c("model_roa_any", "model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal", "model_roa_satisfied_noncriminal","model_roa_unsatisfied_noncriminal", "model_roa_any_criminal", "model_roa_satisfied_criminal", "model_roa_unsatisfied_criminal"))

results_lagappr1$order <- "1"

results_lagappr1$appr_lag <- 1

results_lagappr1$n_cases_any <- llcs_panel[pp_case_any > 0, .N]
results_lagappr1$n_cases_satisfied <- llcs_panel[pp_case_satisfied > 0, .N]
results_lagappr1$n_cases_unsatisfied <- llcs_panel[pp_case_unsatisfied > 0, .N]
results_lagappr1$n_cases_any_noncriminal <- llcs_panel[pp_case_any_noncriminal > 0, .N]
results_lagappr1$n_cases_satisfied_noncriminal <- llcs_panel[pp_case_satisfied_noncriminal > 0, .N]
results_lagappr1$n_cases_unsatisfied_noncriminal <- llcs_panel[pp_case_unsatisfied_noncriminal > 0, .N]
results_lagappr1$n_cases_any_criminal <- llcs_panel[pp_case_any_criminal > 0, .N]
results_lagappr1$n_cases_satisfied_criminal <- llcs_panel[pp_case_satisfied_criminal > 0, .N]
results_lagappr1$n_cases_unsatisfied_criminal <- llcs_panel[pp_case_unsatisfied_criminal > 0, .N]

pp_models_results <- rbind(pp_models_results, results_lagappr1, fill = TRUE)

gc()


## PP second order ====

cols_to_lag <- c("roa", "roa_adj_hist", "market_share", "total_assets", "revenue", "n_participants", "family_element", "zero_interest_loan", "paid_zero_interest", "leverage")

llcs_panel <- assign_lagged_vars(
  dt = llcs_panel_full, 
  cols = cols_to_lag, 
  short_lag = 2, 
  medium_lag = 3,
  long_lag = 4, 
  default_lag = 2, 
  topics_three_years = two_years_limperiod_topics, 
  topics_one_year = one_year_limperiod_topics,
  topics_three_months = three_months_limperiod_topics)

setorderv(llcs_panel, c("inn", "year"), c(1, 1))
llcs_panel[, inn_b1 := shift(inn, type = "lag", n = 1)]
cols_to_lag <- paste0(cols_to_lag, "_b_appr")
llcs_panel[, paste0(cols_to_lag, "_b1") := lapply(.SD, function(x) shift(x, type = "lag", n = 1)), .SDcols = cols_to_lag]
llcs_panel[inn != inn_b1, paste0(cols_to_lag, "_b1") := NA]

llcs_panel <- llcs_panel[roa_b_appr != 0 & !is.na(roa_b_appr) & roa_b_appr_b1 != 0 & !is.na(roa_b_appr_b1) & !is.na(roa_adj_hist_b_appr_b1) & 
                                !is.na(market_share_b_appr) & !is.na(total_assets_b_appr) & !is.na(revenue_b_appr)  & !is.na(age) & !is.na(family_element_b_appr) & !is.na(leverage_b_appr) & !is.infinite(leverage_b_appr) &
                                !is.na(market_share_b_appr_b1) & !is.na(total_assets_b_appr_b1) & !is.na(revenue_b_appr_b1)  & !is.na(age) & !is.na(family_element_b_appr_b1) & !is.na(leverage_b_appr_b1) & !is.infinite(leverage_b_appr_b1) &
                                sole_shareholder_only == 0 & year >= 2014, ] 

cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

control_vars <- c("roa_b_appr", "roa_b_appr_b1", "roa_adj_hist_b_appr_b1", "market_share_b_appr", "total_assets_b_appr", "revenue_b_appr",  "age", "family_element_b_appr", "leverage",
                  "market_share_b_appr_b1", "total_assets_b_appr_b1", "revenue_b_appr_b1",  "family_element_b_appr_b1", "leverage_b_appr_b1")

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

model_roa_unsatisfied <- feols(
  create_formula("pp_case_unsatisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_noncriminal <- feols(
  create_formula("pp_case_any_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_noncriminal <- feols(
  create_formula("pp_case_satisfied_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied_noncriminal <- feols(
  create_formula("pp_case_unsatisfied_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_criminal <- feols(
  create_formula("pp_case_any_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_criminal <- feols(
  create_formula("pp_case_satisfied_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied_criminal <- feols(
  create_formula("pp_case_unsatisfied_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c(c("model_roa_any", "model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal", "model_roa_satisfied_noncriminal","model_roa_unsatisfied_noncriminal", "model_roa_any_criminal", "model_roa_satisfied_criminal", "model_roa_unsatisfied_criminal"))

models_list <- mget(models_names)

results_lagappr2 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_lagappr2$model <- forcats::fct_relevel(results_lagappr2$model, c("model_roa_any", "model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal", "model_roa_satisfied_noncriminal","model_roa_unsatisfied_noncriminal", "model_roa_any_criminal", "model_roa_satisfied_criminal", "model_roa_unsatisfied_criminal"))

results_lagappr2$order <- "2"

results_lagappr2$appr_lag <- 1

results_lagappr2$n_cases_any <- llcs_panel[pp_case_any > 0, .N]
results_lagappr2$n_cases_satisfied <- llcs_panel[pp_case_satisfied > 0, .N]
results_lagappr2$n_cases_unsatisfied <- llcs_panel[pp_case_unsatisfied > 0, .N]
results_lagappr2$n_cases_any_noncriminal <- llcs_panel[pp_case_any_noncriminal > 0, .N]
results_lagappr2$n_cases_satisfied_noncriminal <- llcs_panel[pp_case_satisfied_noncriminal > 0, .N]
results_lagappr2$n_cases_unsatisfied_noncriminal <- llcs_panel[pp_case_unsatisfied_noncriminal > 0, .N]
results_lagappr2$n_cases_any_criminal <- llcs_panel[pp_case_any_criminal > 0, .N]
results_lagappr2$n_cases_satisfied_criminal <- llcs_panel[pp_case_satisfied_criminal > 0, .N]
results_lagappr2$n_cases_unsatisfied_criminal <- llcs_panel[pp_case_unsatisfied_criminal > 0, .N]

pp_models_results <- rbind(pp_models_results, results_lagappr2, fill = TRUE)

gc()

## PP third order ====

cols_to_lag <- c("roa", "roa_adj_hist", "market_share", "total_assets", "revenue", "n_participants", "family_element", "zero_interest_loan", "paid_zero_interest", "leverage")

llcs_panel <- assign_lagged_vars(
  dt = llcs_panel_full, 
  cols = cols_to_lag, 
  short_lag = 3, 
  medium_lag = 4,
  long_lag = 5, 
  default_lag = 3, 
  topics_three_years = two_years_limperiod_topics, 
  topics_one_year = one_year_limperiod_topics,
  topics_three_months = three_months_limperiod_topics)

setorderv(llcs_panel, c("inn", "year"), c(1, 1))
llcs_panel[, inn_b1 := shift(inn, type = "lag", n = 1)]
cols_to_lag <- paste0(cols_to_lag, "_b_appr")
llcs_panel[, paste0(cols_to_lag, "_b1") := lapply(.SD, function(x) shift(x, type = "lag", n = 1)), .SDcols = cols_to_lag]
llcs_panel[inn != inn_b1, paste0(cols_to_lag, "_b1") := NA]

llcs_panel <- llcs_panel[roa_b_appr != 0 & !is.na(roa_b_appr) & roa_b_appr_b1 != 0 & !is.na(roa_b_appr_b1) & !is.na(roa_adj_hist_b_appr_b1) & 
                                !is.na(market_share_b_appr) & !is.na(total_assets_b_appr) & !is.na(revenue_b_appr)  & !is.na(age) & !is.na(family_element_b_appr) & !is.na(leverage_b_appr) & !is.infinite(leverage_b_appr) &
                                !is.na(market_share_b_appr_b1) & !is.na(total_assets_b_appr_b1) & !is.na(revenue_b_appr_b1)  & !is.na(age) & !is.na(family_element_b_appr_b1) & !is.na(leverage_b_appr_b1) & !is.infinite(leverage_b_appr_b1) &
                                sole_shareholder_only == 0 & year >= 2014, ] 

cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

control_vars <- c("roa_b_appr", "roa_b_appr_b1", "roa_adj_hist_b_appr_b1", "market_share_b_appr", "total_assets_b_appr", "revenue_b_appr",  "age", "family_element_b_appr", "leverage",
                  "market_share_b_appr_b1", "total_assets_b_appr_b1", "revenue_b_appr_b1", "family_element_b_appr_b1", "leverage_b_appr_b1")

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

model_roa_unsatisfied <- feols(
  create_formula("pp_case_unsatisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_noncriminal <- feols(
  create_formula("pp_case_any_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_noncriminal <- feols(
  create_formula("pp_case_satisfied_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied_noncriminal <- feols(
  create_formula("pp_case_unsatisfied_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_criminal <- feols(
  create_formula("pp_case_any_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_criminal <- feols(
  create_formula("pp_case_satisfied_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied_criminal <- feols(
  create_formula("pp_case_unsatisfied_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c(c("model_roa_any", "model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal", "model_roa_satisfied_noncriminal","model_roa_unsatisfied_noncriminal", "model_roa_any_criminal", "model_roa_satisfied_criminal", "model_roa_unsatisfied_criminal"))

models_list <- mget(models_names)

results_lagappr3 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_lagappr3$model <- forcats::fct_relevel(results_lagappr3$model, c("model_roa_any", "model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal", "model_roa_satisfied_noncriminal","model_roa_unsatisfied_noncriminal", "model_roa_any_criminal", "model_roa_satisfied_criminal", "model_roa_unsatisfied_criminal"))

results_lagappr3$order <- "3"

results_lagappr3$appr_lag <- 1

results_lagappr3$n_cases_any <- llcs_panel[pp_case_any > 0, .N]
results_lagappr3$n_cases_satisfied <- llcs_panel[pp_case_satisfied > 0, .N]
results_lagappr3$n_cases_unsatisfied <- llcs_panel[pp_case_unsatisfied > 0, .N]
results_lagappr3$n_cases_any_noncriminal <- llcs_panel[pp_case_any_noncriminal > 0, .N]
results_lagappr3$n_cases_satisfied_noncriminal <- llcs_panel[pp_case_satisfied_noncriminal > 0, .N]
results_lagappr3$n_cases_unsatisfied_noncriminal <- llcs_panel[pp_case_unsatisfied_noncriminal > 0, .N]
results_lagappr3$n_cases_any_criminal <- llcs_panel[pp_case_any_criminal > 0, .N]
results_lagappr3$n_cases_satisfied_criminal <- llcs_panel[pp_case_satisfied_criminal > 0, .N]
results_lagappr3$n_cases_unsatisfied_criminal <- llcs_panel[pp_case_unsatisfied_criminal > 0, .N]

pp_models_results <- rbind(pp_models_results, results_lagappr3, fill = TRUE)

gc()

fwrite(pp_models_results, file.path(
         path_d,
        "plots_tables",
        "estimation_results",
        "pp_models_both_predictors.csv"
  ))



# Run analysis for unstandardized variables ====

## PP first order ====

llcs_panel <- llcs_panel_full[roa_b1 != 0 & !is.na(roa_b1) & roa_b2 != 0 & !is.na(roa_b2) & !is.na(roa_adj_hist_b2) & 
                                !is.na(market_share_b2) & !is.na(total_assets_b2) & !is.na(revenue_b2)  & !is.na(age_b2) & !is.na(family_element_b2) &
                                revenue_b2 > 0 & !is.na(leverage_b2) & !is.infinite(leverage_b2) &
                                !is.na(market_share_b1) & !is.na(total_assets_b1) & !is.na(revenue_b1) & !is.na(age_b1) & !is.na(family_element_b1) &
                                revenue_b1 > 0 & !is.na(leverage_b1) & !is.infinite(leverage_b1) &
                                sole_shareholder_only == 0 & year >= 2014, ] 

# cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
# llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

control_vars <- c("roa_b1", "roa_b2", "roa_adj_hist_b2", "market_share_b1", 
                  "total_assets_b1", "revenue_b1", 
                  "n_participants_b1", "family_element_b1", "zero_interest_loan_b1",
                  "paid_zero_interest_b1", "leverage_b1", "total_assets_b2", 
                  "revenue_b2", "age_b2", 
                  "n_participants_b2", "family_element_b2", "zero_interest_loan_b2",
                  "paid_zero_interest_b2", "leverage_b2")

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

model_roa_unsatisfied <- feols(
  create_formula("pp_case_unsatisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_noncriminal <- feols(
  create_formula("pp_case_any_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_noncriminal <- feols(
  create_formula("pp_case_satisfied_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied_noncriminal <- feols(
  create_formula("pp_case_unsatisfied_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_criminal <- feols(
  create_formula("pp_case_any_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_criminal <- feols(
  create_formula("pp_case_satisfied_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied_criminal <- feols(
  create_formula("pp_case_unsatisfied_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c(c("model_roa_any", "model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal", "model_roa_satisfied_noncriminal","model_roa_unsatisfied_noncriminal", "model_roa_any_criminal", "model_roa_satisfied_criminal", "model_roa_unsatisfied_criminal"))

models_list <- mget(models_names)

results_lag1 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_lag1$model <- forcats::fct_relevel(results_lag1$model, c("model_roa_any", "model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal", "model_roa_satisfied_noncriminal","model_roa_unsatisfied_noncriminal", "model_roa_any_criminal", "model_roa_satisfied_criminal", "model_roa_unsatisfied_criminal"))

results_lag1$order <- "1"

results_lag1$n_cases_any <- llcs_panel[pp_case_any > 0, .N]
results_lag1$n_cases_satisfied <- llcs_panel[pp_case_satisfied > 0, .N]
results_lag1$n_cases_any_noncriminal <- llcs_panel[pp_case_any_noncriminal > 0, .N]
results_lag1$n_cases_satisfied_noncriminal <- llcs_panel[pp_case_satisfied_noncriminal > 0, .N]

## Save models to save the regression table lately ====
first_order_model_roa_any <- copy(model_roa_any)
first_order_model_roa_satisfied <- copy(model_roa_satisfied)
first_order_model_roa_unsatisfied <- copy(model_roa_unsatisfied)
first_order_model_roa_any_noncriminal <- copy(model_roa_any_noncriminal)
first_order_model_roa_satisfied_noncriminal <- copy(model_roa_satisfied_noncriminal)
first_order_model_roa_unsatisfied_noncriminal <- copy(model_roa_unsatisfied_noncriminal)
first_order_model_roa_any_criminal <- copy(model_roa_any_criminal)
first_order_model_roa_satisfied_criminal <- copy(model_roa_satisfied_criminal)
first_order_model_roa_unsatisfied_criminal <- copy(model_roa_unsatisfied_criminal)

gc()

## PP second order ====

llcs_panel <- llcs_panel_full[roa_b2 != 0 & !is.na(roa_b2) & roa_b3 != 0 & !is.na(roa_b3) & !is.na(roa_adj_hist_b3) & 
                                !is.na(market_share_b3) & !is.na(total_assets_b3) & !is.na(revenue_b3)  & !is.na(age_b3) & !is.na(family_element_b3) &
                                revenue_b3 > 0 & !is.na(leverage_b3) & !is.infinite(leverage_b3) &
                                !is.na(market_share_b2) & !is.na(total_assets_b2) & !is.na(revenue_b2) & !is.na(age_b2) & !is.na(family_element_b2) &
                                revenue_b2 > 0 & !is.na(leverage_b2) & !is.infinite(leverage_b2) &
                                sole_shareholder_only == 0 & year >= 2014, ] 

# cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
# llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

control_vars <- c("roa_b2", "roa_b3", "roa_adj_hist_b3", "market_share_b2", 
                  "total_assets_b2", "revenue_b2", 
                  "n_participants_b2", "family_element_b2", "zero_interest_loan_b2",
                  "paid_zero_interest_b2", "leverage_b2", "total_assets_b3", 
                  "revenue_b3", "age_b3", 
                  "n_participants_b3", "family_element_b3", "zero_interest_loan_b3",
                  "paid_zero_interest_b3", "leverage_b3")

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

model_roa_unsatisfied <- feols(
  create_formula("pp_case_unsatisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_noncriminal <- feols(
  create_formula("pp_case_any_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_noncriminal <- feols(
  create_formula("pp_case_satisfied_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied_noncriminal <- feols(
  create_formula("pp_case_unsatisfied_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_criminal <- feols(
  create_formula("pp_case_any_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_criminal <- feols(
  create_formula("pp_case_satisfied_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied_criminal <- feols(
  create_formula("pp_case_unsatisfied_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c(c("model_roa_any", "model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal", "model_roa_satisfied_noncriminal","model_roa_unsatisfied_noncriminal", "model_roa_any_criminal", "model_roa_satisfied_criminal", "model_roa_unsatisfied_criminal"))

models_list <- mget(models_names)

results_lag2 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_lag2$model <- forcats::fct_relevel(results_lag2$model, c("model_roa_any", "model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal", "model_roa_satisfied_noncriminal","model_roa_unsatisfied_noncriminal", "model_roa_any_criminal", "model_roa_satisfied_criminal", "model_roa_unsatisfied_criminal"))

results_lag2$order <- "2"

results_lag2$n_cases_any <- llcs_panel[pp_case_any > 0, .N]
results_lag2$n_cases_satisfied <- llcs_panel[pp_case_satisfied > 0, .N]
results_lag2$n_cases_any_noncriminal <- llcs_panel[pp_case_any_noncriminal > 0, .N]
results_lag2$n_cases_satisfied_noncriminal <- llcs_panel[pp_case_satisfied_noncriminal > 0, .N]

## Save models to save the regression table lately ====
second_order_model_roa_any <- copy(model_roa_any)
second_order_model_roa_satisfied <- copy(model_roa_satisfied)
second_order_model_roa_unsatisfied <- copy(model_roa_unsatisfied)
second_order_model_roa_any_noncriminal <- copy(model_roa_any_noncriminal)
second_order_model_roa_satisfied_noncriminal <- copy(model_roa_satisfied_noncriminal)
second_order_model_roa_unsatisfied_noncriminal <- copy(model_roa_unsatisfied_noncriminal)
second_order_model_roa_any_criminal <- copy(model_roa_any_criminal)
second_order_model_roa_satisfied_criminal <- copy(model_roa_satisfied_criminal)
second_order_model_roa_unsatisfied_criminal <- copy(model_roa_unsatisfied_criminal)

## PP third order ====

llcs_panel <- llcs_panel_full[roa_b3 != 0 & !is.na(roa_b3) & roa_b4 != 0 & !is.na(roa_b4) & !is.na(roa_adj_hist_b4) & 
                                !is.na(market_share_b4) & !is.na(total_assets_b4) & !is.na(revenue_b4)  & !is.na(age_b4) & !is.na(family_element_b4) &
                                revenue_b4 > 0 & !is.na(leverage_b4) & !is.infinite(leverage_b4) &
                                !is.na(market_share_b3) & !is.na(total_assets_b3) & !is.na(revenue_b3) & !is.na(age_b3) & !is.na(family_element_b3) &
                                revenue_b3 > 0 & !is.na(leverage_b3) & !is.infinite(leverage_b3) &
                                sole_shareholder_only == 0 & year >= 2014, ] 

# cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
# llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

control_vars <- c("roa_b3", "roa_b4", "roa_adj_hist_b4", "market_share_b3", 
                  "total_assets_b3", "revenue_b3",
                  "n_participants_b3", "family_element_b3", "zero_interest_loan_b3",
                  "paid_zero_interest_b3", "leverage_b3", "total_assets_b4", 
                  "revenue_b4", "age_b4", 
                  "n_participants_b4", "family_element_b4", "zero_interest_loan_b4",
                  "paid_zero_interest_b4", "leverage_b4")

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

model_roa_unsatisfied <- feols(
  create_formula("pp_case_unsatisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_noncriminal <- feols(
  create_formula("pp_case_any_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_noncriminal <- feols(
  create_formula("pp_case_satisfied_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied_noncriminal <- feols(
  create_formula("pp_case_unsatisfied_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_criminal <- feols(
  create_formula("pp_case_any_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_criminal <- feols(
  create_formula("pp_case_satisfied_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied_criminal <- feols(
  create_formula("pp_case_unsatisfied_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c(c("model_roa_any", "model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal", "model_roa_satisfied_noncriminal","model_roa_unsatisfied_noncriminal", "model_roa_any_criminal", "model_roa_satisfied_criminal", "model_roa_unsatisfied_criminal"))

models_list <- mget(models_names)

results_lag3 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_lag3$model <- forcats::fct_relevel(results_lag3$model, c("model_roa_any", "model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal", "model_roa_satisfied_noncriminal","model_roa_unsatisfied_noncriminal", "model_roa_any_criminal", "model_roa_satisfied_criminal", "model_roa_unsatisfied_criminal"))

results_lag3$order <- "3"

results_lag3$n_cases_any <- llcs_panel[pp_case_any > 0, .N]
results_lag3$n_cases_satisfied <- llcs_panel[pp_case_satisfied > 0, .N]
results_lag3$n_cases_any_noncriminal <- llcs_panel[pp_case_any_noncriminal > 0, .N]
results_lag3$n_cases_satisfied_noncriminal <- llcs_panel[pp_case_satisfied_noncriminal > 0, .N]


# Save baseline unstandardized regression tables ====

etable(first_order_model_roa_any, first_order_model_roa_satisfied, first_order_model_roa_unsatisfied, first_order_model_roa_any_criminal, first_order_model_roa_satisfied_criminal, first_order_model_roa_unsatisfied_criminal, 
       second_order_model_roa_any, second_order_model_roa_satisfied, second_order_model_roa_unsatisfied, second_order_model_roa_any_criminal, second_order_model_roa_satisfied_criminal, second_order_model_roa_unsatisfied_criminal, 
       model_roa_any, model_roa_satisfied, model_roa_unsatisfied, model_roa_any_criminal, model_roa_satisfied_criminal, model_roa_unsatisfied_criminal,
       se.below	= TRUE,
       fitstat = c("n", "r2", "aic", "my"),
       signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.10),
       #       keep = "ROA",
       tex = TRUE,
       file = file.path(
    path_d,
    "plots_tables",
    "regression_tables",
    "first_second_third_order_models_unstandard.tex"
  ))

modelsummary(
  list(
    "1nd order: Satisfied"  = first_order_model_roa_satisfied,
    "1nd order: Unsatisfied" = first_order_model_roa_unsatisfied,
    
    "1nd order: Criminal" = first_order_model_roa_any_criminal,
    "1nd order: Non-criminal" = first_order_model_roa_any_noncriminal,
    
    "2nd order: Satisfied"  = second_order_model_roa_satisfied,
    "2nd order: Unsatisfied" = second_order_model_roa_unsatisfied,
    
    "2nd order: Criminal" = second_order_model_roa_any_criminal,
    "2nd order: Non-criminal" = second_order_model_roa_any_noncriminal,
    
    "3rd order: Satisfied"  = model_roa_satisfied,
    "3rd order: Unsatisfied" = model_roa_unsatisfied,
    
    "3rd order: Criminal" = model_roa_any_criminal,
    "3rd order: Non-criminal" = model_roa_any_noncriminal
  ),
  stars = TRUE,
  coef_rename = var_labels,
  fmt = function(x) format(x, scientific = TRUE, digits = 3),
  output = file.path(
    path_d,
    "plots_tables",
    "regression_tables",
    "first_second_third_order_unstadard_models.docx"
  ))

# Cumulative effect models ====

# Absolute roa models ====

llcs_panel <- llcs_panel_full[roa_b2 != 0 & !is.na(roa_b2) & roa_b3 != 0 & !is.na(roa_b3) & roa_b4 != 0 & !is.na(roa_b4) & 
                                !is.na(total_assets_b3) & !is.na(revenue_b3)  & !is.na(age_b3) & !is.na(family_element_b3) &
                                revenue_b3 > 0 & !is.na(leverage_b3) & !is.infinite(leverage_b3) &
                                !is.na(market_share_b2) & !is.na(total_assets_b2) & !is.na(revenue_b2) & !is.na(age_b2) & !is.na(family_element_b2) &
                                revenue_b2 > 0 & !is.na(leverage_b2) & !is.infinite(leverage_b2) &
                                sole_shareholder_only == 0 & year >= 2014, ] 

cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

control_vars <- c("roa_b2", "roa_b3", "roa_b4", "age_b2", "market_share_b2", 
                  "total_assets_b2", "revenue_b2",  
                  "n_participants_b2", "family_element_b2", "zero_interest_loan_b2",
                  "paid_zero_interest_b2", "leverage_b2", "total_assets_b3", 
                  "revenue_b3", 
                  "n_participants_b3", "family_element_b3", "zero_interest_loan_b3",
                  "paid_zero_interest_b3", "leverage_b3")

# model_roa_any <- feols(
#   create_formula("pp_case_any"),
#   cluster = ~ inn,
#   panel.id = ~inn + year,
#   data = llcs_panel,
#   mem.clean = TRUE, verbose = 3, nthreads = 16)
# 
model_roa_satisfied <- feols(
  create_formula("pp_case_satisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied <- feols(
  create_formula("pp_case_unsatisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_noncriminal <- feols(
  create_formula("pp_case_any_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

# model_roa_satisfied_noncriminal <- feols(
#   create_formula("pp_case_satisfied_noncriminal"),
#   cluster = ~ inn,
#   panel.id = ~inn + year,
#   data = llcs_panel,
#   mem.clean = TRUE, verbose = 3, nthreads = 16)
# 
# model_roa_unsatisfied_noncriminal <- feols(
#   create_formula("pp_case_unsatisfied_noncriminal"),
#   cluster = ~ inn,
#   panel.id = ~inn + year,
#   data = llcs_panel,
#   mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_criminal <- feols(
  create_formula("pp_case_any_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

# model_roa_satisfied_criminal <- feols(
#   create_formula("pp_case_satisfied_criminal"),
#   cluster = ~ inn,
#   panel.id = ~inn + year,
#   data = llcs_panel,
#   mem.clean = TRUE, verbose = 3, nthreads = 16)
# 
# model_roa_unsatisfied_criminal <- feols(
#   create_formula("pp_case_unsatisfied_criminal"),
#   cluster = ~ inn,
#   panel.id = ~inn + year,
#   data = llcs_panel,
#   mem.clean = TRUE, verbose = 3, nthreads = 16)

etable(model_roa_satisfied, model_roa_unsatisfied, model_roa_any_noncriminal, model_roa_any_criminal,
       se.below	= TRUE,
       fitstat = c("n", "r2", "aic"),
       signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.10),
       #       keep = "roa",
       tex = TRUE,
       file = file.path(
    path_d
    "plots_tables",
    "regression_tables",
    "all_abs_roa_models.tex"
  ))

models_names <- c("model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal","model_roa_any_criminal")

models_list <- mget(models_names)

extract_joint_test_all_roa <- function(x, model_id = "name") {
  wald_results <- wald(x, 
                       keep = "roa",
                       cluster = "inn",
                       print = FALSE)
  results <- data.table(
    wald_stat = wald_results$stat,
    wald_p =  wald_results$p,
    model = model_id,
    strain = "absolute"
  )
  return(results)
}

joint_wald_results_all_roa <- rbindlist(Map(
  function(m, n) {extract_joint_test_all_roa(m, model_id = n)}, 
  m = models_list, 
  n = names(models_list)))

extract_cum_effect_all_roa <- function(x, model_id = "name") {
  cum_results <- hypotheses(x,
                            hypothesis = "roa_b2 + roa_b3 + roa_b4 = 0")
  results <- data.table(
    estimate = cum_results$estimate,
    se = cum_results$std.error,
    p_value =  cum_results$p.value,
    model = model_id,
    strain = "absolute"
  )
  return(results)
}

cum_estimate_all_roa <- rbindlist(Map(
  function(m, n) {extract_cum_effect_all_roa(m, model_id = n)}, 
  m = models_list, 
  n = names(models_list)))

fwrite(joint_wald_results_all_roa, file.path(
    path_d,
    "plots_tables",
    "regression_tables",
    "wald_tests_all_abs_roa.csv"
  ))

fwrite(cum_estimate_all_roa, file.path(
    path_d,
    "plots_tables",
    "regression_tables",
    "cum_est_all_abs_roa.csv"
  ))

# Absolute roa models (unstandardized) ====

llcs_panel <- llcs_panel_full[roa_b2 != 0 & !is.na(roa_b2) & roa_b3 != 0 & !is.na(roa_b3) & roa_b4 != 0 & !is.na(roa_b4) & 
                                !is.na(total_assets_b3) & !is.na(revenue_b3)  & !is.na(age_b3) & !is.na(family_element_b3) &
                                revenue_b3 > 0 & !is.na(leverage_b3) & !is.infinite(leverage_b3) &
                                !is.na(market_share_b2) & !is.na(total_assets_b2) & !is.na(revenue_b2) & !is.na(age_b2) & !is.na(family_element_b2) &
                                revenue_b2 > 0 & !is.na(leverage_b2) & !is.infinite(leverage_b2) &
                                sole_shareholder_only == 0 & year >= 2014, ] 

control_vars <- c("roa_b2", "roa_b3", "roa_b4", "age_b2", "market_share_b2", 
                  "total_assets_b2", "revenue_b2",  
                  "n_participants_b2", "family_element_b2", "zero_interest_loan_b2",
                  "paid_zero_interest_b2", "leverage_b2", "total_assets_b3", 
                  "revenue_b3", 
                  "n_participants_b3", "family_element_b3", "zero_interest_loan_b3",
                  "paid_zero_interest_b3", "leverage_b3")

# model_roa_any <- feols(
#   create_formula("pp_case_any"),
#   cluster = ~ inn,
#   panel.id = ~inn + year,
#   data = llcs_panel,
#   mem.clean = TRUE, verbose = 3, nthreads = 16)
# 
model_roa_satisfied <- feols(
  create_formula("pp_case_satisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_unsatisfied <- feols(
  create_formula("pp_case_unsatisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_noncriminal <- feols(
  create_formula("pp_case_any_noncriminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

# model_roa_satisfied_noncriminal <- feols(
#   create_formula("pp_case_satisfied_noncriminal"),
#   cluster = ~ inn,
#   panel.id = ~inn + year,
#   data = llcs_panel,
#   mem.clean = TRUE, verbose = 3, nthreads = 16)
# 
# model_roa_unsatisfied_noncriminal <- feols(
#   create_formula("pp_case_unsatisfied_noncriminal"),
#   cluster = ~ inn,
#   panel.id = ~inn + year,
#   data = llcs_panel,
#   mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_criminal <- feols(
  create_formula("pp_case_any_criminal"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

# model_roa_satisfied_criminal <- feols(
#   create_formula("pp_case_satisfied_criminal"),
#   cluster = ~ inn,
#   panel.id = ~inn + year,
#   data = llcs_panel,
#   mem.clean = TRUE, verbose = 3, nthreads = 16)
# 
# model_roa_unsatisfied_criminal <- feols(
#   create_formula("pp_case_unsatisfied_criminal"),
#   cluster = ~ inn,
#   panel.id = ~inn + year,
#   data = llcs_panel,
#   mem.clean = TRUE, verbose = 3, nthreads = 16)

modelsummary(
  list(
    "Satisfied"  = model_roa_satisfied,
    "Unsatisfied" = model_roa_unsatisfied,
    
    "Criminal" = model_roa_any_criminal,
    "Non-criminal" = model_roa_any_noncriminal
  ),
  stars = TRUE,
  coef_rename = var_labels,
  fmt = function(x) format(x, scientific = TRUE, digits = 3),
  output = file.path(
    path_d,
    "plots_tables",
    "regression_tables",
    "all_abs_roa_models_unstandard.docx"
  ))

models_names <- c("model_roa_satisfied", "model_roa_unsatisfied", "model_roa_any_noncriminal","model_roa_any_criminal")

models_list <- mget(models_names)

extract_joint_test_all_roa <- function(x, model_id = "name") {
  wald_results <- wald(x, 
                       keep = "roa",
                       cluster = "inn",
                       print = FALSE)
  results <- data.table(
    wald_stat = wald_results$stat,
    wald_p =  wald_results$p,
    model = model_id,
    strain = "absolute"
  )
  return(results)
}

joint_wald_results_all_roa <- rbindlist(Map(
  function(m, n) {extract_joint_test_all_roa(m, model_id = n)}, 
  m = models_list, 
  n = names(models_list)))

extract_cum_effect_all_roa <- function(x, model_id = "name") {
  cum_results <- hypotheses(x,
                            hypothesis = "roa_b2 + roa_b3 + roa_b4 = 0")
  results <- data.table(
    estimate = cum_results$estimate,
    se = cum_results$std.error,
    p_value =  cum_results$p.value,
    model = model_id,
    strain = "absolute"
  )
  return(results)
}

cum_estimate_all_roa <- rbindlist(Map(
  function(m, n) {extract_cum_effect_all_roa(m, model_id = n)}, 
  m = models_list, 
  n = names(models_list)))


fwrite(cum_estimate_all_roa, file.path(
  path_d,
  "plots_tables",
  "regression_tables",
  "cum_est_all_abs_roa_unstandard.csv"
))


