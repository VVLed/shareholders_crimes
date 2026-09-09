library(data.table)
library(ggplot2)
library(stringr); library(stringi)
library(ggpp)
library(egg)
options(scipen = 999)

# Declare working directory beforehand in an environment variable
# CLASSIFICATION_CORP_CASES = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to take effect
path <- Sys.getenv("TFP_STUDY_PATH")
setwd(path)
path_d <- Sys.getenv("DRIVE_D")

# Regression models ====
pp_models_results_both_predictors_both_lags <- fread(paste0(path_d, "plots_tables/estimation_results/pp_models_both_predictors.csv"))

pp_models_results_both_predictors_both_lags$order <- as.character(pp_models_results_both_predictors_both_lags$order)
pp_models_results_both_predictors_both_lags$appr_lag <- as.character(pp_models_results_both_predictors_both_lags$appr_lag)

pp_models_results <- rbindlist(list(
  pp_models_results_both_predictors_both_lags[order == "1" & appr_lag == "0", .(estimate = roa_b1_slope, se = roa_b1_se, model = paste0(model, "_" ,order), lag = "-1", order, strain = "absolute", appr_lag)],
  pp_models_results_both_predictors_both_lags[order == "1" & appr_lag == "0", .(estimate = roa_b2_slope, se = roa_b2_se,  model = paste0(model, "_" ,order), lag = "-2", order, strain = "absolute", appr_lag)],
  pp_models_results_both_predictors_both_lags[order == "1" & appr_lag == "0", .(estimate = roa_hist_b2_slope, se = roa_hist_b2_se,  model = paste0(model, "_" ,order), lag = "-2", order, strain = "hist", appr_lag)],
  
  pp_models_results_both_predictors_both_lags[order == "2" & appr_lag == "0", .(estimate = roa_b1_slope, se = roa_b1_se,  model = paste0(model, "_" ,order), lag = "-2", order, strain = "absolute", appr_lag)],
  pp_models_results_both_predictors_both_lags[order == "2" & appr_lag == "0", .(estimate = roa_b2_slope, se = roa_b2_se,  model = paste0(model, "_" ,order), lag = "-3", order, strain = "absolute", appr_lag)],
  pp_models_results_both_predictors_both_lags[order == "2" & appr_lag == "0", .(estimate = roa_hist_b2_slope, se = roa_hist_b2_se,  model = paste0(model, "_" ,order), lag = "-3", order, strain = "hist", appr_lag)],
  
  pp_models_results_both_predictors_both_lags[order == "3" & appr_lag == "0", .(estimate = roa_b1_slope, se = roa_b1_se,  model = paste0(model, "_" ,order), lag = "-3", order, strain = "absolute", appr_lag)],
  pp_models_results_both_predictors_both_lags[order == "3" & appr_lag == "0", .(estimate = roa_b2_slope, se = roa_b2_se,  model = paste0(model, "_" ,order), lag = "-4", order, strain = "absolute", appr_lag)],
  pp_models_results_both_predictors_both_lags[order == "3" & appr_lag == "0", .(estimate = roa_hist_b2_slope, se = roa_hist_b2_se,  model = paste0(model, "_" ,order), lag = "-4", order, strain = "hist", appr_lag)],
  
  pp_models_results_both_predictors_both_lags[order == "1" & appr_lag == "1", .(estimate = roa_b1_slope, se = roa_b1_se, model = paste0(model, "_" ,order), lag = "-1", order, strain = "absolute", appr_lag)],
  pp_models_results_both_predictors_both_lags[order == "1" & appr_lag == "1", .(estimate = roa_b2_slope, se = roa_b2_se,  model = paste0(model, "_" ,order), lag = "-2", order, strain = "absolute", appr_lag)],
  pp_models_results_both_predictors_both_lags[order == "1" & appr_lag == "1", .(estimate = roa_hist_b2_slope, se = roa_hist_b2_se,  model = paste0(model, "_" ,order), lag = "-2", order, strain = "hist", appr_lag)],
  
  pp_models_results_both_predictors_both_lags[order == "2" & appr_lag == "1", .(estimate = roa_b1_slope, se = roa_b1_se,  model = paste0(model, "_" ,order), lag = "-2", order, strain = "absolute", appr_lag)],
  pp_models_results_both_predictors_both_lags[order == "2" & appr_lag == "1", .(estimate = roa_b2_slope, se = roa_b2_se,  model = paste0(model, "_" ,order), lag = "-3", order, strain = "absolute", appr_lag)],
  pp_models_results_both_predictors_both_lags[order == "2" & appr_lag == "1", .(estimate = roa_hist_b2_slope, se = roa_hist_b2_se,  model = paste0(model, "_" ,order), lag = "-3", order, strain = "hist", appr_lag)],
  
  pp_models_results_both_predictors_both_lags[order == "3" & appr_lag == "1", .(estimate = roa_b1_slope, se = roa_b1_se,  model = paste0(model, "_" ,order), lag = "-3", order, strain = "absolute", appr_lag)],
  pp_models_results_both_predictors_both_lags[order == "3" & appr_lag == "1", .(estimate = roa_b2_slope, se = roa_b2_se,  model = paste0(model, "_" ,order), lag = "-4", order, strain = "absolute", appr_lag)],
  pp_models_results_both_predictors_both_lags[order == "3" & appr_lag == "1", .(estimate = roa_hist_b2_slope, se = roa_hist_b2_se,  model = paste0(model, "_" ,order), lag = "-4", order, strain = "hist", appr_lag)]
))

models_order <- c(
  "model_roa_any_1", "model_roa_satisfied_1", "model_roa_unsatisfied_1", "model_roa_any_criminal_1", "model_roa_satisfied_criminal_1", "model_roa_unsatisfied_criminal_1",  "model_roa_any_noncriminal_1", "model_roa_satisfied_noncriminal_1", "model_roa_unsatisfied_noncriminal_1", 
  "model_roa_any_2", "model_roa_satisfied_2", "model_roa_unsatisfied_2", "model_roa_any_criminal_2",  "model_roa_satisfied_criminal_2", "model_roa_unsatisfied_criminal_2", "model_roa_any_noncriminal_2", "model_roa_satisfied_noncriminal_2", "model_roa_unsatisfied_noncriminal_2",   
  "model_roa_any_3", "model_roa_satisfied_3", "model_roa_unsatisfied_3",  "model_roa_any_criminal_3", "model_roa_satisfied_criminal_3", "model_roa_unsatisfied_criminal_3",   "model_roa_any_noncriminal_3", "model_roa_satisfied_noncriminal_3", "model_roa_unsatisfied_noncriminal_3"
)

pp_models_results$model <- forcats::fct_relevel(pp_models_results$model, models_order)

pp_models_results[, criminality := fcase(
  grepl("noncrim", pp_models_results$model), "noncrim",
  grepl("_crim", pp_models_results$model), "crim",
  default = "any"
)]

pp_models_results$criminality <- forcats::fct_relevel(pp_models_results$criminality, c("crim", "noncrim", "any"))

pp_models_results[, satisfaction := fcase(
  grepl("_satis", pp_models_results$model), "satisfied",
  grepl("unsatisf", pp_models_results$model), "unsatisfied",
  default = "any"
)]

pp_models_results$satisfaction <- forcats::fct_relevel(pp_models_results$satisfaction, c("satisfied", "unsatisfied", "any"))

pp_models_results[, positive_estimate := fifelse(estimate > 0, "1", "0")]

pp_models_results[, significant := fifelse(
  (estimate > 0 & (estimate - (1.96 * se)) > 0) | 
    (estimate < 0 & (estimate + (1.96 * se)) < 0)
  , "1", "0")]

## First-third orders ====

models_to_plot <- c(
  "model_roa_any_noncriminal_1", "model_roa_any_criminal_1", "model_roa_satisfied_1", "model_roa_unsatisfied_1", 
  "model_roa_any_noncriminal_2", "model_roa_any_criminal_2", "model_roa_satisfied_2", "model_roa_unsatisfied_2",
  "model_roa_any_noncriminal_3", "model_roa_any_criminal_3", "model_roa_satisfied_3", "model_roa_unsatisfied_3"
)


pp_models_results[, shape_group := interaction(strain, positive_estimate, sep = "_")]

baseline_plot <- ggplot(pp_models_results[appr_lag == "0" & model %chin% models_to_plot, ], 
       aes(x = lag, y = estimate, group = strain, shape = strain, linetype = strain)) + 
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  geom_errorbar(aes(ymin = estimate - (1.96 * se), ymax = estimate + (1.96 * se)), 
                linewidth = 0.5, width = 0.3, 
                position = position_dodge(width = 0.5)) +
  theme_bw() +
  theme(plot.title = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(0.1, "cm"),
        panel.spacing.y = unit(-0.5, "cm"),
        text = element_text(family = "Times New Roman"),
        legend.key.width = unit(3, "cm"),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 10),
        strip.background = element_blank()) +
  geom_hline(yintercept = 0) +
#  coord_cartesian(ylim = c(-0.005, 0.005)) +
  facet_wrap(~ model, nrow = 3, ncol = 4, scales = "free_x",
             labeller = labeller(model = c(
               model_roa_any_noncriminal_1 = "Non-criminal", 
               model_roa_any_criminal_1 = "Criminal", 
               model_roa_satisfied_1 = "Satisfied", 
               model_roa_unsatisfied_1 = "Rejected", 
               model_roa_any_noncriminal_2 = "", 
               model_roa_any_criminal_2 = "", 
               model_roa_satisfied_2 = "", 
               model_roa_unsatisfied_2 = "", 
               model_roa_any_noncriminal_3 = "", 
               model_roa_any_criminal_3 = "", 
               model_roa_satisfied_3 = "", 
               model_roa_unsatisfied_3 = ""
             ))
  ) +
  scale_shape_manual(
    name = "Strain",
    values = c(
      "absolute" = 16,  
      "hist" = 17
    ),
    labels = c(
      "absolute" = "Absolute",  
      "hist" = "Historical"
    )
  ) +
  scale_linetype_manual(
    name = "Strain",
    values = c(
      "absolute" = "solid",  
      "hist" = "dashed"
    ),
    labels = c(
      "absolute" = "Absolute",  
      "hist" = "Historical"
    )
  ) +
  labs(
    x = "Lag of financial variables relative to suit filing", 
    y = "Standardized estimates", 
    subtitle = "Type of claim",
    shape = "Strain",
    linetype = "Strain"
    ) +
  guides(shape = guide_legend(nrow = 2, byrow = TRUE, order = 1), linetype = guide_legend(nrow = 2, byrow = TRUE, order = 1) )

ggsave(baseline_plot, file = paste0(path_d, "plots_tables/plots/pp_models_results_both_predictors_baseline.jpeg"), device = "jpeg", height = 7, width = 9)

## With appropriate lags ====
models_to_plot <- c(
  "model_roa_any_noncriminal_1", "model_roa_any_criminal_1", "model_roa_satisfied_1", "model_roa_unsatisfied_1", 
  "model_roa_any_noncriminal_2", "model_roa_any_criminal_2", "model_roa_satisfied_2", "model_roa_unsatisfied_2",
  "model_roa_any_noncriminal_3", "model_roa_any_criminal_3", "model_roa_satisfied_3", "model_roa_unsatisfied_3"
)
baseline_appr_lags_plot <-  ggplot(pp_models_results[model %chin% models_to_plot, ], aes(x = lag, y = estimate, group = interaction(strain, appr_lag), shape = strain, linetype = appr_lag)) + 
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  geom_errorbar(aes(ymin = estimate - (1.96 * se), ymax = estimate + (1.96 * se)), 
                linewidth = 0.5, width = 0.5, 
                position = position_dodge(width = 0.5)) +
  theme_bw() +
  theme(plot.title = element_blank(), # element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_text(size = 12,),
        axis.title.y = element_text(size = 12),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(0.1, "cm"),
        panel.spacing.y = unit(-0.5, "cm"),
        text = element_text(family = "Times New Roman"),
        legend.key.width = unit(3, "cm"),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 10),
        strip.background = element_blank()) +
  geom_hline(yintercept = 0) +
#  coord_cartesian(ylim = c(-0.006, 0.01)) +
  facet_wrap(~ model, nrow = 3, ncol = 4, scales = "free_x",
             labeller = labeller(model = c(
               model_roa_any_noncriminal_1 = "Non-criminal", 
               model_roa_any_criminal_1 = "Criminal", 
               model_roa_satisfied_1 = "Satisfied", 
               model_roa_unsatisfied_1 = "Rejected", 
               model_roa_any_noncriminal_2 = "", 
               model_roa_any_criminal_2 = "", 
               model_roa_satisfied_2 = "", 
               model_roa_unsatisfied_2 = "", 
               model_roa_any_noncriminal_3 = "", 
               model_roa_any_criminal_3 = "", 
               model_roa_satisfied_3 = "", 
               model_roa_unsatisfied_3 = ""
               )
               )
  ) +
  scale_shape_manual(values = c("absolute" = 1,  
                                "hist" = 16),     
                     labels = c("absolute" = "Absolute", 
                                "hist" = "Historical")) +
  scale_linetype_manual(values = c("0" = "solid",  
                                   "1" = "dashed"),     
                        labels = c("0" = "Unadjusted", 
                                   "1" = "Adjusted")) +
  labs(
    x = "Lag of financial variables relative to suit filing", 
    y = "Standardized estimates", 
    subtitle = "Type of claim",
    shape = "Financial performance (ROA)",
    linetype = "Limitation period") +
  guides(linetype = guide_legend(nrow = 2, byrow = TRUE, order = 2),
         shape = guide_legend(nrow = 2, byrow = TRUE, order = 1))

ggsave(baseline_appr_lags_plot, file = paste0(path_d, "plots_tables/plots/pp_models_results_both_predictors_baseline_with_appr_lags.jpeg"), device = "jpeg", height = 10, width = 11)

# Cum. estimates ====

## Without weights ====
pp_models_cumulative_estimates_hist <- fread(paste0(path_d, "plots_tables/estimation_results/cum_est_all_hist_roa.csv"))
pp_models_cumulative_estimates_abs <- fread(paste0(path_d, "plots_tables/estimation_results/cum_est_all_abs_roa.csv"))

pp_models_cumulative_estimates <- rbind(pp_models_cumulative_estimates_abs, pp_models_cumulative_estimates_hist)

models_order <- c(
  "model_roa_satisfied", "model_roa_unsatisfied",
  "model_roa_any_criminal", "model_roa_any_noncriminal"
  )

pp_models_cumulative_estimates$model <- forcats::fct_relevel(pp_models_cumulative_estimates$model, models_order)

pp_models_cumulative_estimates[, positive_estimate := fifelse(estimate > 0, "1", "0")]

pp_models_cumulative_estimates[, significant := fifelse(
  (estimate > 0 & (estimate - (1.96 * se)) > 0) | 
    (estimate < 0 & (estimate + (1.96 * se)) < 0)
  , "1", "0")]

models_to_plot <- c(
  "model_roa_satisfied", "model_roa_unsatisfied",
  "model_roa_any_criminal", "model_roa_any_noncriminal"
  )

plots_cumulative_abs <- ggplot(pp_models_cumulative_estimates[strain == "absolute"], 
                               aes(x = model, y = estimate)) + 
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = estimate - (1.96 * se), ymax = estimate + (1.96 * se)), 
                linewidth = 0.5, width = 0.3, position = position_dodge(width = 0.5)) +
  theme_bw() +
  theme(plot.title = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 25, vjust = 0.5, hjust = 0.5),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        text = element_text(family = "Times New Roman"),
        legend.key.width = unit(3, "cm"),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 10),
        strip.background = element_blank()) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = c(
    "model_roa_satisfied" = "", 
    "model_roa_unsatisfied" = "",
    "model_roa_any_criminal" = "", 
    "model_roa_any_noncriminal" = ""
  )) +
  labs(
    x = "", 
    y = "", # Cumulative standardized estimates
    subtitle = "Without weights"
  )

ggsave(plots_cumulative_abs, file = paste0(path_d, "plots_tables/plots/pp_models_results_cumulative_abs_roa.jpeg"), device = "jpeg", height = 5, width = 9)

## With weights ====

pp_models_cumulative_estimates_weighted <- fread(paste0(path_d, "plots_tables/estimation_results/cum_est_all_abs_roa_with_weights.csv"))

models_to_plot <- c(
  "pp_case_satisfied", "pp_case_unsatisfied",
  "pp_case_any_criminal", "pp_case_any_noncriminal"
)

pp_models_cumulative_estimates_weighted$model <- forcats::fct_relevel(pp_models_cumulative_estimates_weighted$model, models_to_plot)

plots_cumulative_abs_weighted <- ggplot(pp_models_cumulative_estimates_weighted[strain == "absolute" & model %chin% models_to_plot & standardized == "yes", ], 
aes(x = model, y = estimate)) +  # , group = method, shape = method, linetype = method
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = estimate - (1.645 * se), ymax = estimate + (1.645 * se)), 
                linewidth = 0.5, width = 0.3, position = position_dodge(width = 0.5)) +
  theme_bw() +
  theme(plot.title = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 25, vjust = 0.5, hjust = 0.5),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        text = element_text(family = "Times New Roman"),
        legend.key.width = unit(3, "cm"),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 10),
        strip.background = element_blank()) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = c(
    "pp_case_satisfied" = "Satisfied", 
    "pp_case_unsatisfied" = "Rejected",
    "pp_case_any_criminal" = "Criminal", 
    "pp_case_any_noncriminal" = "Non-criminal")) +
  facet_wrap(~ method, nrow = 1, ncol = 2, scales = "free_y",
             labeller = labeller(method = c(
               cem = "CEM", 
               ebal = "EBAL"))) +
  labs(
    x = "", 
    y = "Cumulative standardized estimates")

ggsave(plots_cumulative_abs_weighted, file = paste0(path_d, "plots_tables/plots/pp_models_results_cumulative_abs_roa_with_weights.jpeg"), device = "jpeg", height = 5, width = 9)
