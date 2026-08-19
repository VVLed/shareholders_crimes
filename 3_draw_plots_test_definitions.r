library(data.table)
library(ggplot2)
library(stringr); library(stringi)
options(scipen = 999)

# Declare working directory beforehand in an environment variable
# CLASSIFICATION_CORP_CASES = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to take effect
path <- Sys.getenv("SHAREHOLDERS_CRIMES_PATH")
setwd(path)

# Both predictors ====
pp_models_results_both_predictors_both_lags <- fread(paste0("plots_tables/estimation_results/disp_strain_pp_models_results_eight_transformed_both_predictors.csv"))

pp_models_results_both_predictors_both_lags$order <- as.character(pp_models_results_both_predictors_both_lags$order)

pp_models_results <- rbindlist(list(
  pp_models_results_both_predictors_both_lags[order == "1", .(estimate = roa_b1_slope, se = roa_b1_se, model = paste0(model, "_" ,order), lag = "-1", order, strain = "absolute")],
  pp_models_results_both_predictors_both_lags[order == "1", .(estimate = roa_b2_slope, se = roa_b2_se,  model = paste0(model, "_" ,order), lag = "-2", order, strain = "absolute")],
  pp_models_results_both_predictors_both_lags[order == "1", .(estimate = roa_hist_b2_slope, se = roa_hist_b2_se,  model = paste0(model, "_" ,order), lag = "-2", order, strain = "hist")],
  
  pp_models_results_both_predictors_both_lags[order == "2", .(estimate = roa_b1_slope, se = roa_b1_se,  model = paste0(model, "_" ,order), lag = "-2", order, strain = "absolute")],
  pp_models_results_both_predictors_both_lags[order == "2", .(estimate = roa_b2_slope, se = roa_b2_se,  model = paste0(model, "_" ,order), lag = "-3", order, strain = "absolute")],
  pp_models_results_both_predictors_both_lags[order == "2", .(estimate = roa_hist_b2_slope, se = roa_hist_b2_se,  model = paste0(model, "_" ,order), lag = "-3", order, strain = "hist")]
))

pp_models_results$model <- forcats::fct_relevel(pp_models_results$model, 
                                                                "model_roa_any_1", "model_roa_satisfied_1", "model_roa_any_key_1", "model_roa_satisfied_key_1",
                                                "model_roa_any_2", "model_roa_satisfied_2", "model_roa_any_key_2", "model_roa_satisfied_key_2")

# Draw plot ====
pp_models_plot <- ggplot(pp_models_results, aes(x = lag, y = estimate, group = strain, shape = strain)) + # 
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  geom_errorbar(aes(ymin = estimate - (1.96 * se), ymax = estimate + (1.96 * se)), 
                linewidth = 0.5, width = 0.3, 
                position = position_dodge(width = 0.5)) +
  theme_bw() +
  theme(plot.title = element_blank(), # element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
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
#  scale_y_continuous(breaks = c(-0.008, -0.004, 0, 0.004)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ model, nrow = 2, ncol = 4, scales = "free_x",
             labeller = labeller(model = c(model_roa_any_1 = "Any claim", model_roa_satisfied_1 = "Satisfied claim", model_roa_any_key_1 = "Any serious claim", model_roa_satisfied_key_1 = "Satisfied serious claim",
                                           model_roa_any_2 = "", model_roa_satisfied_2 = "", model_roa_any_key_2 = "", model_roa_satisfied_key_2 = ""
                                           ))) +
  scale_shape_manual(values = c("absolute" = 1,  
                                "hist" = 15),     
                     labels = c("absolute" = "Absolute", 
                                "hist" = "Historical")) +
  labs(
    x = "Lag of financial variables relative to suit filing", 
    y = "Standardized estimates", 
#    title = "", # How absolute and relative firm performance metrics are associated with\n shareholders crimes
    shape = "Firm performance (ROA)")

ggsave(pp_models_plot, file = paste0("plots_tables/plots/pp_models_results_both_predictors.png"), device = "png", height = 5, width = 9)

# Separate models for separate predictors ====

pp_models_results_sole_predictors <- fread(paste0("plots_tables/estimation_results/disp_strain_pp_models_results_eight_transformed_sole_predictors.csv"))
pp_models_results_sole_predictors$order <- as.character(pp_models_results_sole_predictors$order)
pp_models_plot_sole <- ggplot(pp_models_results_sole_predictors[order %chin% c("2", "3", "4")], aes(x = order, y = estimate, group = strain, shape = strain)) + # 
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  geom_errorbar(aes(ymin = estimate - (1.96 * se), ymax = estimate + (1.96 * se)), 
                linewidth = 0.5, width = 0.3, 
                position = position_dodge(width = 0.5)) +
  theme_bw() +
  theme(plot.title = element_blank(), # element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
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
#  scale_y_continuous(breaks = c(-0.01, 0, 0.01)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ model, nrow = 1, ncol = 4, 
             labeller = labeller(model = c(model_roa_any = "Any claim", model_roa_satisfied = "Satisfied claim", model_roa_any_key = "Any serious claim", model_roa_satisfied_key = "Satisfied serious claim"))) +
  scale_shape_manual(values = c("absolute" = 1,  
                                "ind" = 2,       
                                "hist" = 15),     
                     labels = c("absolute" = "Absolute", 
                                "ind" = "Industrial", 
                                "hist" = "Historical")) +
  labs(
    x = "Lag of financial variables relative to suit filing", 
    y = "Standardized estimates", 
#    title = "", # How different financial strains are associated with\ndifferent operationalizations of the shareholders crimes
#    subtitle = "", # Separate models for separate strains
    shape = "Firm performance (ROA)")

ggsave(pp_models_plot_sole, file = paste0("plots_tables/plots/pp_models_results_sole_predictors.png"), device = "png", height = 5, width = 9)

