# Clear all
cat("\f")
rm(list=ls())

# Load necessary library
library(readxl)
library(irr)
library(rmarkdown)
library(lpSolve)
library(ggplot2)
library(MethComp)
library(moments)
library(rlang)
library(knitr)

# Set working directory to the directory of the current script
setwd(dirname(sys.frame(1)$ofile))

# List all Excel files in the current directory
excel_files <- list.files(pattern = "\\.(xlsx|xls)$")
input_names <- tools::file_path_sans_ext(excel_files)

# Check if there are not exactly 2 Excel files
if(length(excel_files) != 2) {
  stop("There are not exactly 2 excel files. Please check your input data again!")
}

# Import the first and second Excel files as data frames
data_set_1 <- read_excel(excel_files[1])
data_set_2 <- read_excel(excel_files[2])

# Check column titles of imported data
column_titles_1 <- colnames(data_set_1)
column_titles_2 <- colnames(data_set_2)

# Check if the column titles are identical
if (!identical(column_titles_1, column_titles_2)) {
  stop("The column titles inside both excel files are not identical. Please check your input data again!")
}

# Ensure that both data frames have the same number of columns
if (ncol(data_set_1) != ncol(data_set_2)) {
  stop("The number of columns in the two data frames is not equal!")
}

# Number of columns
num_cols <- ncol(data_set_1)

# Initializing output list
icc_results_list <- list()
paired_t_results_list <- list()
unpaired_t_results_list <- list()
pearson_results_list <- list()
pb_results_list <- list()
pb_plot_results_list <- list()
ba_plot_results_list <- list()
pba_plot_results_list <- list()
hst_plot_results_list <- list()
shapiro_results_list <- list()
agostino_results_list <- list()
ks_results_list <- list()
quad_fit_results_list <- list()
quad_fit_plot_results_list <- list()
cubic_fit_results_list <- list()
cubic_fit_plot_results_list <- list()

# Creating function of subsequent Bland-altman analysis
baize_df <- function(df) {
  # Ensure the data frame has only two columns
  if (ncol(df) != 2) {
    stop("The data frame should have exactly two columns.")
  }
  
  # Calculate mean, difference, percentage difference
  df$mean <- (df[,1] + df[,2]) / 2
  df$difference <- df[,1] - df[,2]
  df$percentage_difference <- (df$difference / df$mean) * 100
  
  return(df)
}

# Creating function for calculation of Bland-altman statistics
calculate_BA_CI <- function(input) {
  # Ensure input is a numeric vector
  if (!is.numeric(input)) {
    stop("Input should be a numeric vector.")
  }
  
  # Calculate necessary statistics
  sample_size <- length(input)
  sample_df <- sample_size - 1
  input_SD <- sd(input)
  input_SE <- sqrt(input_SD^2 / sample_size)
  sd_SE <- sqrt(3 * input_SD^2 / sample_size)
  
  # Assuming you want a 95% CI, the t-value for a two-tailed test is:
  t_value_df <- qt(0.975, df = sample_df)
  
  input_conf <- input_SE * t_value_df
  sd_conf <- sd_SE * t_value_df
  
  # Calculate the confidence intervals
  CI_of_input <- c(mean(input) - input_conf, mean(input) + input_conf)
  CI_of_upr_sd <- c(mean(input) + 1.96 * input_SD - sd_conf, mean(input) + 1.96 * input_SD + sd_conf)
  CI_of_lwr_sd <- c(mean(input) - 1.96 * input_SD - sd_conf, mean(input) - 1.96 * input_SD + sd_conf)
  
  # Return the confidence intervals as a list
  return(list(CI_of_input, CI_of_upr_sd, CI_of_lwr_sd))
}

# My customized Bland altman plot function
my_baplot <- function(data_x, data_y, x_axis_label, y_axis_label, title_label) {
  
  # Deriving BA plot statistics for plotting - Preparing for x and y axes coordinates
  ba_x_min <- floor(min(data_x) / 5) * 5
  ba_x_max <- ceiling(max(data_x) / 5) * 5
  ba_x_breaks <- seq(from = ba_x_min, to = ba_x_max, length.out = 5)
  absolute_max_diff <- max(abs(min(data_y)), abs(max(data_y)))
  ba_y_min <- -ceiling(absolute_max_diff / 5) * 5
  ba_y_max <- ceiling(absolute_max_diff / 5) * 5
  ba_y_breaks <- seq(from = ba_y_min, to = ba_y_max, length.out = 5)
  
  ba_ci_data <- calculate_BA_CI(data_y)
  ba_x_vals <- seq(from = ba_x_min, to = ba_x_max, length.out = 100)
  ba_ribbon_data <- data.frame(
    x = ba_x_vals, 
    ymin = pmax(ba_ci_data[[1]][[1]], ba_y_min), 
    ymax = pmin(ba_ci_data[[1]][[2]], ba_y_max)
  )
  ba_upr_ribbon_data <- data.frame(
    x = ba_x_vals, 
    ymin = pmax(ba_ci_data[[2]][[1]], ba_y_min), 
    ymax = pmin(ba_ci_data[[2]][[2]], ba_y_max)
  )
  ba_lwr_ribbon_data <- data.frame(
    x = ba_x_vals, 
    ymin = pmax(ba_ci_data[[3]][[1]], ba_y_min), 
    ymax = pmin(ba_ci_data[[3]][[2]], ba_y_max)
  )
  
  data <- data.frame(x = data_x, y = data_y)
  
  ba_plot_result <- ggplot(
    data, aes(x = x, y = y)) +
    geom_ribbon(data = ba_ribbon_data, aes(x = x, ymin = ymin, ymax = ymax),
                fill = "#253494", alpha = 0.2, inherit.aes = FALSE)+
    geom_ribbon(data = ba_upr_ribbon_data, aes(x = x, ymin = ymin, ymax = ymax),
                fill = "orange", alpha = 0.2, inherit.aes = FALSE)+
    geom_ribbon(data = ba_lwr_ribbon_data, aes(x = x, ymin = ymin, ymax = ymax),
                fill = "orange", alpha = 0.2, inherit.aes = FALSE)+
    geom_hline(yintercept = 0, color = "red", linewidth = 0.1, linetype="dashed") +
    geom_hline(yintercept = mean(data_y), color = "#253494", linewidth = 1) +
    geom_hline(yintercept = mean(data_y) + 1.96 * sd(data_y), color = "orange", linewidth = 0.25, linetype="dashed") +
    geom_hline(yintercept = mean(data_y) - 1.96 * sd(data_y), color = "orange", linewidth = 0.25, linetype="dashed") +
    geom_point(shape = 21, colour = "black", fill = "white", size = 2) +
    labs(title = title_label,
         x = x_axis_label,
         y = y_axis_label) +
    scale_x_continuous(limits = c(ba_x_min, ba_x_max), breaks = ba_x_breaks, expand = c(0,0)) +
    scale_y_continuous(limits = c(ba_y_min, ba_y_max), breaks = ba_y_breaks, expand = c(0,0)) +
    theme_minimal()+
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      plot.title = element_text(hjust = 0.5),
      aspect.ratio = 1
    )
  
  # Label the mean / +1.96SD / -1.96SD
  ba_mean_label_expr <- sprintf("%.1f", mean(data_y))
  mean_y_position <- mean(data_y)
  upper_sd_position <- mean(data_y) + 1.96 * sd(data_y)
  lower_sd_position <- mean(data_y) - 1.96 * sd(data_y)
  ba_x_label_offset <- (ba_x_max - ba_x_min) * 0.0125
  ba_y_label_offset <- (ba_y_max - ba_y_min) * 0.0125
  upper_sd_label_expr <- sprintf("%.1f", upper_sd_position)
  lower_sd_label_expr <- sprintf("%.1f", lower_sd_position)
  font_size = 3
  
  ba_plot_result <- ba_plot_result +
    geom_text(inherit.aes = FALSE,
              x = ba_x_max - ba_x_label_offset,
              y = mean_y_position + ba_y_label_offset,
              label = "Mean", hjust = 1, vjust = 0, size = font_size
    ) +
    geom_text(inherit.aes = FALSE,
              x = ba_x_max - ba_x_label_offset,
              y = mean_y_position - ba_y_label_offset,
              label = ba_mean_label_expr,
              hjust = 1, vjust = 1, size = 3
    ) +
    geom_text(inherit.aes = FALSE,
              x = ba_x_max - ba_x_label_offset,
              y = upper_sd_position + ba_y_label_offset,
              label = "+1.96 SD", hjust = 1, vjust = 0, size = font_size
    ) +
    geom_text(inherit.aes = FALSE,
              x = ba_x_max - ba_x_label_offset,
              y = upper_sd_position - ba_y_label_offset,
              label = upper_sd_label_expr,
              hjust = 1, vjust = 1, size = font_size
    ) +
    geom_text(inherit.aes = FALSE,
              x = ba_x_max - ba_x_label_offset,
              y = lower_sd_position + ba_y_label_offset,
              label = "-1.96 SD", hjust = 1, vjust = 0, size = font_size
    ) +
    geom_text(inherit.aes = FALSE,
              x = ba_x_max - ba_x_label_offset,
              y = lower_sd_position - ba_y_label_offset,
              label = lower_sd_label_expr,
              hjust = 1, vjust = 1, size = font_size
    )
  
  # Return the plot
  return(ba_plot_result)
}

# Loop through each column and calculate each column
for (i in 1:num_cols) {

  # Check if the lengths are not the same
  if (length(data_set_1[[i]]) != length(data_set_2[[i]])) {
    # Print the error message and skip the rest of the current iteration
    print(paste0(column_titles_1[[i]], " is having unequal number of input sample hence the calculation cannot proceed"))
    next
  }
  
  # Defining input data frame
  df_rater1vs2 <- data.frame(rater1 = data_set_1[[i]], rater2 = data_set_2[[i]])
  
  # Calculate icc and store result onto list
  icc_result <- icc(df_rater1vs2, model="twoway", type="agreement", unit="single")
  icc_results_list[[i]] <- icc_result
  
  # Calculate paired t test and store result onto list
  paired_t_result <- t.test(df_rater1vs2$rater1, df_rater1vs2$rater2, paired=TRUE)
  paired_t_results_list[[i]] <- paired_t_result
  
  # Calculate unpaired t test and store result onto list
  unpaired_t_result <- t.test(df_rater1vs2$rater1, df_rater1vs2$rater2, paired=FALSE)
  unpaired_t_results_list[[i]] <- unpaired_t_result
  
  # Calculate Pearson Product-Moment Correlation and store result onto list
  pearson_results <- cor.test(df_rater1vs2$rater1, df_rater1vs2$rater2, method = "pearson")
  pearson_results_list[[i]] <- pearson_results
  
  # Compute R square
  lm_model <- lm(rater2 ~ rater1, data = df_rater1vs2)
  r2_value <- summary(lm_model)$r.squared
  label_expr <- sprintf("R^2 == %.2f", r2_value)

  # Passing-Bablok regression
  pb_result <- PBreg(df_rater1vs2$rater1, df_rater1vs2$rater2)
  pb_results_list[[i]] <- pb_result

  # Deriving PB plot statistics for plotting - Preparing for x and y axes coordinates
  common_min <- min(c(df_rater1vs2$rater1, df_rater1vs2$rater2))
  common_min <- floor(common_min / 5) * 5
  common_max <- max(c(df_rater1vs2$rater1, df_rater1vs2$rater2))
  common_max <- ceiling(common_max / 5) * 5
  common_breaks <- seq(from = common_min, to = common_max, length.out = 5)
  
  # Deriving PB plot statistics for plotting - Calculate intercept and slope
  pb_intercept <- pb_result$coefficients["Intercept", "Estimate"]
  pb_slope <- pb_result$coefficients["Slope", "Estimate"]
  
  x_vals <- seq(from = common_min, to = common_max, length.out = 10000)
  preds <- predict(pb_result, newdata=x_vals)
  upper_bound <- preds$upr
  upper_bound <- pmin(upper_bound, common_max)
  lower_bound <- preds$lwr
  lower_bound <- pmax(lower_bound, common_min)
  ribbon_data <- data.frame(
    x = x_vals,
    ymin = lower_bound,
    ymax = upper_bound
  )

  # Generating PB plot
  pb_plot_result <- ggplot(
    data = df_rater1vs2, aes(x = rater1, y = rater2)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = ymin, ymax = ymax),
                fill = "#253494", alpha = 0.2, inherit.aes = FALSE)+
    geom_abline(intercept = 0, slope = 1,
                color = "orange", linewidth = 0.25, linetype="dashed") +
    geom_abline(intercept = pb_intercept, slope = pb_slope,
                color = "#253494", linewidth = 1) +
    geom_point(shape = 21, colour = "black", fill = "white", size = 2) +
    labs(title = column_titles_1[[i]], x = input_names[[1]], y = input_names[[2]]) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      plot.title = element_text(hjust = 0.5),
      aspect.ratio = 1
    ) +
    coord_fixed(ratio = 1) +
    scale_x_continuous(limits = c(common_min, common_max), breaks = common_breaks, expand = c(0,0)) +
    scale_y_continuous(limits = c(common_min, common_max), breaks = common_breaks, expand = c(0,0)) +
    geom_text(inherit.aes = FALSE, x = common_min + ((common_max - common_min)*0.05), y = common_max - ((common_max - common_min)*0.05), label = label_expr, hjust = 0, vjust = 1, parse = TRUE)

  # Store the completed plot in list
  pb_plot_results_list[[i]] <- pb_plot_result
  
  # Bland Altman analysis
  ba_df <- baize_df(df_rater1vs2)
  
  # BA plot - Plotting difference over mean
  ba_x_label <- paste("Mean of", input_names[[1]], "and", input_names[[2]])
  ba_y_label <- paste(input_names[[1]], "-", input_names[[2]])
  ba_title_label <- column_titles_1[[i]]
  my_ba_plot_result <- my_baplot(ba_df$mean, ba_df$difference, ba_x_label, ba_y_label, ba_title_label)
  ba_plot_results_list[[i]] <- my_ba_plot_result
  
  # Percent BA plot - Plotting percentage_difference over mean
  pba_x_label <- paste("Mean of", input_names[[1]], "and", input_names[[2]])
  pba_y_label <- paste("(", input_names[[1]], "-", input_names[[2]], ")", "/ Average %")
  pba_title_label <- column_titles_1[[i]]
  my_pba_plot_result <- my_baplot(ba_df$mean, ba_df$percentage_difference, pba_x_label, pba_y_label, pba_title_label)
  pba_plot_results_list[[i]] <- my_pba_plot_result
  
  # Histogram of difference for normality testing
  histo_data <- data.frame(values = ba_df$difference)
  absolute_max_diff <- max(abs(min(ba_df$difference)), abs(max(ba_df$difference)))
  hst_x_min <- -ceiling(absolute_max_diff / 5) * 5
  hst_x_max <- ceiling(absolute_max_diff / 5) * 5
  hst_x_breaks <- seq(from = hst_x_min, to = hst_x_max, length.out = 5)
  
  histo_plot <- ggplot(
    histo_data, aes(x=values)) +
    geom_histogram(aes(y=..ndensity.. * 100), binwidth=(hst_x_max - hst_x_min)*0.025, fill="#4287f5", color = "black", linewidth = 0.2, alpha=1) +
    geom_density(aes(y=..ndensity.. * 100), color="orange", linewidth = 1, alpha=1, linetype="dashed") +
    geom_vline(xintercept = 0, color = "red", linewidth = 0.1, linetype="dashed") +
    labs(title = column_titles_1[[i]],
         x = paste(input_names[[1]], "-", input_names[[2]]),
         y = "Relative frequency (%)") +
    theme_minimal()+
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      plot.title = element_text(hjust = 0.5),
      aspect.ratio = 1
    ) +
    scale_x_continuous(limits = c(hst_x_min, hst_x_max), breaks = hst_x_breaks, expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))
  
  hst_plot_results_list[[i]] <- histo_plot
  
  # Normality testing with Shapiro-Wilk test
  shapiro_results <- shapiro.test(ba_df$difference)
  shapiro_results_list[[i]] <- shapiro_results
  
  # Normality testing with D’Agostino-Pearson test
  agostino_results <- agostino.test(ba_df$difference)
  agostino_results_list[[i]] <- agostino_results
  
  # This part was hidden as KS test not suitable for data with ties
  # Normality testing with Kolmogorov-Smirnov test
  # ks_results <- ks.test(ba_df$difference, "pnorm", mean=mean(ba_df$difference), sd=sd(ba_df$difference))
  # ks_results_list[[i]] <- ks_results
  
  
  # ADDENDUM 15/02/2024
  # This part added polynomial regression in particular cubic & quadratic regression
  
  ## Quadratic model
  formula_str_quad <- paste(names(df_rater1vs2)[2], "~ poly(", names(df_rater1vs2)[1], ", 2, raw=TRUE)")
  model_quad <- lm(as.formula(formula_str_quad), data = df_rater1vs2)
  summary_model_quad <- summary(model_quad)
  
  quad_fstats <- summary_model_quad$fstatistic
  quad_pvalue <- pf(quad_fstats["value"], quad_fstats["numdf"], quad_fstats["dendf"], lower.tail = FALSE)
  quad_r2_value <- summary_model_quad$r.squared
  quad_label_expr <- sprintf("R^2 == %.2f", quad_r2_value)
  quad_label_expr_p <- sprintf("p == %.2f", quad_pvalue)
  
  quad_x_coord <- common_max - ((common_max - common_min) * 0.05)
  quad_y_coord <- common_max - ((common_max - common_min) * 0.05)
  quad_y_coord_p <- quad_y_coord - ((common_max - common_min) * 0.05)
  quad_text_data <- data.frame(x = c(quad_x_coord, quad_x_coord),
                               y = c(quad_y_coord, quad_y_coord_p),
                               label = c(quad_label_expr, quad_label_expr_p))
  
  x_vals <- seq(from = common_min, to = common_max, length.out = 10000)
  quad_predictions_with_intervals <- predict(model_quad, newdata = data.frame(rater1 = x_vals), interval = "confidence")
  quad_ribbon_data <- data.frame(
    rater1 = x_vals,
    ymin = quad_predictions_with_intervals[, "lwr"],
    ymax = quad_predictions_with_intervals[, "upr"]
  )
  quad_ribbon_data$ymin <- pmax(quad_ribbon_data$ymin, common_min)
  quad_ribbon_data$ymax <- pmin(quad_ribbon_data$ymax, common_max)
  
  quad_new_data <- data.frame(
    rater1 = x_vals, 
    predicted = quad_predictions_with_intervals[, "fit"]
  )

  quad_model_plot_result <- ggplot() +
    geom_ribbon(data = quad_ribbon_data, aes(x = rater1, ymin = ymin, ymax = ymax), fill = "#253494", alpha = 0.2) +
    geom_point(data = df_rater1vs2, aes(x = rater1, y = rater2), shape = 21, colour = "black", fill = "white", size = 2) +  
    geom_line(data = quad_new_data, aes(x = rater1, y = predicted), color = "#253494", linewidth = 1) +
    labs(title = paste("Quadratic fit for",column_titles_1[[i]]), x = paste(input_names[1]), y = paste(input_names[2])) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      plot.title = element_text(hjust = 0.5),
      aspect.ratio = 1
    ) +
    coord_fixed(ratio = 1) +
    scale_x_continuous(limits = c(common_min, common_max), breaks = common_breaks, expand = c(0,0)) +
    scale_y_continuous(limits = c(common_min, common_max), breaks = common_breaks, expand = c(0,0)) +
    geom_text(data = quad_text_data[1, ], aes(x = x, y = y, label = label), hjust = 1, vjust = 1, parse = TRUE) +
    geom_text(data = quad_text_data[2, ], aes(x = x, y = y, label = label), hjust = 1, vjust = 1, parse = TRUE)

  quad_fit_results_list[[i]] <- summary_model_quad
  quad_fit_plot_results_list[[i]] <- quad_model_plot_result

  ## Cubic model
  
  formula_str_cubic <- paste(names(df_rater1vs2)[2], "~ poly(", names(df_rater1vs2)[1], ", 3, raw=TRUE)")
  model_cubic <- lm(as.formula(formula_str_cubic), data = df_rater1vs2)
  summary_model_cubic <- summary(model_cubic)
  
  cubic_fstats <- summary_model_cubic$fstatistic
  cubic_pvalue <- pf(cubic_fstats["value"], cubic_fstats["numdf"], cubic_fstats["dendf"], lower.tail = FALSE)
  cubic_r2_value <- summary_model_cubic$r.squared
  cubic_label_expr <- sprintf("R^2 == %.2f", cubic_r2_value)
  cubic_label_expr_p <- sprintf("p == %.2f", cubic_pvalue)
  
  cubic_x_coord <- common_max - ((common_max - common_min) * 0.05)
  cubic_y_coord <- common_max - ((common_max - common_min) * 0.05)
  cubic_y_coord_p <- cubic_y_coord - ((common_max - common_min) * 0.05)
  cubic_text_data <- data.frame(x = c(cubic_x_coord, cubic_x_coord),
                                y = c(cubic_y_coord, cubic_y_coord_p),
                                label = c(cubic_label_expr, cubic_label_expr_p))
  
  x_vals <- seq(from = common_min, to = common_max, length.out = 10000)
  cubic_predictions_with_intervals <- predict(model_cubic, newdata = data.frame(rater1 = x_vals), interval = "confidence")
  cubic_ribbon_data <- data.frame(
    rater1 = x_vals,
    ymin = cubic_predictions_with_intervals[, "lwr"],
    ymax = cubic_predictions_with_intervals[, "upr"]
  )
  cubic_ribbon_data$ymin <- pmax(cubic_ribbon_data$ymin, common_min)
  cubic_ribbon_data$ymax <- pmin(cubic_ribbon_data$ymax, common_max)
  
  cubic_new_data <- data.frame(
    rater1 = x_vals, 
    predicted = cubic_predictions_with_intervals[, "fit"]
  )
  
  cubic_model_plot_result <- ggplot() +
    geom_ribbon(data = cubic_ribbon_data, aes(x = rater1, ymin = ymin, ymax = ymax), fill = "#253494", alpha = 0.2) +
    geom_point(data = df_rater1vs2, aes(x = rater1, y = rater2), shape = 21, colour = "black", fill = "white", size = 2) +  
    geom_line(data = cubic_new_data, aes(x = rater1, y = predicted), color = "#253494", linewidth = 1) +
    labs(title = paste("Cubic fit for",column_titles_1[[i]]), x = paste(input_names[1]), y = paste(input_names[2])) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      plot.title = element_text(hjust = 0.5),
      aspect.ratio = 1
    ) +
    coord_fixed(ratio = 1) +
    scale_x_continuous(limits = c(common_min, common_max), breaks = common_breaks, expand = c(0,0)) +
    scale_y_continuous(limits = c(common_min, common_max), breaks = common_breaks, expand = c(0,0)) +
    geom_text(data = cubic_text_data[1, ], aes(x = x, y = y, label = label), hjust = 1, vjust = 1, parse = TRUE) +
    geom_text(data = cubic_text_data[2, ], aes(x = x, y = y, label = label), hjust = 1, vjust = 1, parse = TRUE)
  
  cubic_fit_results_list[[i]] <- summary_model_cubic
  cubic_fit_plot_results_list[[i]] <- cubic_model_plot_result
  
}

# Render the Rmd to PDF
rmarkdown::render("report.Rmd", quiet = TRUE)