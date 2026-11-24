# 1c_calculate_probabilistic_threshold.R =======================================
# Apply Platt scaling to species validation data to derive probabilistic thresholds and performance
# ==============================================================================

source("scripts/methods/0_config.R")

# Classifier calibration (Platt scaling)
use_platt_scaling = TRUE
tp_min_prob = 0.95 # Desired minimum probability of true positive
threshold_min_classifier_score = 0.5 # Naive classifier minimum confidence score threshold to assume binary presence/absence. # "For most false-positive models in our study, using a mid-range threshold of 0.50 or above generally yielded stable estimates." (Katsis et al. 2025)

# Species validation data
data = tibble()

# Simulate species validation data for demonstration (replace `data` with your own species validation data)
set.seed(1)
label = "Simulated Species"
n = 100
b0 = -4 # intercept (log-odds of true positive at score = 0)
b1 =  8 # slope (log-odds per unit score)
data = tibble(
  score = rbeta(n, shape1 = 1.2, shape2 = 1.5)
) %>% mutate(
  presence = rbinom(n, size = 1, prob = plogis(b0 + b1 * score))
) %>% arrange(score)

# Check data validity
stopifnot(all(unique(data$presence) %in% c(0, 1)))

# Convert confidence scores to logit scale
data = data %>% mutate(score_logit = sapply(score, conf_to_logit))

# Visualize data score distribution
logit_breaks = floor(min(data$score_logit)):ceiling(max(data$score_logit))
plt = ggplot() +
  labs(title = paste(label, "validation data score distribution")) +
  geom_point(data = data, aes(x = score, y = presence), shape = 1, alpha = 0.5) +
  scale_x_continuous(
    name   = "Score (confidence scale)",
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1),
    sec.axis = dup_axis(
      name   = "Score (logit scale)",
      breaks = logit_to_conf(logit_breaks),
      labels = logit_breaks
    )
  ); print(plt)

# Calculate PR AUC and ROC AUC of classifier (from raw scores)
auc_pr = pr.curve(scores.class0 = subset(data, presence == 1) %>% pull(score_logit),
                  scores.class1 = subset(data, presence == 0) %>% pull(score_logit))$auc.integral

auc_roc = roc.curve(scores.class0 = subset(data, presence == 1) %>% pull(score_logit),
                    scores.class1 = subset(data, presence == 0) %>% pull(score_logit))$auc

message("  PR-AUC ", round(auc_pr,3), ", ROC-AUC ", round(auc_roc,3))

# Perform Platt scaling to determine threshold for desired probability of true positive
threshold = Inf # default threshold is infinite (i.e. do not retain detections unless a species is validated)
precision_threshold <- recall_threshold <- precision_tmin <- recall_tmin <- NA
model_warning = FALSE
tryCatch(
  {
    regression = glm(presence ~ score_logit, data, family = binomial(link = "logit"))
    intercept   = as.numeric(coef(regression)[1])
    coefficient = as.numeric(coef(regression)[2])
    threshold_logit = (log(tp_min_prob / (1 - tp_min_prob)) - intercept) / coefficient # logit scale
    threshold       = logit_to_conf(threshold_logit) # confidence scale
    
    message("Threshold to achieve Pr(TP)>=", tp_min_prob, ":  ", round(threshold, 3))
    
    # Calculate estimated precision and recall at this threshold
    # Predicted positive/negative based on threshold
    calc_precision_recall = function(d, t_logit) {
      predicted = ifelse(d$score_logit >= t_logit, 1, 0)
      TP = sum(predicted == 1 & d$presence == 1)
      FP = sum(predicted == 1 & d$presence == 0)
      FN = sum(predicted == 0 & d$presence == 1)
      return(data.frame(
        precision = TP / (TP + FP),
        recall    = TP / (TP + FN)
      ))
    }
    perf_t = calc_precision_recall(data, threshold_logit)
    precision_threshold  = perf_t$precision
    recall_threshold     = perf_t$recall
    message("  Performance at threshold ", round(threshold,3), ":\n  Precision ", round(precision_threshold,3), "\n  Recall ", round(recall_threshold,3))
    
    # Calculate at minimum threshold
    perf_tmin = calc_precision_recall(data, conf_to_logit(threshold_min_classifier_score))
    precision_tmin = perf_tmin$precision
    recall_tmin    = perf_tmin$recall
    message("  Performance at minimum threshold ", threshold_min_classifier_score, ":\n  Precision ", round(precision_tmin,3), "\n  Recall ", round(recall_tmin,3))
    
    ## Visualize precision and recall performance as a function of score
    if (TRUE) {
      data_sorted = data %>% arrange(desc(score_logit))
      n_pos = sum(data_sorted$presence == 1)
      data_sorted = data_sorted %>% mutate(
        tp = cumsum(presence == 1),
        fp = cumsum(presence == 0),
        recall = tp / n_pos,
        precision = ifelse(tp + fp == 0, 1, tp / (tp + fp))  # handle division by zero
      )
      plt = ggplot(data_sorted, aes(x = score)) +
        geom_line(aes(y = precision, color = "Precision"), linewidth = 1) +
        geom_line(aes(y = recall, color = "Recall"), linewidth = 1) +
        scale_x_continuous(
          name   = "Score (confidence scale)",
          breaks = seq(0, 1, 0.1),
          limits = c(0, 1),
          sec.axis = dup_axis(
            name   = "Score (logit scale)",
            breaks = logit_to_conf(logit_breaks),
            labels = logit_breaks
          )) +
        labs(title = paste0(label), x = "Score", y = "Performance", color = "Metric"); print(plt)
      
      ## Visualize the logistic regression and data
      x_range = seq(min(data$score_logit), max(data$score_logit), length.out = 100)
      
      # Predict fitted values and calculate confidence intervals on probability scale
      pred = predict(regression, newdata = data.frame(score_logit = x_range), type = "link", se.fit = TRUE)
      
      # Calculate confidence intervals on probability scale
      z = qnorm(0.975)  # 95% CI
      upper  = logit_to_conf(pred$fit + z * pred$se.fit)
      lower  = logit_to_conf(pred$fit - z * pred$se.fit)
      y_pred = logit_to_conf(pred$fit)
      
      regression_df = data.frame(score = x_range, prob = y_pred, lower = lower, upper = upper, warning = model_warning)
      plt = ggplot() +
        geom_vline(xintercept = 0, color = "gray90", linetype = "solid", linewidth = 0.5) +
        geom_hline(yintercept = tp_min_prob, color = "black", linetype = "dashed", linewidth = 0.5) +
        geom_vline(xintercept = threshold_logit, color = "black", linetype = "dashed", linewidth = 0.5) +
        geom_ribbon(data = regression_df, aes(x = score, ymin = lower, ymax = upper, fill = warning), alpha = 0.2) +
        geom_line(data = regression_df, aes(x = score, y = prob, color = warning), linewidth = 1) +
        geom_point(data = data, aes(x = score_logit, y = presence), shape = 1, alpha = 0.5) +
        scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
        scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
        scale_x_continuous(breaks = logit_breaks) +
        labs(
          x = "Score (logit)", y = "True positive probability",
          title = paste0(label), subtitle = paste0("Threshold p(TP) ≥ ", tp_min_prob, " = ", round(threshold, 3))
        ); print(plt)
    }
  },
  warning = function(w) {
    model_warning <<- TRUE
    message(crayon::yellow("WARNING", conditionMessage(w)))
  },
  error = function(e) {
    stop(crayon::red("ERROR", conditionMessage(e)))
  }
)

calibration_results = data.frame(
  common_name   = label,
  n_pos         = sum(data$presence == 1),
  n_neg         = sum(data$presence == 0),
  auc_pr        = auc_pr,
  auc_roc       = auc_roc,
  warning       = model_warning,
  tp_min_prob   = tp_min_prob,
  threshold     = threshold,
  threshold_min = threshold_min_classifier_score,
  precision_threshold  = precision_threshold,
  recall_threshold     = recall_threshold,
  precision_tmin = precision_tmin,
  recall_tmin    = recall_tmin
)
print(calibration_results)
