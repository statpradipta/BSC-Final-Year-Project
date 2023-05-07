# Import the data
data <- read.csv("data.csv")

# Calculate Weight_loss as Starting_weight - Current_weight
data$Weight_loss <- data$Starting_weight - data$Current_weight

# Calculate descriptive statistics
exercise_mean <- mean(data$Exercise)
exercise_sd <- sd(data$Exercise)
weight_loss_mean <- mean(data$Weight_loss)
weight_loss_sd <- sd(data$Weight_loss)

# Create a scatterplot of exercise vs. weight loss
library(ggplot2)
library(tidyverse)
ggplot(data, aes(x = Exercise, y = Weight_loss)) +
  geom_point(size = 4, alpha = 0.8, color = "#0072B2") +
  geom_smooth(method = "lm", se = FALSE, color = "#D55E00", size = 1.5) +
  labs(x = "Exercise (Days per week)", y = "Weight Loss (kg)", title = "Exercise vs. Weight Loss in Young Adults",
       subtitle = "Linear Regression Analysis") +
  theme_bw() +
  theme(plot.title = element_text(color = "#0072B2", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#0072B2", size = 14),
        axis.title.x = element_text(color = "#0072B2", size = 14, face = "bold"),
        axis.title.y = element_text(color = "#0072B2", size = 14, face = "bold"),
        axis.text = element_text(color = "#0072B2", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(color = "#0072B2", size = 12))
# Calculate correlation between exercise and weight loss
correlation <- cor(data$Exercise, data$Weight_loss)

# Perform linear regression analysis
fit <- lm(Weight_loss ~ Exercise, data = data)
summary(fit)

# Print descriptive statistics and correlation
cat("Exercise mean:", exercise_mean, "\n")
cat("Exercise standard deviation:", exercise_sd, "\n")
cat("Weight loss mean:", weight_loss_mean, "\n")
cat("Weight loss standard deviation:", weight_loss_sd, "\n")
cat("Correlation between exercise and weight loss:", correlation, "\n")

