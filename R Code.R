# Install packages
#install.packages("tidyverse")

# Call the packages
library(tidyverse)

# Import the data
data <- read.csv("data.csv")

# Calculate Weight_loss as Starting_weight - Current_weight
data$Weight_loss <- data$Starting_weight - data$Current_weight

# Perform Shapiro-Wilk test
shapiro.test(data$Starting_weight)
shapiro.test(data$Current_weight)
shapiro.test(data$Weight_loss)

# Calculate descriptive statistics
exercise_mean <- mean(data$Exercise)
exercise_sd <- sd(data$Exercise)

inweight_mean<-mean(data$Starting_weight)
inweight_sd<-sd(data$Starting_weight)

curweight_mean<-mean(data$Current_weight)
curweight_sd<-sd(data$Current_weight)

weight_loss_mean <- mean(data$Weight_loss)
weight_loss_sd <- sd(data$Weight_loss)

# Set color for the boxplots
my_color <- c("#FF8C00", "#4169E1")  # Orange and Royal Blue

par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 2, 0))

# Boxplot of starting weight
boxplot(data$Starting_weight,
        main = "Starting Weight",
        ylab = "Weight (kg)",
        col = my_color[1])

# Boxplot of current weight
boxplot(data$Current_weight,
        main = "Current Weight",
        ylab = "Weight (kg)",
        col = my_color[2])

# Add label to the entire grid
mtext("BOX PLOT OF STARTING AND CURRENT WEIGHTS OF THE PARTICIPANTS", outer = TRUE, cex = 1.5, col = "#007FFF")


# Create a scatterplot of exercise vs. weight loss
ggplot(data, aes(x = Exercise, y = Weight_loss)) +
  geom_point(size = 2, alpha = 0.8, color = "#0072B2") +
  geom_smooth(method = "lm",se = FALSE, color = "#D55E00", size = 1) +
  labs(x = "Exercise (Days per week) ➯ ", y = "Weight Loss (kg) ➯ ", title = "Exercise vs. Weight Loss in Young Adults",
       subtitle = "Linear Regression Analysis")+
  theme_bw()+
  theme(plot.title = element_text(color = "#007FFF", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#0072B2", size = 14),
        axis.title.x = element_text(color = "#0072B2", size = 14, face = "bold"),
        axis.title.y = element_text(color = "#0072B2", size = 14, face = "bold"),
        axis.text = element_text(color = "#0072B2", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(color = "#0072B2", size = 12))+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")
  

# Calculate correlation between exercise and weight loss
correlation1 <- cor(data$Exercise, data$Weight_loss)

# Calculate the correlation between weight loss and duration of exercise
correlation2 <- cor(data$Weight_loss, data$Duration)

fit_combined <- lm(Weight_loss ~ Exercise + Duration, data = data)
fit_combined
summary(fit_combined)

# Print descriptive statistics and correlation
cat("\n","➲ " ,"Exercise(Days/week) :-","\n", "Mean:", exercise_mean, "\b",",",
"Standard Deviation:", exercise_sd, "\n",

"\n","➲ ","Starting Weight(in kg) :- ","\n", "Mean:", inweight_mean, "\b",",",
    "Standard Deviation:", inweight_sd, "\n",

"\n","➲ ","Final Weight(in kg) :-","\n", "Mean:", curweight_mean, "\b",",",
"Standard Deviation:", curweight_sd, "\n",


"\n","➲ ","Weight loss(in kg) :-","\n", "Mean:", weight_loss_mean, "\b",",",
"Standard Deviation:", weight_loss_sd, "\n",

"\n","➲ ","Correlation between exercise(Days/week) and weight loss:", correlation1, "\b",",","\n",
"\n","➲ ","Correlation between Duration of exercise(in mins) and weight loss:", correlation2, "\b",",","\n")

# Perform ANOVA
anova_result <- aov(data$Weight_loss ~ data$Type.of.Exercise, data)

# Calculate the average weight loss for each exercise type
avg_weight_loss <- aggregate(data$Weight_loss ~ data$Type.of.Exercise, data, mean)

# Find the exercise type with the highest average weight loss
best_exercise <- avg_weight_loss[which.max(avg_weight_loss$`data$Weight_loss`), "data$Type.of.Exercise"]

# Print the result
cat("The One-way Anova Table is as follow:","\n")
summary(anova_result)

cat("The best exercise for weight loss is:\033[1m", best_exercise,"\033[0m\n")


