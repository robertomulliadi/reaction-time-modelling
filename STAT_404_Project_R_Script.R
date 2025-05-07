library(dplyr, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(readr, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(MASS, quietly = TRUE)
library(car, quietly = TRUE)

data <- read_csv("reaction_time_data.csv", skip = 7)
colnames(data) <- c("Lighting", "Hand_Used", "Visual_Distraction", 
                    "Auditory_Distraction", "trial_1", "trial_2", "trial_3", 
                    "trial_4", "trial_5", "average_time")
head(data)
summary(data)


options(digits = 4)

# Average response time for each level of Hand Used
average_hand_used <- data |>
  group_by(Hand_Used) |>
  summarise(mean_reaction_time = mean(average_time, na.rm = TRUE))

print("Average reaction time for each hand used:")
print(average_hand_used)

# Average response time for each level of Visual Distraction
average_visual_distraction <- data |>
  group_by(Visual_Distraction) |>
  summarise(mean_reaction_time = mean(average_time, na.rm = TRUE))

print("Average reaction time for each visual distraction level:")
print(average_visual_distraction)

# Average response time for each level of Auditory Distraction
average_auditory_distraction <- data |>
  group_by(Auditory_Distraction) |>
  summarise(mean_reaction_time = mean(average_time, na.rm = TRUE))

print("Average reaction time for each auditory distraction level:")
print(average_auditory_distraction)



data <- data |>
  mutate(
    Hand_Used = as.factor(Hand_Used),
    Visual_Distraction = as.factor(Visual_Distraction),
    Auditory_Distraction = as.factor(Auditory_Distraction)
  )

anova_model <- aov(average_time ~ Lighting + Hand_Used * Visual_Distraction * 
                     Auditory_Distraction, data = data)
summary(anova_model)


par(mfrow = c(3, 1))
options(repr.plot.width = 12, repr.plot.height = 12)

residuals <- residuals(anova_model)

# Histogram of residuals
hist(residuals, 
     breaks = 10, 
     main = "Histogram of Residuals", 
     xlab = "Residuals")

# Q-Q plot
qqnorm(residuals)
qqline(residuals, col = "red")

# Residual Plot (fitted values against residuals)
plot(fitted(anova_model), residuals, 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")


# Boc-Cox
boxcox(anova_model, lambda = seq(-7, 7, 0.01))


# Interaction Plots
par(mfrow = c(3, 1))
# par(mar = c(5.1, 4.1, 4.1, 7))

interaction.plot(
  data$Auditory_Distraction, 
  data$Visual_Distraction, 
  data$average_time,
  col = c("blue", "red"),
  trace.label = "Visual Distraction",
  xlab = "Auditory Distraction", 
  ylab = "Average Reaction Time (ms)",
  main = "Visual Distraction vs. Auditory Distraction")

interaction.plot(
  data$Visual_Distraction, 
  data$Hand_Used, 
  data$average_time,
  col = c("blue", "red"),
  trace.label = "Hand Used",
  xlab = "Visual Distraction", 
  ylab = "Average Reaction Time (ms)",
  main = "Hand Used vs. Visual Distraction")

interaction.plot(
  data$Auditory_Distraction, 
  data$Hand_Used, 
  data$average_time,
  col = c("blue", "red"),
  trace.label = "Hand Used",
  xlab = "Auditory Distraction", 
  ylab = "Average Reaction Time (ms)",
  main = "Hand Used vs. Auditory Distraction")


# Side by side Boxplots
par(mfrow = c(2, 2))

boxplot(average_time ~ Hand_Used, data = data, 
        main = "Effect of Hand Used on Reaction Time", 
        ylab = "Reaction Time (ms)", xlab = "Hand Used")

boxplot(average_time ~ Visual_Distraction, data = data, 
        main = "Effect of Visual Distraction on Reaction Time", 
        ylab = "Reaction Time (ms)", 
        xlab = "Visual Distraction",
        col = c("lightgreen", "lightpink"))

boxplot(average_time ~ Auditory_Distraction, data = data, 
        main = "Effect of Auditory Distraction on Reaction Time", 
        ylab = "Reaction Time (ms)", 
        xlab = "Auditory Distraction",
        col = c("gold", "orange", "red"))

boxplot(average_time ~ Lighting, data = data, 
        main = "Effect of Lighting on Reaction Time", 
        ylab = "Reaction Time (ms)", 
        xlab = "Lighting Condition",
        col = c("grey70", "grey30", "grey50"))


# Half-normal Plot
anova_table <- summary(anova_model)[[1]]
effects <- anova_table$`Sum Sq`[-c(1, nrow(anova_table))] 
names(effects) <- rownames(anova_table)[-c(1, nrow(anova_table))]

sorted_effects <- sort(abs(effects))
labels <- names(sorted_effects)

n <- length(sorted_effects)
theoretical_quantiles <- qnorm((1:n - 0.5) / n, mean = 0, sd = 1)

plot(
  theoretical_quantiles, sorted_effects,
  xlab = "Theoretical Quantiles",
  ylab = "Observed Effects",
  main = "Half-Normal Plot of Effects (Excluding Intercepts)",
  pch = 19,
  xlim = range(theoretical_quantiles) + c(-0.1, 0.5),  # Expand x-axis range
  ylim = range(sorted_effects) + c(-50, 100),         # Expand y-axis range
  cex.lab = 1.5,
  cex.axis = 1.2,
  cex.main = 1.5)

text(
  theoretical_quantiles, sorted_effects,
  labels = labels, pos = 4, cex = 1,   # Adjust text size (cex) here
  offset = 0.5)                        # Add space between points and labels



# Estimation and CI

# recommended levels for optimal average reaction speed
new_data <- data.frame(
  Lighting = factor('Dim'),  
  Hand_Used = factor(1), 
  Visual_Distraction = factor(0), 
  Auditory_Distraction = factor(0)
)

# estimated response given the recommended, with SE
predictions <- predict(anova_model, newdata = new_data, se.fit=TRUE)
predictions

# dof for prediction
df <- df.residual(anova_model)

# 95% confidence interval for estimated response
t_critical <- qt(0.975, df) 
lower_bound <- predictions$fit - t_critical * predictions$se.fit
upper_bound <- predictions$fit + t_critical * predictions$se.fit

confidence_interval <- c(lower_bound, upper_bound)
confidence_interval




