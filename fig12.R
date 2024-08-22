library(ggplot2)

if (length(which(is.na(group_fairness_summary[,1,1]))) > 0){
  group_fairness_summary[which(is.na(group_fairness_summary[,1,1])), 1, ] <- 
    no_fairness_summary[which(is.na(group_fairness_summary[,2,1])), ]
}

if (length(which(is.na(group_fairness_summary[,2,1]))) > 0){
  group_fairness_summary[which(is.na(group_fairness_summary[,2,1])), 2, ] <- 
    group_fairness_summary[which(is.na(group_fairness_summary[,2,1])), 1, ]
}
NN <- paste0("No fairness \noverall utility = ", round(mean(no_fairness_summary[, 1]), 2))
GN1 <- paste0("Group fairness (strong)\noverall utility = ", round(mean(group_fairness_summary[, 2, 1]), 2))
IN1 <- paste0("Individual fairness (strong)\noverall utility = ", round(mean(individual_fairness_summary[, 1, 1]), 2))
ON1 <- paste0("Our fairness (strong)\noverall utility = ", round(mean(race_fairness_summary[, 1, 1]), 2))
GN2 <- paste0("Group fairness (weak)\noverall utility = ", round(mean(group_fairness_summary[, 1, 1]), 2))
IN2 <- paste0("Individual fairness (weak)\noverall utility = ", round(mean(individual_fairness_summary[, 2, 1]), 2))
ON2 <- paste0("Our fairness (weak)\noverall utility = ", round(mean(race_fairness_summary[, 2, 1]), 2))

ct <- c(1, 1, 1, 1, 1, 1)

Group <- rep(c(NN, GN1, GN2, IN1, IN2, ON1, ON2), each = 3)
PRA <- rep(c("High", "Medium", "Low"), 7)
White <- c(mean(no_fairness_summary[, 2]) / ct[1], 
           mean(no_fairness_summary[, 4]) / ct[2],
           mean(no_fairness_summary[, 6]) / ct[3],
           mean(group_fairness_summary[, 2, 2]) / ct[1], 
           mean(group_fairness_summary[, 2, 4]) / ct[2],
           mean(group_fairness_summary[, 2, 6]) / ct[3],
           mean(group_fairness_summary[, 1, 2]) / ct[1], 
           mean(group_fairness_summary[, 1, 4]) / ct[2],
           mean(group_fairness_summary[, 1, 6]) / ct[3],
           mean(individual_fairness_summary[, 1, 2]) / ct[1], 
           mean(individual_fairness_summary[, 1, 4]) / ct[2],
           mean(individual_fairness_summary[, 1, 6]) / ct[3],
           mean(individual_fairness_summary[, 2, 2]) / ct[1], 
           mean(individual_fairness_summary[, 2, 4]) / ct[2],
           mean(individual_fairness_summary[, 2, 6]) / ct[3],
           mean(race_fairness_summary[, 1, 2]) / ct[1], 
           mean(race_fairness_summary[, 1, 4]) / ct[2],
           mean(race_fairness_summary[, 1, 6]) / ct[3],
           mean(race_fairness_summary[, 2, 2]) / ct[1], 
           mean(race_fairness_summary[, 2, 4]) / ct[2],
           mean(race_fairness_summary[, 2, 6]) / ct[3])
Non_white <- c(mean(no_fairness_summary[, 3]) / ct[4], 
               mean(no_fairness_summary[, 5]) / ct[5],
               mean(no_fairness_summary[, 7]) / ct[6],
               mean(group_fairness_summary[, 2, 3]) / ct[4], 
               mean(group_fairness_summary[, 2, 5]) / ct[5],
               mean(group_fairness_summary[, 2, 7]) / ct[6],
               mean(group_fairness_summary[, 1, 3]) / ct[4], 
               mean(group_fairness_summary[, 1, 5]) / ct[5],
               mean(group_fairness_summary[, 1, 7]) / ct[6],
               mean(individual_fairness_summary[, 1, 3]) / ct[4], 
               mean(individual_fairness_summary[, 1, 5]) / ct[5],
               mean(individual_fairness_summary[, 1, 7]) / ct[6],
               mean(individual_fairness_summary[, 2, 3]) / ct[4], 
               mean(individual_fairness_summary[, 2, 5]) / ct[5],
               mean(individual_fairness_summary[, 2, 7]) / ct[6],
               mean(race_fairness_summary[, 1, 3]) / ct[4], 
               mean(race_fairness_summary[, 1, 5]) / ct[5],
               mean(race_fairness_summary[, 1, 7]) / ct[6],
               mean(race_fairness_summary[, 2, 3]) / ct[4], 
               mean(race_fairness_summary[, 2, 5]) / ct[5],
               mean(race_fairness_summary[, 2, 7]) / ct[6])
Abs_Diff <- c(mean(abs(no_fairness_summary[, 2] / ct[1] - no_fairness_summary[, 3] / ct[4])), 
              mean(abs(no_fairness_summary[, 4] / ct[2] - no_fairness_summary[, 5] / ct[5])),
              mean(abs(no_fairness_summary[, 6] / ct[3] - no_fairness_summary[, 7] / ct[6])),
              mean(abs(group_fairness_summary[, 2, 2] / ct[1] - group_fairness_summary[, 2, 3] / ct[4])), 
              mean(abs(group_fairness_summary[, 2, 4] / ct[2] - group_fairness_summary[, 2, 5] / ct[5])),
              mean(abs(group_fairness_summary[, 2, 6] / ct[3] - group_fairness_summary[, 2, 7] / ct[6])),
              mean(abs(group_fairness_summary[, 1, 2] / ct[1] - group_fairness_summary[, 1, 3] / ct[4])), 
              mean(abs(group_fairness_summary[, 1, 4] / ct[2] - group_fairness_summary[, 1, 5] / ct[5])),
              mean(abs(group_fairness_summary[, 1, 6] / ct[3] - group_fairness_summary[, 1, 7] / ct[6])),
              mean(abs(individual_fairness_summary[, 1, 2] / ct[1] - individual_fairness_summary[, 1, 3] / ct[4])), 
              mean(abs(individual_fairness_summary[, 1, 4] / ct[2] - individual_fairness_summary[, 1, 5] / ct[5])),
              mean(abs(individual_fairness_summary[, 1, 6] / ct[3] - individual_fairness_summary[, 1, 7] / ct[6])),
              mean(abs(individual_fairness_summary[, 2, 2] / ct[1] - individual_fairness_summary[, 2, 3] / ct[4])), 
              mean(abs(individual_fairness_summary[, 2, 4] / ct[2] - individual_fairness_summary[, 2, 5] / ct[5])),
              mean(abs(individual_fairness_summary[, 2, 6] / ct[3] - individual_fairness_summary[, 2, 7] / ct[6])),
              mean(abs(race_fairness_summary[, 1, 2] / ct[1] - race_fairness_summary[, 1, 3] / ct[4])), 
              mean(abs(race_fairness_summary[, 1, 4] / ct[2] - race_fairness_summary[, 1, 5] / ct[5])),
              mean(abs(race_fairness_summary[, 1, 6] / ct[3] - race_fairness_summary[, 1, 7] / ct[6])),
              mean(abs(race_fairness_summary[, 2, 2] / ct[1] - race_fairness_summary[, 2, 3] / ct[4])), 
              mean(abs(race_fairness_summary[, 2, 4] / ct[2] - race_fairness_summary[, 2, 5] / ct[5])),
              mean(abs(race_fairness_summary[, 2, 6] / ct[3] - race_fairness_summary[, 2, 7] / ct[6])))    

Abs_Diff_sd <- c(sd(abs(no_fairness_summary[, 2] / ct[1] - no_fairness_summary[, 3] / ct[4])), 
                 sd(abs(no_fairness_summary[, 4] / ct[2] - no_fairness_summary[, 5] / ct[5])),
                 sd(abs(no_fairness_summary[, 6] / ct[3] - no_fairness_summary[, 7] / ct[6])),
                 sd(abs(group_fairness_summary[, 2, 2] / ct[1] - group_fairness_summary[, 2, 3] / ct[4])), 
                 sd(abs(group_fairness_summary[, 2, 4] / ct[2] - group_fairness_summary[, 2, 5] / ct[5])),
                 sd(abs(group_fairness_summary[, 2, 6] / ct[3] - group_fairness_summary[, 2, 7] / ct[6])),
                 sd(abs(group_fairness_summary[, 1, 2] / ct[1] - group_fairness_summary[, 1, 3] / ct[4])), 
                 sd(abs(group_fairness_summary[, 1, 4] / ct[2] - group_fairness_summary[, 1, 5] / ct[5])),
                 sd(abs(group_fairness_summary[, 1, 6] / ct[3] - group_fairness_summary[, 1, 7] / ct[6])),
                 sd(abs(individual_fairness_summary[, 1, 2] / ct[1] - individual_fairness_summary[, 1, 3] / ct[4])), 
                 sd(abs(individual_fairness_summary[, 1, 4] / ct[2] - individual_fairness_summary[, 1, 5] / ct[5])),
                 sd(abs(individual_fairness_summary[, 1, 6] / ct[3] - individual_fairness_summary[, 1, 7] / ct[6])),
                 sd(abs(individual_fairness_summary[, 2, 2] / ct[1] - individual_fairness_summary[, 2, 3] / ct[4])), 
                 sd(abs(individual_fairness_summary[, 2, 4] / ct[2] - individual_fairness_summary[, 2, 5] / ct[5])),
                 sd(abs(individual_fairness_summary[, 2, 6] / ct[3] - individual_fairness_summary[, 2, 7] / ct[6])),
                 sd(abs(race_fairness_summary[, 1, 2] / ct[1] - race_fairness_summary[, 1, 3] / ct[4])), 
                 sd(abs(race_fairness_summary[, 1, 4] / ct[2] - race_fairness_summary[, 1, 5] / ct[5])),
                 sd(abs(race_fairness_summary[, 1, 6] / ct[3] - race_fairness_summary[, 1, 7] / ct[6])),
                 sd(abs(race_fairness_summary[, 2, 2] / ct[1] - race_fairness_summary[, 2, 3] / ct[4])), 
                 sd(abs(race_fairness_summary[, 2, 4] / ct[2] - race_fairness_summary[, 2, 5] / ct[5])),
                 sd(abs(race_fairness_summary[, 2, 6] / ct[3] - race_fairness_summary[, 2, 7] / ct[6])))    


data <- data.frame(Group, PRA, White, Non_white, Abs_Diff, 
                   Abs_Diff_sd)
data$PRA <- factor(data$PRA, levels = c("High", "Medium", "Low"))
data$Group <- factor(data$Group, levels = c(GN1, GN2, IN1, IN2, ON1, ON2, NN))

# Melt the data for plotting
library(reshape2)
melted_data <- melt(data, id.vars = c("Group", "PRA"), variable.name = "Metric", value.name = "Rate")

# Separate the standard deviation data
sd_data <- melt(data, id.vars = c("Group", "PRA"), measure.vars = c("Abs_Diff_sd"), variable.name = "Metric", value.name = "SD")
sd_data$Metric <- gsub("_sd", "", sd_data$Metric)

# Merge the data with standard deviations
melted_data <- merge(melted_data, sd_data, by = c("Group", "PRA", "Metric"), all.x = T)


# Plotting with error bars
ggplot(melted_data, aes(x = PRA, y = Rate, color = Metric, group = Metric, linetype = Metric, shape = Metric)) +
  geom_point(data = subset(melted_data, Metric != "Abs_Diff_sd")) +
  geom_line(data = subset(melted_data, Metric != "Abs_Diff_sd")) +
  geom_errorbar(data = subset(melted_data, Metric == "Abs_Diff"), aes(ymin = pmax(Rate - SD, 0), ymax = Rate + SD), width = 0.1) +
  facet_wrap(~ Group, scales = "fixed", nrow = 3, ncol = 3) +
  labs(y = "Selection Rate",
       x = "PRA") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(size = 40, hjust = 0.5)) +
  scale_linetype_manual(values = c("White" = "solid", "Non_white" = "dashed", "Abs_Diff" = "dotdash")) +
  scale_shape_manual(values = c("White" = 16, "Non_white" = 17, "Abs_Diff" = 18))
