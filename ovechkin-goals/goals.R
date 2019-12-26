library(dplyr)
library(ggplot2)
library(ggalluvial)

goals_raw <- read.csv("./goal_list.csv")
goals_all <- subset(goals_raw, select=c(index, date, scorer, primary_assist, secondary_assist, career_goal_number, strength, other))

# primary <- as.data.frame(table(unlist(goals_all$primary_assist)), stringsAsFactors=FALSE)
# colnames(primary) <- c("player", "count")
# secondary <- as.data.frame(table(unlist(goals_all$secondary_assist)), stringsAsFactors=FALSE)
# colnames(secondary) <- c("player", "count")
# 
# scoring <- merge(primary, secondary, by="player")
# colnames(scoring) <- c("player", "primary", "secondary")

# Prerpare the data for an alluvial chart
scoring <- dplyr::count(goals_all, scorer, primary_assist, secondary_assist)

presenter <- scoring[which(scoring$n >= 3), names(scoring) %in% c("scorer", "primary_assist", "secondary_assist", "n")]

# Plot
# TODO add fill by PP or ES
ggplot(as.data.frame(presenter),
       aes(axis1 = primary_assist, axis2 = secondary_assist)) +
  geom_alluvium(aes(fill=primary_assist), width = 0, knot.pos = 0) +
  geom_stratum(width = 1/50, fill = "black", color = "grey") +
  geom_label(stat = "stratum", infer.label = TRUE) +
  scale_x_discrete(limits = c("Primary Assist", "Secondary Assist"), expand = c(.05, .05)) +
  ggtitle("Ovechkin's Goals: Scoring Combinations (min 3)") +
  theme(legend.position = "none")

remove(goals_all, goals_raw, presenter, scoring)