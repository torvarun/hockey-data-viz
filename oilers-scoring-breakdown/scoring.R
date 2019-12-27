library(dplyr)
library(ggplot2)
library(ggalluvial)
library(ggthemes)
library(ggfittext)

# function for a match
is_star <- function(name) {
  return (grepl("McDavid", name, fixed = TRUE) | grepl("Draisaitl", name, fixed = TRUE))
}

# my ggplot theme
torvarun_theme <- theme_fivethirtyeight() + theme(
  axis.title = element_text(colour = "#1d1d1d"),
  axis.title.x = element_text(hjust=0.5, colour = "#1d1d1d"),
  axis.title.y = element_text(colour = "#1d1d1d"),
  plot.title = element_text(size = 15, lineheight = 0.9, face = "bold", hjust=0.5, colour = "#1d1d1d"),
  plot.subtitle = element_text(hjust=0.5),
  axis.text = element_text(colour = "#1d1d1d", size = 11),
  legend.text = element_text(colour = "#1d1d1d", size = 11)
)


goals_raw <- read.csv("./edm_goals.csv")
goals_all <- subset(goals_raw, select=c(index, date, scorer, primary_assist, secondary_assist, career_goal_number, strength, other))

# Tally up the scoring combinations
scoring <- dplyr::count(goals_all, scorer, primary_assist, secondary_assist, name = "times")

# Add marker for whether McDavid or Draisaitl contributed to the goal
scoring$stars <- ifelse(is_star(scoring$scorer) | is_star(scoring$primary_assist) | is_star(scoring$secondary_assist), TRUE, FALSE)

# Prepare the data for an alluvial chart
presenter <- scoring[which(scoring$times >= 1), names(scoring) %in% c("scorer", "primary_assist", "secondary_assist", "times", "stars")]
presenter$primary_assist <- ifelse(presenter$primary_assist == "", "Unassisted",  as.character(presenter$primary_assist))
presenter$secondary_assist <- ifelse(presenter$secondary_assist == "", "Unassisted",  as.character(presenter$secondary_assist))

# Plot
ggplot(as.data.frame(presenter),
       aes(axis1 = scorer, axis2 = primary_assist, axis3 = secondary_assist)) +
  #geom_flow(aes(fill = stars), alpha = 0.9) +
  geom_alluvium(aes(fill=stars), aes.bind=TRUE, reverse = FALSE, alpha = 0.9, width = 0.5) +
  scale_fill_manual(labels = c("Neither", "McDavid or Draisaitl"), values = c("#041E42", "#FF4C00"), guide = guide_legend(reverse = TRUE)) +
  geom_stratum(alpha=1, reverse = FALSE, colour = "#1d1d1d", fill = "#f0f0f0") +
  geom_text(stat="stratum", infer.label = TRUE, reverse = FALSE, min.y = 1) +
  #ggfittext::geom_fit_text(stat="stratum", infer.label = TRUE, reverse = FALSE, width = 1/4, min.y = 2) +
  scale_x_discrete(limits = c("Scorer", "Primary Assist", "Secondary Assist"), expand = c(0,0), position = "top") +
  scale_y_continuous(labels = NULL, expand = c(0,0)) +
  labs (
    title = "Oilers' Goal Scoring Combinations",
    subtitle = "2019-20 season until the Christmas Break",
    caption = "Data courtesy of Hockey Reference."
  ) +
  torvarun_theme +
  theme(
    panel.grid.major = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    legend.title = element_blank(),
    plot.subtitle = element_text(colour = "#1d1d1d"),
    plot.caption = element_text(hjust = 0.5, colour = "#1d1d1d", size = 10)
  )

#remove(goals_all, goals_raw, presenter, scoring)