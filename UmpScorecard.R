library(grid)
library(ggplot2)
library(ggtext)
library(mgcv)
library(broom)
library(modelr)
library(tidyverse)
setwd("~/Documents/Hyannis")

# Set Umpire Name
ump_name <- "Marvic Gomez"
last_name <- substring(ump_name, regexpr(" ", ump_name)+1)[1]

# Read in Run Environment Data
re288 <- read_csv("re288_cape.csv")

# Read in Trackman File
data <- read_csv("Trackman/HYA_July_24.csv")

# Filter for called pitches, determine whether in rulebook zone or not and whether correct call was made
calls <- data %>% filter(PitchCall %in% c("BallCalled", "StrikeCalled")) %>% select(PitchNo, Pitcher, Batter, BatterTeam, Inning, `Top/Bottom`, Balls, Strikes, Outs, PitchCall, PlateLocSide, PlateLocHeight, GameID)
calls <- calls %>% mutate(
  InZone = ifelse(abs(PlateLocSide) <= 0.83 & PlateLocHeight > 1.479 & PlateLocHeight <= 3.5, 1, 0),
  CallNum = ifelse(PitchCall == "StrikeCalled", 1, 0),
  CallScore = InZone - CallNum
)
misses <- calls %>% filter(CallScore != 0)

# Read in typical CCBL Strike Zone
strike_mod <- read_rds("strike_mod.rds")
grid_hats <- read.csv("gridhats.csv") %>% filter(.fitted >= 0.5)
call_hats <- strike_mod %>%
  augment(type.predict = "response", newdata = calls)

# Determine whether calls were correct based on CCBL Typical Strike Zone
typ_zone <- call_hats %>% mutate(InTypZone = ifelse(.fitted >= 0.5,1,0)) %>% 
  mutate(TypZoneCorrect = ifelse(InTypZone == 1 & PitchCall == "BallCalled" | InTypZone == 0 & PitchCall == "StrikeCalled", 0, 1)) 
typ_zone_acc <- mean(typ_zone$TypZoneCorrect, na.rm = TRUE)

# Calculate overall accuracy
accuracy <- 1 - dim(misses)[1] / dim(calls)[1]
accu_text <- paste0("True Zone\n Accuracy:\n ", round(accuracy * 100, 1), "%")
caccu_text <- paste0("Typical CCBL\n Zone Consistency:\n ", round(typ_zone_acc * 100, 1), "%")

# Calculate ball and strike accuracy
true_strikes <- sum(calls$InZone, na.rm = TRUE)
true_balls <- dim(calls)[1] - true_strikes
true_balls_called_strikes <- sum(misses$CallScore == -1)
true_strikes_called_balls <- sum(misses$CallScore == 1)
ball_acc <- 1 - true_balls_called_strikes / true_balls
strike_acc <- 1 - true_strikes_called_balls / true_strikes
baccu_text <- paste0("True balls\n called balls:\n", round(ball_acc * 100, 1), "%")
saccu_text <- paste0("True strikes\n called strikes:\n", round(strike_acc * 100, 1), "%")

# Finds the impact of a missed call
find_impact <- function(balls, strikes, outs, bases, ball) {
  fil <- re288 %>% filter(Balls == balls, Strikes == strikes, Bases == bases, Outs == outs) 
  if (ball == "BallCalled") {
    res <- fil %>% pull(IncBall) - fil %>% pull(IncStrike)
  } else {
    res <- fil %>% pull(IncStrike) - fil %>% pull(IncBall)
  }
  res
  
}

# Write a CSV of all missed calls
misses <- misses %>% mutate(Bases = "0")
misses %>% select(Inning, `Top/Bottom`, Batter, Balls, Strikes, Outs, Bases) -> m
write_csv(m, "misses.csv")


### STOP HERE ###

# Read in csv with baserunners
m <- read_csv("misses 7-24.csv")
misses$Bases <- m$Bases

# Calculate impact of missed calls
misses <- misses %>% mutate(RunImpact = mapply(find_impact, balls = misses$Balls, strikes = misses$Strikes, outs = misses$Outs, bases = misses$Bases, ball = misses$PitchCall))

# Calculate score
road_team <- substring(data$AwayTeam, 1, regexpr("_", data$AwayTeam) - 1)[1]
home_team <- substring(data$HomeTeam, 1, regexpr("_", data$HomeTeam) - 1)[1]
road_runs <- data %>% filter(BatterTeam == AwayTeam) %>% summarise(total_runs = sum(RunsScored)) %>% pull(total_runs)
home_runs <- data %>% filter(BatterTeam == HomeTeam) %>% summarise(total_runs = sum(RunsScored)) %>% pull(total_runs)
scoreline <- paste0(road_team, " (", road_runs, ") at ", home_team, " (", home_runs, ")")

# Calculate run impact by team
team_favors <- misses %>% group_by(BatterTeam) %>% summarise(TeamFavor = sum(RunImpact))
ben_home <- team_favors %>% filter(BatterTeam == data$HomeTeam[1]) %>% pull(TeamFavor)
ben_away <- team_favors %>% filter(BatterTeam == data$AwayTeam[1]) %>% pull(TeamFavor)
benefits <- paste(road_team, ifelse(ben_away < 0, "minus", "plus"), abs(round(ben_away,3)), "runs\n", home_team, ifelse(ben_home < 0, "minus", "plus"), abs(round(ben_home,3)), "runs\n")
ovr_ben <- paste0(ifelse(ben_away > ben_home, road_team, home_team), " favored by ", round(abs(ben_away - ben_home),3), " runs")

# Find Most influential missed call
sorted_miss <- misses %>% arrange(desc(abs(RunImpact))) 
base_state <- sorted_miss[1,]$Bases
base_text <- ifelse(base_state == "0", "Bases Empty", ifelse(base_state == "1", "Runner on 1st", ifelse(base_state == "2", "Runner on 2nd", ifelse(base_state == "3", "Runner on 3rd", ifelse(base_state == "12", "Runners on 1st and 2nd", ifelse(base_state == "23", "Runners on 2nd and 3rd", ifelse(base_state == "13", "Runners on 1st and 3rd", "Bases Loaded")))))))

miss1_text <- paste0(sorted_miss[1,]$`Top/Bottom`, " ", sorted_miss[1,]$Inning, ", ", sorted_miss[1,]$Balls, "-",  sorted_miss[1,]$Strikes, " count, ", sorted_miss[1,]$Outs, " out\n", base_text, "\n", sorted_miss[1,]$Pitcher, " to ", 
                     sorted_miss[1,]$Batter, "\nTrue ", ifelse(sorted_miss[1,]$PitchCall == "StrikeCalled", "Ball", "Strike"), " is called a ", ifelse(sorted_miss[1,]$PitchCall == "BallCalled", "Ball", "Strike"),
                     "\n", substring(sorted_miss[1,]$BatterTeam, 1, regexpr("_", sorted_miss[1,]$BatterTeam) - 1)[1], ifelse(sorted_miss[1,]$PitchCall == "StrikeCalled", " minus ", " plus "), round(abs(sorted_miss[1,]$RunImpact),3), " runs\n")

# Find most extreme miss
misses <- misses %>% mutate(Wide = ifelse(abs(PlateLocSide) > 0.83, 1, 0), HiLow = ifelse(PlateLocHeight > 1.479 & PlateLocHeight <= 3.5, 0, 1), 
                  ToSide = abs(0.83 - abs(PlateLocSide)), ToTop = abs(PlateLocHeight - 3.5), ToBot = abs(PlateLocHeight - 1.479)) %>% 
  mutate(ToEdge = ifelse(Wide == HiLow, pmin(ToSide, ToTop, ToBot), 
                                ifelse(Wide == 0 & HiLow == 1, pmin(ToBot, ToTop), abs(ToSide)))) 

sorted_miss2 <- misses %>% arrange(desc(abs(ToEdge))) 
base_state <- sorted_miss2[1,]$Bases
base_text <- ifelse(base_state == "0", "Bases Empty", ifelse(base_state == "1", "Runner on 1st", ifelse(base_state == "2", "Runner on 2nd", ifelse(base_state == "3", "Runner on 3rd", ifelse(base_state == "12", "Runners on 1st and 2nd", ifelse(base_state == "23", "Runners on 2nd and 3rd", ifelse(base_state == "13", "Runners on 1st and 3rd", "Bases Loaded")))))))
miss2_text <- paste0(sorted_miss2[1,]$`Top/Bottom`, " ", sorted_miss2[1,]$Inning, ", ", sorted_miss2[1,]$Balls, "-",  sorted_miss2[1,]$Strikes, " count, ", sorted_miss2[1,]$Outs, " out\n", base_text, "\n", sorted_miss2[1,]$Pitcher, " to ", 
                     sorted_miss2[1,]$Batter, "\nTrue ", ifelse(sorted_miss2[1,]$PitchCall == "StrikeCalled", "Ball", "Strike"), " is called a ", ifelse(sorted_miss2[1,]$PitchCall == "BallCalled", "Ball", "Strike"),
                     "\nPitch ", round(12 * sorted_miss2[1,]$ToEdge, 2), " inches from edge of zone")


# Draw home plate
pentagon <- data.frame(
  x = c(-17 / 24, -17 / 24, 0, 17 / 24, 17 / 24),
  y = c(0, .3, .58, .3, 0)
)

# Draw typical zone
hull_points <- grid_hats[chull(grid_hats), ]

# Plot balls and strikes
umpire_plot <- ggplot(misses) +
  aes(PlateLocSide, PlateLocHeight, color = PitchCall) +
  geom_point(size = 7.95) +
  geom_rect(xmin = -17 / 24, xmax = 17 / 24, ymin = 1.6, ymax = 3.38, col = "blue", alpha = 0, size = 0.2) +
  geom_polygon(data = pentagon, aes(x, y), color = "black", fill = "white", alpha = 0.5) +
  scale_color_manual(values = c("BallCalled" = "green3", "StrikeCalled" = "red"), guide = "none") +
  scale_x_continuous(limits = c(-2, 2), breaks = c(-2, -1, 0, 1, 2), labels = c("-2", "-1", "0", "1", "2"), name = "") +
  scale_y_continuous(limits = c(0, 4.3), name = "") +
  theme(axis.title = element_blank()) +
  geom_text(data = sorted_miss[1,], aes(label = "A"), size = 5, color = "black") + 
  geom_text(data = sorted_miss2[1,], aes(label = "B"), size = 5, color = "black") +
  geom_polygon(data = hull_points, fill = NA, color = "goldenrod", size = 0.75) +
  geom_path(data = hull_points, color = "goldenrod", size = 0.75) 
  #geom_vline(xintercept = c(-0.1621267, -0.4037933), linetype = "solid", color = "orange", size = 0.05) +
  #geom_hline(yintercept = c(3.375577, 3.617243), linetype = "solid", color = "orange", size = 0.05) 



# Name the file and start drawing everything
file_title <- paste0("scorecards/", "Umpire_", last_name, "_", unique(calls$GameID), ".png")
png(file_title, width = 8, height = 10, units = "in", res = 300)

# Use grid library to organize scorecard
grid.newpage()
ly <- grid.layout(20, 8)
pushViewport(viewport(layout = ly))

pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1:8))
grid.text("Cape Cod League Ump Scorecard",  gp = gpar(fontsize = 25, fontface = "bold"))
popViewport()

pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1:8))
grid.text(paste0("Umpire: ", ump_name), gp = gpar(fontsize = 25, fontface = "bold"))
popViewport()

pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1:8))
grid.text(paste0("Umpire: ", ump_name), gp = gpar(fontsize = 25, fontface = "bold"))
popViewport()

pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1:8))
grid.text(scoreline, gp = gpar(fontsize = 20, fontface = "bold"))
popViewport()

pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 1:8))
grid.text(data$Date[10], gp = gpar(fontsize = 20, fontface = "bold"))
popViewport()

pushViewport(viewport(layout.pos.row = 5:7, layout.pos.col = 1:2))
grid.text(accu_text, gp = gpar(fontsize = 18, fontface = "bold"))
popViewport()

pushViewport(viewport(layout.pos.row = 5:7, layout.pos.col = 5:6))
grid.text(baccu_text, gp = gpar(fontsize = 18, fontface = "bold", col = "red"))
popViewport()

pushViewport(viewport(layout.pos.row = 5:7, layout.pos.col = 3:4))
grid.text(saccu_text, gp = gpar(fontsize = 18, fontface = "bold", col = "green3"))
popViewport()

pushViewport(viewport(layout.pos.row = 5:7, layout.pos.col = 7:8))
grid.text(caccu_text, gp = gpar(fontsize = 15, fontface = "bold", col = "goldenrod"))
popViewport()

pushViewport(viewport(layout.pos.row = 8:9, layout.pos.col = 1:4))
grid.text(ovr_ben, gp = gpar(fontsize = 20, fontface = "bold"))
popViewport()

pushViewport(viewport(layout.pos.row = 8:10, layout.pos.col = 6:8))
grid.text(benefits, gp = gpar(fontsize = 18, fontface = "bold"))
popViewport()

print(umpire_plot, vp = viewport(layout.pos.row = 10:20,
                       layout.pos.col = 1:5))

pushViewport(viewport(layout.pos.row = 11, layout.pos.col = 6:8))
grid.text("Most influential missed call (A):\n", gp = gpar(fontsize = 12, fontface = "bold"))
popViewport()

pushViewport(viewport(layout.pos.row = 12:13, layout.pos.col = 6:8))
grid.text(miss1_text, gp = gpar(fontsize = 12))
popViewport()

pushViewport(viewport(layout.pos.row = 15, layout.pos.col = 6:8))
grid.text("Most extreme missed call (B):\n", gp = gpar(fontsize = 12, fontface = "bold"))
popViewport()

pushViewport(viewport(layout.pos.row = 16:17, layout.pos.col = 6:8))
grid.text(miss2_text, gp = gpar(fontsize = 12))
popViewport()

popViewport()

dev.off()
