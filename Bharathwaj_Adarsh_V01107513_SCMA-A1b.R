# Load necessary libraries
library(dplyr)
library(readxl)
library(ggplot2)
library(fitdistrplus)
library(stringdist)
library(fuzzyjoin)
library(corrplot)
library(RColorBrewer)

# Read data
ipl_bbb <- read.csv('IPL_ball_by_ball_updated till 2024.csv', stringsAsFactors = FALSE)
ipl_salary <- read_excel('IPL SALARIES 2024.xlsx')

# View the first few rows of the salary data
head(ipl_salary)

# Group and summarize data
grouped_data <- ipl_bbb %>%
  group_by(Season, Innings.No, Striker, Bowler) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE), wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE),.groups = 'drop_last') %>%
  ungroup()

# Summarize player runs and wickets
player_runs <- grouped_data %>%
  group_by(Season, Striker) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE),.groups = 'drop_last') %>%
  ungroup()

player_wickets <- grouped_data %>%
  group_by(Season, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE),.groups = 'drop_last') %>%
  ungroup()

# Filter and sort player runs for the year 2023
player_runs %>%
  filter(Season == '2023') %>%
  arrange(desc(runs_scored))

# Top 3 run getters and wicket takers each season
top_run_getters <- player_runs %>%
  group_by(Season) %>%
  top_n(3, runs_scored) %>%
  arrange(Season,desc(runs_scored))%>%
  ungroup()

top_wicket_takers <- player_wickets %>%
  group_by(Season) %>%
  top_n(3, wicket_confirmation) %>%
  arrange(Season,desc(wicket_confirmation))%>%
  ungroup()

# Print top run getters and wicket takers
print("Top Three Run Getters:")
print(top_run_getters)
print("Top Three Wicket Takers:")
print(top_wicket_takers)

# Adding year to the dataset
ipl_bbb <- ipl_bbb %>%
  mutate(year = as.numeric(format(as.Date(Date, "%d-%m-%Y"), "%Y")))

ipl_bbbc <- ipl_bbb

names(ipl_bbbc)
# View the head of the dataframe with the new 'year' column
head(ipl_bbbc %>% dplyr::select(Match.id, year, Bowler, runs_scored, wicket_confirmation, Striker))

# Function to find best distribution
get_best_distribution <- function(data) {
  dist_names <- c('norm', 'lnorm', 'gamma', 'weibull', 'exp')
  dist_results <- list()
  params <- list()
  
  for (dist_name in dist_names) {
    fit <- try(fitdist(data, dist_name), silent = TRUE)
    if (class(fit) != "try-error") {
      gof <- gofstat(fit)
      dist_results[[dist_name]] <- gof$ks
      params[[dist_name]] <- fit$estimate
    }
  }
  
  best_dist <- names(dist_results)[which.min(unlist(dist_results))]
  best_p <- min(unlist(dist_results))
  best_params <- params[[best_dist]]
  
  cat("Best fitting distribution:", best_dist, "\n")
  cat("Best p value:", best_p, "\n")
  cat("Parameters for the best fit:", best_params, "\n")
  
  return(list(best_dist = best_dist, best_p = best_p, best_params = best_params))
}

# Calculate total runs and wickets each year
total_run_each_year <- ipl_bbbc %>%
  group_by(year, Striker) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE)) %>%
  arrange(year, desc(runs_scored)) %>%
  ungroup()

print(total_run_each_year)

list_top_batsman_last_three_year <- list()
for (i in unique(total_run_each_year$year)[1:3]) {
  list_top_batsman_last_three_year[[as.character(i)]] <- total_run_each_year %>%
    filter(year == i) %>%
    top_n(3, runs_scored) %>%
    pull(Striker)
}
print(list_top_batsman_last_three_year)

# Group by Striker and Match.id
runs <- ipl_bbbc %>%
  group_by(Striker, Match.id) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE),.groups = 'drop_last') %>%
  ungroup()

# Get best distribution for top batsmen
for (key in names(list_top_batsman_last_three_year)) {
  for (Striker in list_top_batsman_last_three_year[[key]]) {
    cat("************************\n")
    cat("year:", key, " Batsman:", Striker, "\n")
    get_best_distribution(runs %>% filter(Striker == Striker) %>% pull(runs_scored))
    cat("\n\n")
  }
}

total_wicket_each_year <- ipl_bbbc %>%
  group_by(year, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE),.groups = 'drop_last') %>%
  arrange(year, desc(wicket_confirmation)) %>%
  ungroup()

print(total_wicket_each_year)

list_top_bowler_last_three_year <- list()
for (i in unique(total_wicket_each_year$year)[1:3]) {
  list_top_bowler_last_three_year[[as.character(i)]] <- total_wicket_each_year %>%
    filter(year == i) %>%
    top_n(3, wicket_confirmation) %>%
    pull(Bowler)
}
print(list_top_bowler_last_three_year)

# Group by Bowler and Match.id
wickets <- ipl_bbbc %>%
  group_by(Bowler, Match.id) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup()

# Get best distribution for top bowlers
for (key in names(list_top_bowler_last_three_year)) {
  for (Bowler in list_top_bowler_last_three_year[[key]]) {
    cat("************************\n")
    cat("year:", key, " Bowler:", Bowler, "\n")
    get_best_distribution(wickets %>% filter(Bowler == Bowler) %>% pull(wicket_confirmation))
    cat("\n\n")
  }
}

# Example for AR Patel
list_top_bowler_last_three_year <- list()
for (i in unique(total_wicket_each_year$year)[1:3]) {
  list_top_bowler_last_three_year[[as.character(i)]] <- total_wicket_each_year %>%
    filter(year == i & Bowler == "AR Patel") %>%
    pull(Bowler)
}
print(list_top_bowler_last_three_year)

for (year in names(list_top_bowler_last_three_year)) {
  for (bowler in list_top_bowler_last_three_year[[year]]) {
    if (bowler == "AR Patel") {
      cat("************************\n")
      cat("year:", year, " Bowler:", bowler, "\n")
      get_best_distribution(wickets %>% filter(Bowler == bowler) %>% pull(wicket_confirmation))
      cat("\n\n")
    }
  }
}

# Example for Batsman AR Patel
list_top_batsman_last_three_year <- list()
for (i in unique(total_run_each_year$year)[1:3]) {
  list_top_batsman_last_three_year[[as.character(i)]] <- total_run_each_year %>%
    filter(year == i & Striker == "AR Patel") %>%
    pull(Striker)
}
print(list_top_batsman_last_three_year)

for (year in names(list_top_batsman_last_three_year)) {
  for (striker in list_top_batsman_last_three_year[[year]]) {
    if (striker == "AR Patel") {
      cat("************************\n")
      cat("year:", year, " Batsman:", striker, "\n")
      get_best_distribution(runs %>% filter(Striker == striker) %>% pull(runs_scored))
      cat("\n\n")
    }
  }
}

# Calculate correlation between salary and runs scored
R2024 <- total_run_each_year %>%
  filter(year == 2024)

# Function to match names
match_names <- function(name, names_list) {
  match <- amatch(name, names_list, method = "jw", maxDist = 0.2)
  if (is.na(match)) return(NA) else return(names_list[match])
}

# Create a new column in df_salary with matched names from df_runs
ipl_salary$Matched_Player <- sapply(ipl_salary$Player, match_names, R2024$Striker)

# Merge the dataframes on the matched names
df_merged <- merge(ipl_salary, R2024, by.x = "Matched_Player", by.y = "Striker")

# Calculate the correlation
correlation <- cor(df_merged$Rs, df_merged$runs_scored, use = "complete.obs")
cat("Correlation between Salary and Runs:", correlation)
M <- df_merged %>% 
  dplyr::select(Rs, runs_scored) %>%
  as.data.frame()
corr_matrix <- cor(M, use = "complete.obs")
corrplot(corr_matrix, type = "upper", order = "hclust", col = brewer.pal(n = 8, name = "RdYlBu"))