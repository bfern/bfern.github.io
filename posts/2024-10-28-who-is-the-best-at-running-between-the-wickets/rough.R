library(readr)
library(dplyr)
library(ggplot2)
library(randomForest)

df <- read_csv("t20s_2013-23_mgd.csv")

# plot boundaries vs non boundaries as sense check on data
df %>%
	filter(wagonX != 0, wagonY != 0) %>%
	mutate(boundary = outcome %in% c("four", "six")) %>%
	ggplot(aes(x = wagonX, y = wagonY, col = boundary)) +
	geom_point()

# plot to check how wagonZone works
df %>%
	filter(wagonX != 0, wagonY != 0) %>%
	ggplot(aes(x = wagonX, y = wagonY, col = factor(wagonZone)) +
	geom_point()
    
 # plot to sfor non boundary runs, colour number of runs
 df %>%
 	filter(
 		wagonX != 0,
 		wagonY != 0,
 		outcome %in% c("no run", "run"),
 		competition == "T20 Blast",
 		over >= 7, # exclude powerplay because of fielding positions are different
 		score <= 3,
 		bowl_kind != "mixture/unknown"
 	) %>%
 	ggplot(aes(x = wagonX, y = wagonY, col = factor(score))) +
 	geom_point() +
 	facet_wrap(~bowl_kind + bat_hand)


 df_grpd_ground <- df %>%
 	filter(
 		competition == "T20 Blast",
 		wagonX != 0,
 		wagonY != 0
 	) %>%
 	group_by(ground) %>%
 	summarise(
 		number = sum(outcome %in% c("no run", "run")),
 		dot_prop = mean(score[outcome %in% c("no run", "run")] == 0),
 		one_prop = mean(score[outcome %in% c("no run", "run")] == 1),
 		two_prop = mean(score[outcome %in% c("no run", "run")] %in% c(2, 3)),
 		boundary_prop = mean(outcome %in% c("four", "six"))
 	)
 	
 	
 df_grpd_ground %>%
 	filter(number > 6000) %>%
 	ggplot(aes(x = two_prop, y = boundary_prop)) +
 	geom_point()
 	
 	
 
 ##############################
 ###### Modelling ######
 ##############################

## Data cleaning

first_class_grounds <- df %>%
	filter(competition == "T20 Blast") %>%
	group_by(ground) %>%
	summarise(number = n()) %>%
	arrange(desc(number)) %>%
	slice(1:18) %>%
	pull(ground)

df_for_model <- df %>%
	filter(
 		wagonX != 0,
 		wagonY != 0,
 		outcome %in% c("no run", "run"),
 		competition == "T20 Blast",
 		ground %in% first_class_grounds,
 		over >= 7, # exclude powerplay because of fielding positions are different
 		score <= 3 # excluding overthrows (not too sure how exactly they are done)
 	) %>%
 	mutate(
 		bowl_kind = if_else(bowl_kind == "mixture/unknown", "spin bowler", bowl_kind)
 	)
 
 ## Random forest model
 
 rf_model <- randomForest(
  score ~ wagonX + wagonY + ground + bowl_kind + bat_hand,
  data = df_for_model,
  ntree = 400,           # Increased number of trees to 400
  mtry = 3,              # Set mtry to 3, a smaller value to avoid overfitting
  nodesize = 15,         # Increased minimum node size to prevent overfitting
  importance = TRUE      # Set to TRUE to extract feature importance
)

importance(rf_model)
 
unique_points <- df_for_model %>%
  #filter(ground == "Edgbaston, Birmingham") %>%
  distinct(wagonX, wagonY, bowl_kind, bat_hand, ground) #%>%
  #mutate(ground = "Edgbaston, Birmingham")

# Make predictions for the unique points
unique_points$predictions <- predict(rf_model, newdata = unique_points)

# Create the plot with 4 panels using bowl_kind and bat_hand
# Create the plot with 4 panels using bowl_kind and bat_hand
# Create the plot with 4 panels using bowl_kind and bat_hand
ggplot(unique_points, aes(x = -wagonX, y = wagonY, fill = predictions)) +
  geom_tile(color = "white") +  # Add a white border to the tiles for better visibility
  facet_grid(bowl_kind ~ bat_hand) + 
  scale_fill_gradientn(colors = c("red", "orange", "blue", "purple"),  # Define the colors
                       values = scales::rescale(c(0, 1, 2, 3)),  # Rescale values
                       limits = c(0, 3), 
                       name = "Prediction") + 
  labs(#title = "Predictions by -wagonX and wagonY for Ground: Edgbaston, Birmingham",
       x = NULL, y = NULL) +  # Remove axis labels
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines")) +  # Increase spacing between facets
  geom_point(size = 4, shape = 21, color = "black", alpha = 0.7) +  # Add points with fixed size
  scale_size_continuous(range = c(4, 4)) +  # Set all points to the same size
  theme(axis.text.x = element_blank(),   # Remove x-axis text
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        axis.text.y = element_blank(),   # Remove y-axis text
        axis.ticks.y = element_blank())   # Remove y-axis ticks

 
t20_bbb$pred <- as.numeric(predict(rf_model))


df_for_model %>%
	group_by(bat) %>%
	summarise(
		number = n(),
		extra_run_ave = mean(score - pred)
	) %>%
	arrange(extra_run_ave) %>%
	filter(number >= 100) %>%
	head(20)

resuls_df <- bind_rows(
	t20_bbb %>%
		filter(!is.na(p_bat)) %>%
		select(p_bat, score_minus_wides_and_noballs, pred) %>%
		rename(player_id = p_bat),
	t20_bbb %>%
		filter(!is.na(p_nonstriker_bat)) %>%
		select(p_nonstriker_bat, score_minus_wides_and_noballs, pred) %>%
		rename(player_id = p_nonstriker_bat)
) %>%
	group_by(player_id) %>%
		summarise(
			number = n(),
			extra_run_ave = mean(score_minus_wides_and_noballs - pred)
		) %>%
		arrange(extra_run_ave) %>%
		filter(number >= 100) %>%
		left_join(players_df)
 
