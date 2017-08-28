library(raster)
library(gridExtra)
library(dplyr)

setwd('~/Code/basketball')
source('constants.R')
source('utils.R')
source('movement_functions.R')
source('plot_court_functions.R')
source('visualization_functions.R')

# High level steps to turn the moments into acceleration plots
# - Add data that signifies who has possession
# - Grab a player's offensive moments
# - Flip the coordinates so that it's all on one half of the court
# - Get empirical etas for on and off ball
# - Get the bayesian regression outputs
# - Plot the results

# Away = Miami, Home = Brooklyn
moments = read.csv('data/2013_11_01_MIA_BKN.csv')

p_id = 214152
team = 'a'
moments_df = add_possession_data(moments)
dir_of_play = get_directions_of_play(moments_df)
off_moments = get_offensive_moments(team, dir_of_play, moments_df)
player_df = get_player_offensive_moments(team, p_id, off_moments)
flipped_df = flip_coords(player_df, team, dir_of_play)
df_on_ball = filter(flipped_df, ent_w_poss == p_id)
df_off_ball = filter(flipped_df, ent_w_poss != p_id)

r = raster(xmn=XMIN, ymn=YMIN, xmx=XMID, ymx=YMAX, res=RASTER_RES)
r[] = 0
sigma = 2

emp_etas_on_ball = get_empirical_etas(r, df_on_ball)
emp_etas_off_ball = get_empirical_etas(r, df_off_ball)

regression_data_on = get_regression_inputs(emp_etas_on_ball)
regression_data_off = get_regression_inputs(emp_etas_off_ball)

regression_output_on_bayes = bayes_regression(regression_data_on[['X']], regression_data_on[['Y']], sigma=sigma)
regression_output_off_bayes = bayes_regression(regression_data_off[['X']], regression_data_off[['Y']], sigma=sigma)

eta_data_on_ball_bayes = format_data_to_plot(r, regression_output_on_bayes[['ls_x']], regression_output_on_bayes[['ls_y']])
eta_data_off_ball_bayes = format_data_to_plot(r, regression_output_off_bayes[['ls_x']], regression_output_off_bayes[['ls_y']])

visualize_etas(r, eta_data_on_ball_bayes, "2013/11/01: LeBron James on-ball")
visualize_etas(r, eta_data_off_ball_bayes, "2013/11/01: LeBron James off-ball")
