# ---
# Use this file to load objects that are created from the functions
# in utils.R and movement_functions.R if you make changes and want to
# check your code
# ---

source("constants.R")
source("utils.R")
source("movement_functions.R")

# ---
# Test flip coordinates in helpers.R
# The court is 94x50; many times we want to flip the
# coordinates seen on one side of the court to the other
test_flip_coords = function() {
    team_id = 20
    # Build coordinates on the right side of the court
    coords = data.frame(
        rbind(
            c(60, 40, 3), c(50, 25, 3),
            c(60, 10, 3), c(90, 48, 3), c(90, 2, 3)
        )
    )
    colnames(coords) = c('x', 'y', 'quarter')
    # Build directions_of_play information s.t these coords will be flipped
    directions_of_play = data.frame(
        rbind(c("left", "right"), c("right", "left"))
    )
    rownames(directions_of_play) = c(20, 25)
    colnames(directions_of_play) = c("1st", "2nd")
    # Get the flipped coordinates
    flipped_coords = flip_coords(coords, team_id, directions_of_play)
    # Assert they're correct
    expected_flipped_coords = data.frame(
        rbind(
            c(34, 10, 3), c(44, 25, 3), c(34, 40, 3),
            c(4, 2, 3), c(4, 48, 3)
        )
    )
    colnames(expected_flipped_coords) = c('x', 'y', 'quarter')
    stopifnot(identical(expected_flipped_coords, flipped_coords))
    print("flip_coords() passed")
}

# ---
test_get_directions_of_play = function() {
    load("data/test/test_get_directions_of_play.Rdata")

    start.time <- Sys.time()
    dir_of_play_dplyr = get_directions_of_play(moments_df)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)

    result = all.equal(dir_of_play_dplyr, actual_dir_of_play)
    stopifnot(result == TRUE)
    print("get_directions_of_play implementation is correct!")
}

# ---
test_get_moments_w_possession_cols = function() {
    load("data/test/test_add_possession_columns.Rdata")

    start.time <- Sys.time()
    moments_df_dplyr = get_moments_w_possession_cols(moments)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)

    result = all.equal(moments_df_dplyr, actual_moments_with_poss_column)
    stopifnot(result == TRUE)
    print("get_moments_w_possession_cols implementation is correct!")
}

# ---
test_get_offensive_moments = function() {
    load("data/test/test_get_offensive_moments.Rdata")

    team = 'a'
    start.time <- Sys.time()
    offensive_moments_dplyr = get_offensive_moments(team, dir_of_play, moments_df)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)

    result = all.equal(offensive_moments_dplyr, actual_offensive_moments)
    stopifnot(result == TRUE)
    print("get_offensive_moments implementation is correct!")
}

# ---
test_get_player_offensive_moments = function() {
    load("data/test/test_get_player_offensive_moments.Rdata")

    team = 'a'
    p_id = LBJ_P_ID
    start.time <- Sys.time()
    player_df = get_player_offensive_moments(team, p_id, off_moments)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)

    # Don't let rownames affect the comparison
    rownames(actual_player_df) = seq(1, nrow(actual_player_df))
    rownames(player_df) = seq(1, nrow(player_df))
    result = all.equal(player_df, actual_player_df)
    stopifnot(result == TRUE)
    print("get_player_offensive_moments implementation is correct!")
}

# ---
test_get_empirical_etas = function() {
    load("data/test/test_get_empirical_etas.Rdata")

    r = raster(xmn=XMIN, ymn=YMIN, xmx=XMID, ymx=YMAX, res=RASTER_RES)
    r[] = 0
    start.time <- Sys.time()
    emp_etas_on_ball = get_empirical_etas(r, df_on_ball)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)

    result = all.equal(emp_etas_on_ball, actual_empirical_etas_on_ball)
    stopifnot(result == TRUE)
    print("get_player_offensive_moments implementation is correct!")
}
