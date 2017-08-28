library(dplyr)

add_possession_data_for_quarter = function(moments_df) {
    # We rely on the event_id=23 to denote possession
    # event_id=6, defensive rebound, does not also invoke the possession id
    # so we check for this also
    #
    # This function adds two columns:
    # - 'team_w_poss': can be either of the 2 team ids or -1
    # - 'ent_w_poss': can be any of the entities in the game or -1
    nr = nrow(moments_df)
    team_possession_col = rep(-1, nr)
    ent_possession_col = rep(-1, nr)
    curr_team_w_poss = -1
    curr_ent_w_poss = -1
    for (i in 1:nr) {
        row = moments_df[i,]
        away_events = as.numeric(row[,AWAY_EVTS_COLS])
        home_events = as.numeric(row[,HOME_EVTS_COLS])
        events = c(away_events, home_events)
        if (POSSESSION_ID %in% events) {
            evt_id = POSSESSION_ID
        } else if (DEF_REB_ID %in% events) {
            evt_id = DEF_REB_ID
        } else if (OFF_REB_ID %in% events) {
            evt_id = OFF_REB_ID
        } else {
            evt_id = NA
        }
        if (!is.na(evt_id)) {
            # Figure out if away or home had the possession event
            is_away = evt_id %in% away_events
            is_home = evt_id %in% home_events
            stopifnot(xor(is_away, is_home))
            if (is_away) {
                team_w_poss = 'a'
                evts = away_events
            } else {
                team_w_poss = 'h'
                evts = home_events
            }
            # Then figure out which of the players it was
            p_index = match(evt_id, evts)
            col_name = sprintf("%s%i_ent", team_w_poss, p_index)
            ent_w_poss = row[,col_name]
            if (curr_team_w_poss != team_w_poss) {
                curr_team_w_poss = team_w_poss
            }
            if (curr_ent_w_poss != ent_w_poss) {
                curr_ent_w_poss = ent_w_poss
            }
        }
        team_possession_col[i] = curr_team_w_poss
        ent_possession_col[i] = curr_ent_w_poss
    }
    moments_df$team_w_poss = team_possession_col
    moments_df$entity_w_poss = as.integer(ent_possession_col)
    return (moments_df)
}

add_possession_data = function(moments_df) {
    quarters = unique(moments_df$quarter)
    nr = nrow(moments_df)
    nc = ncol(moments_df) + 2
    new_df = as.data.frame(matrix(NA, nrow=nr, ncol=nc))
    for (q in quarters) {
        quarter_query = which(moments_df$quarter == q)
        m = moments_df[quarter_query,]
        new_m = add_possession_data_for_quarter(m)
        new_df[quarter_query,] = new_m
    }
    colnames(new_df) = c(colnames(moments_df), 'team_w_poss', 'ent_w_poss')
    # The first 3 columns should not have NAs so they indicate incomplete rows
    final_df = new_df[complete.cases(new_df[,1:3]),]
    return (final_df)
}

row_has = function(row, options) {
    return (any(row %in% options))
}

get_directions_of_play = function(moments_df) {
    # Using the 1st q frames, figure out the direction the ball is
    # moving i.e: who starts off shooting to the right
    # Whoever starts shooting to the right continues to do so in the
    # 2nd q, then for the 3rd and 4th q, the directions flip
    teams = c('a', 'h')
    dir_of_play = as.data.frame(matrix(NA, nrow=2, ncol=2))
    rownames(dir_of_play) = teams
    colnames(dir_of_play) = c("1st", "2nd")
    # Start from when someone touches the ball
    first_moments = filter(
        moments_df,
        moments_df$game_clock < 720,
        moments_df$quarter == 1
    )
    # Get all of the offensive possessions of each team where a shot
    # occurred and look at the average x value
    away_events = first_moments[,AWAY_EVTS_COLS]
    home_events = first_moments[,HOME_EVTS_COLS]
    away_inds = apply(away_events, 1, row_has, options=SHOT_BEGIN_EVTS)
    home_inds = apply(home_events, 1, row_has, options=SHOT_BEGIN_EVTS)
    a_df = first_moments[away_inds, AWAY_X_COLS]
    h_df = first_moments[home_inds, HOME_X_COLS]
    avg_x1 = mean(apply(a_df, 1, mean))
    avg_x2 = mean(apply(h_df, 1, mean))
    # Determine which team starts out shooting to the right
    # Make sure not both teams have their average moments in
    # one side of the court
    team1_shoots_right = avg_x1 >= XMID
    team2_shoots_right = avg_x2 >= XMID
    stopifnot(xor(team1_shoots_right, team2_shoots_right))
    if (team1_shoots_right) {
        dir_of_play[toString(teams[1]),] = c('right', 'left')
        dir_of_play[toString(teams[2]),] = c('left', 'right')
    } else {
        dir_of_play[toString(teams[1]),] = c('left', 'right')
        dir_of_play[toString(teams[2]),] = c('right', 'left')
    }
    return (dir_of_play)
}

get_offensive_moments = function(team, dir_of_play, moments_df) {
    # Only give back the moments where the team has crossed the
    # halfcourt
    # Do each half separately since the sides change

    # First, trim all offensive plays in between a shot attempt and a
    # rebound
    # It's possible for a shot to go up with no "finishing" event,
    # e.g: heave at end of the quarter. We handle this case too
    if (team == 'a') {
        evts_cols = AWAY_EVTS_COLS
        x_cols = AWAY_X_COLS
    } else if (team == 'h') {
        evts_cols = HOME_EVTS_COLS
        x_cols = HOME_X_COLS
    } else {
        print("Error: team is not 'a' or 'h'")
    }
    m = moments_df[,evts_cols]
    shot_begin_inds = which(apply(m, 1, row_has, options=SHOT_BEGIN_EVTS))
    shot_end_inds = which(apply(m, 1, row_has, options=SHOT_END_EVTS))
    inds_to_keep = !logical(nrow(moments_df))
    for (begin_ind in shot_begin_inds) {
        # Get the frame that the shot finishes in
        end_ind_candidates = which(shot_end_inds >= begin_ind)
        if (length(end_ind_candidates) == 0) {
            next
        }
        end_ind = shot_end_inds[min(end_ind_candidates)]
        inds_to_keep[begin_ind:end_ind] = FALSE
    }
    moments_df = moments_df[inds_to_keep,]
    # Filter for all offensive moments
    team_moments_df = filter(moments_df, team_w_poss == team)
    # Filter for possessions where everybody has crossed the half court
    final_df = as.data.frame(matrix(NA, nrow=0, ncol=ncol(moments_df)))
    halves = c("1st", "2nd")
    for (h in halves) {
        if (h == "1st") {
            qs = c(1,2)
        } else if (h == "2nd") {
            qs = c(3,4)
        }
        direction = dir_of_play[team,h]
        half_moments = dplyr::filter(team_moments_df, quarter %in% qs)
        half_xs = half_moments[,x_cols]
        if (direction == "left") {
            keep = apply(half_xs, 1, function(r) all(r < XMID))
        } else if (direction == "right") {
            keep = apply(half_xs, 1, function(r) all(r > XMID))
        }
        final_df = rbind(final_df, half_moments[keep,])
    }
    return (final_df)
}

get_player_offensive_moments = function(team, p_id, offensive_moments) {
    if (team == 'a') {
        x_cols = AWAY_X_COLS
        y_cols = AWAY_Y_COLS
        ent_cols = AWAY_ENT_COLS
    } else if (team == 'h') {
        x_cols = HOME_X_COLS
        y_cols = HOME_Y_COLS
        ent_cols = HOME_ENT_COLS
    } else {
        print("Error: team is not 'a' or 'h'")
    }
    # Filter for only frames that contain that player on the court
    entities = offensive_moments[,ent_cols]
    keep = apply(entities, 1, function(r) any(p_id %in% r))
    off_moments = offensive_moments[keep,]
    entities = entities[keep,]
    # Since a player can appear in many columns, for simplicity's sake
    # we transform the data to put the player's (x,y) coordinates
    # in the 'x', 'y' column.
    p_inds = apply(entities, 1, function(r) match(p_id, r))
    off_xs = cbind(off_moments[,x_cols], p_inds)
    off_ys = cbind(off_moments[,y_cols], p_inds)
    off_moments$x = apply(off_xs, 1, function(r) r[r["p_inds"]])
    off_moments$y = apply(off_ys, 1, function(r) r[r["p_inds"]])
    df = dplyr::select(off_moments, c(time:y, team_w_poss:ent_w_poss))
    return (df)
}

flip_coords = function(player_df, team, directions_of_play) {
    # Input player_df is assumed to be cleaned, e.g: if attacking left
    # in the 1st half there should be no possessions where he is in
    # the right side
    # Take any points that are on the right side of the court and
    # transpose it to the left
    # If the team started the game attacking the left, then transpose
    # all 2nd half coordinates from right back to left.
    # If the team started the game attacking the right, then transpose
    # all 1st half coordinates to the left and leave the 2nd half.
    direction = directions_of_play[toString(team),"1st"]
    if (direction == "left") {
        qs = c(3,4)
    } else {
        qs = c(1,2)
    }
    inds_to_change = which(player_df$x >= XMID & player_df$quarter %in% qs)
    player_df[inds_to_change,'x'] = XMAX - player_df[inds_to_change,'x']
    player_df[inds_to_change,'y'] = YMAX - player_df[inds_to_change,'y']
    return (player_df)
}
