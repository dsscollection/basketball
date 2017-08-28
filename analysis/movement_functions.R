library(raster)

eucl_dist = function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

skip_this_iteration = function(r, k, player_moments) {
    # the 'k' passed in should be such that this won't generate an
    # index-based error
    skip = FALSE
    next_coords = player_moments[(k+1), c('x','y')]
    curr_coords = player_moments[k, c('x','y')]
    curr_cell = raster::cellFromXY(r, curr_coords)
    if (is.na(curr_cell)) {
        # Player stepped out of our bounds of interest
        was_out_of_bounds = TRUE
    } else {
        was_out_of_bounds = FALSE
    }
    dist = eucl_dist(curr_coords, next_coords)
    diff_too_large = dist >= MAX_JUMP
    if (was_out_of_bounds | diff_too_large) {
        skip = TRUE
    }
    return (skip)
}

get_empirical_etas = function(r, player_df) {
    nr = nrow(player_df)
    results = as.data.frame(matrix(NA, nrow=nr, ncol=3))
    colnames(results) = c('cell', 'x', 'y')
    for (i in 2:(nr-1)) {
        if (skip_this_iteration(r, i, player_df)) {
            next
        }
        prev_x = player_df[i-1,'x']
        curr_x = player_df[i,'x']
        next_x = player_df[i+1,'x']
        prev_y = player_df[i-1,'y']
        curr_y = player_df[i,'y']
        next_y = player_df[i+1,'y']

        eta_x = (next_x - curr_x) - (curr_x - prev_x)
        eta_y = (next_y - curr_y) - (curr_y - prev_y)
        curr_cell = raster::cellFromXY(r, cbind(curr_x, curr_y))
        results[i,] = c(curr_cell, eta_x, eta_y)
    }
    results = results[complete.cases(results),]
    return (results)
}

# Create function that takes in our empirical etas and transforms
# it to formats that we can use for Bayesian regression, mainly
# by one-hot encoding the cell
# Y = vector of acc_x or acc_y that we observed
# X = 1 hot encoded, n x V matrix of which bin the observed
# acceleration came from
get_regression_inputs = function(emp_etas) {
    nr = nrow(emp_etas)
    Y = emp_etas[,c('x','y')]
    X = as.data.frame(matrix(0, nrow=nr, ncol=NUM_CELLS))
    for (i in 1:nr) {
        X[i,emp_etas[i,'cell']] = 1
    }
    return (list(X=as.matrix(X), Y=as.matrix(Y)))
}

# - sigma increases this prior's weight; higher sigma means a smoother
# plot because of higher influence of using neighbouring values
# - phi is the range parameter; affects the range of the covariance
# in terms of spatial lag
get_Sigma = function(r, sigma, phi) {
    # Get the NUM_CELLS x 2 matrix that holds the coords of each cell
    coords = matrix(NA, nrow=NUM_CELLS, ncol=2)
    for (i in 1:NUM_CELLS) {
        coords[i,] = raster::xyFromCell(r, i)
    }
    # Get the distance matrix
    dist_coords = as.matrix(dist(coords))
    # Return corr_mat = sigma^2 * exp(-dist_mat/phi)
    # where sigma and phi are parameters we tune
    # corr_mat is the covariance, invert it to get the precision
    corr_mat = (sigma^2)*exp(-dist_coords/phi)
    return (solve(corr_mat))
}

bayes_regression = function(X_mat, Y_mat, sigma=1, phi=1) {
    r = raster::raster(xmn=XMIN, ymn=YMIN, xmx=XMID, ymx=YMAX, res=RASTER_RES)
    r[] = 0

    t_X_mat = t(X_mat)
    # Validate X_mat
    # Every row should only have one value
    # If Z has any 0's, that means those cells did not see any
    # empirical etas
    Z = t_X_mat %*% X_mat + get_Sigma(r, sigma, phi)

    # Calculate vars and save to var to avoid recalculating
    Z_inv = solve(Z)

    # Below should not give any errors
    # ls <- solve(t(X_mat) %*% X_mat) %*% t(X_mat)%*%Y_mat[,1]
    ls_x <- Z_inv %*% t_X_mat%*%Y_mat[,1]
    ls_y <- Z_inv %*% t_X_mat%*%Y_mat[,2]
    return (list(ls_x=ls_x, ls_y=ls_y))
}

format_data_to_plot = function(r, ls_x, ls_y) {
    eta_data = as.data.frame(matrix(NA, nrow=NUM_CELLS, ncol=4))
    colnames(eta_data) = c('x1', 'y1', 'x2', 'y2')
    for (i in 1:NUM_CELLS) {
        xy1 = raster::xyFromCell(r, i)
        xy2 = xy1 + c(ls_x[i], ls_y[i])
        eta_data[i,] = c(xy1[,'x'], xy1[,'y'], xy2[,'x'], xy2[,'y'])
    }
    return (eta_data)
}
