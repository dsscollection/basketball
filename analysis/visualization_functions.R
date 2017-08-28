library(ggplot2)

plot_distribution_of_movement_distances = function(player_df) {
    n = nrow(player_df)
    distances = numeric(n-1)
    for (i in 1:(n-1)) {
        curr_coords = player_df[i,c('x','y')]
        next_coords = player_df[(i+1),c('x','y')]
        distances[i] = eucl_dist(curr_coords, next_coords)
    }
    df = as.data.frame(distances)
    plot_dists = ggplot(df, aes(distances)) +
        geom_histogram(binwidth=0.05) +
        xlim(c(0,2)) +
        labs(title="Histogram for distances between moments")
    plot_dists
}

visualize_etas = function(r, eta_data, in_title="") {
    # Hack to make the y-axis line up with Ewen Gallic's court
    eta_data[,'y1'] = eta_data[,'y1'] - YMAX
    eta_data[,'y2'] = eta_data[,'y2'] - YMAX
    euc_dist = apply(
        eta_data,
        1,
        function(x) sqrt((x[3]-x[1])^2 + (x[4]-x[2])^2)
    )
    eta_data$dist = euc_dist
    colorpalette = colorRampPalette(c('green', 'purple'))
    eta_data$col = colorpalette(10)[
        as.numeric(cut(eta_data$dist, breaks=10))
    ]
    base = get_half_court_left_to_right()
    # Proportionally sized arrows:
    # http://stackoverflow.com/questions/20656126/proportionally-
    # sized-arrows-in-ggplot
    # Colored arrows:
    # http://stackoverflow.com/questions/9946630/colour-points-in-a-
    # plot-differently-depending-on-a-vector-of-values
    arrows = geom_segment(
        aes(x=x1, y=y1, xend=x2, yend=y2),
        arrow=arrow(angle=15, length=unit(eta_data$dist*0.75,"cm")),
        data=eta_data,
        size=1,
        alpha=0.8,
        col=eta_data$col
    )
    p = base + arrows +
        theme(legend.position="none", plot.margin=unit(c(0,0,0,0), "cm")) +
        scale_size(range=c(1,2)) + ggtitle(in_title)
    return (p)
}
