Top_5_Wineries_SA_Plot <- function(df_SA, Tasters){


gg <-
    df_SA %>%
    group_by(taster_name, winery) %>%
    summarise(Med_pnts = median(points)) %>% # Median score per winery-taster
    group_by(taster_name) %>%
    top_n(3, Med_pnts) %>% # Top 3 wineries per taster
    arrange(desc(Med_pnts)) %>% # Sort by score (high to low)
    ggplot() +
    # Points for median scores
    geom_point(aes(winery, Med_pnts, color = taster_name), size = 5, alpha = 0.7) +
    # Vertical lines from baseline (80) to scores
    geom_segment(aes(x = winery, xend = winery, y = 80, yend = Med_pnts), size = 1.5, alpha = 0.7, color = "darkgreen") +
    facet_wrap(~taster_name, scales = "free_x") + # One panel per taster
    ylim( c(80,100)) +  # Adjust y-axis limits
    scale_y_continuous( limits = c(80, 98)) +
    theme_bw() +
    guides(color = F) +  # Hide redundant legend
    scale_fill_brewer(palette="Dark2") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    labs(title = "Top Median Reviews per Winery: South Africa", subtitle = "Top wineries for Lauren and Susan", caption = "Data source: WineMag.com", x = "", y = "Median Winery review for wines over $20") +
    theme(legend.position = "top", legend.title = element_blank()) +
    theme(plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 12))

gg

}