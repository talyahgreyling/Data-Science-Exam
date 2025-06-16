First_fruits_plot <- function(fruit_count, xaxis_rows = 2, xaxis_size = 5){

    #Selecting Top 5 Fruits
fruitsplot <- fruit_count %>%
    group_by(Fruit) %>%
    summarise(largest = sum(Ref_froot)) %>% # calculates total mentions of each fruit across all countries
    arrange(desc(largest)) %>% # sort large to small
    head(5) %>% # select top 5 fruits
    pull(Fruit) # Extracts fruit names as character vector

g <- fruit_count %>%
    filter(Fruit %in% fruitsplot) %>%  # Keep only top 5 fruits
    plot_orderset(., Column = "Fruit", Order = fruitsplot) %>% # Order fruits by popularity
    plot_orderset(., Column = "country", Order = CountryList) %>% # Order countries consistently
    ggplot() +
    geom_bar(aes(Fruit, Ref_froot, fill = country), # X = Fruits, Y = %, Color = Country
             stat = "identity", # Use exact Ref_froot values
             position = "dodge") + # each fruit has side-by-side grouped bars per country
    theme_bw() +
    # y axis percentage:
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) + # Format y-axis as %
    scale_fill_brewer(palette="Dark2") +
    scale_x_discrete(guide = guide_axis(n.dodge = xaxis_rows)) + # Prevent label overlap
    labs(title = "Fruit References for Wine Reviews", subtitle = "Some subtitle", caption = "Data source: WineMag.com", x = "", y = "Fruit Reference Percentages") +
    theme(legend.position = "top", legend.title = element_blank()) +
    theme(plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 12),
          axis.text.x = element_text(size = xaxis_size))

g

}

# function creates a customized bar chart visualizing the top 5 most frequently mentioned fruit flavors in wine reviews across selected countries