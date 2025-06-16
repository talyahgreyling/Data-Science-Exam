Barplot_breakdown_Happy_Contributors <- function(datcolat, Title, Subtitle,
                                                 xaxis_size = 10,
                                                 xaxis_rows = 2){

    # This removes the ugly summarise warning...
    options(dplyr.summarise.inform=F)

dfp <-   # transform datcolat into long format
    datcolat %>%
    group_by(`Regional indicator`) %>%
    summarise_at( vars( c(`Ladder score`, starts_with("Explained"), `Dystopia + residual`)), ~mean(.)) %>%
    # Above: creates a dataframe with one row per region and columns for each averaged metric
    gather(Score, Value, -`Regional indicator`, -`Ladder score`) %>%
    # Converts wide data to long format using tidyr::gather() (modern alternative: pivot_longer()):
    ## Score: New column storing original column names (e.g., "Explained by: GDP").
    ## Value: New column storing the corresponding mean values.
    ## Exclusions: Keeps Regional indicator and Ladder score as ID variables.
    mutate(Score = gsub("Explained by: ", "", Score)) # Removes the prefix "Explained by: " from score names

# Make SA data bindable:
SA <-
    datcolat %>% filter(`Country name` == "South Africa") %>% # extract rows of SA data
    select(c(`Regional indicator`, `Ladder score`, starts_with("Explained"), `Dystopia + residual`)) %>% #select relevant cols
    mutate(`Regional indicator` = "South Africa") %>% #change all to SA ipv bv Sub Saharan SA
    gather(Score, Value, -`Regional indicator`, -`Ladder score`) %>% #Converts wide data to long format
    mutate(Score = gsub("Explained by: ", "", Score))

# See this makes your life much easier:
dfp <- bind_rows(dfp,SA)

# Arrange the Score column for consistency as from highest avg to lowest:

order1 <- c("South Africa", # Force South Africa to appear first
            datcolat %>%
                group_by(`Regional indicator`) %>%
                summarise(Lad = mean(`Ladder score`)) %>%
                arrange(Lad) %>%  # # Sort regions by mean happiness (low to high)
                pull(`Regional indicator`)) ## Extract as vector
order2 <- dfp %>%
    group_by(Score) %>%
    summarise(Avg = mean(Value)) %>%
    arrange(Avg) %>%
    pull(Score)

g <-
dfp %>%
    plot_orderset(., Column = "Regional indicator", Order = order1) %>%
    plot_orderset(., Column = "Score", Order = order2) %>%
    ggplot() + geom_bar(aes(`Regional indicator`, y = Value, fill = Score), stat = "identity", position = "stack") +
    theme_bw() +
    scale_fill_brewer(palette="Dark2") +
    scale_x_discrete(guide = guide_axis(n.dodge = xaxis_rows)) +
    labs(title = Title, subtitle = Subtitle, caption = "Data source: World Happiness Index", x = "", y = "Breakdown of Happiness") +
    theme(legend.position = "top", legend.title = element_blank()) +
    theme(plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 12), axis.text.x = element_text(size = xaxis_size)) +
    guides(fill = F) # Hides the fill legend (if redundant)

g


}