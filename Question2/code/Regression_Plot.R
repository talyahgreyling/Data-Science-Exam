
Regression_Plot <- function(df_SA){

dfreg <- df_SA %>%
    select(taster_name, points, price, province, variety)

Reglist <-
    dfreg %>%
    group_by(taster_name) %>%
    do(reg = lm(as.formula("points ~ price + province + variety"), data = .)) %>% # fits regression for each taster
    # Make each taster a list so we can use map:
    group_split(taster_name) # Stores models in a list (Reglist), split by taster.


g <- Reglist %>%
    map(~Reg_Graph_Creator(.)) %>% # Applies a custom plotting function
    cowplot::plot_grid(plotlist = ., ncol = 1) # Combines plots vertically

g

}



