Reg_Graph_Creator <- function(Reglist){

    # TIP: For building function, uncomment the following:
    # Reguse <- Reglist[[1]]
    Reguse <- Reglist # Get the taster-specific regression results
    # Let's be fancy and grab the R squared as well...
    Rsqd <- Reguse %>%
        pull(reg) %>% # Extract the lm object
        .[[1]] %>% # Access the first (only) model
        broom::glance() %>% # Get model summary stats
        pull(r.squared) %>% # Extract R-squared
        round(.,3)*100 # Convert to percentage (e.g., 0.85 → 85%)

    Reguse %>%
        pull(reg) %>%
        .[[1]] %>%
        broom::augment() %>% # Get fitted values and residuals
        ggplot()  +
        geom_point(aes(.fitted, price)) + # Points: Actual vs. Fitted ratings
        # Trend line: Shows price bias in predictions
        geom_smooth(aes(.fitted, price), method = "lm", se = FALSE, color = "lightgrey") +
        # Labels with dynamic taster name and R-squared
        labs(x = "Actual", y = "Fitted",
             title =  glue::glue("Regression Type: {Reguse$taster_name}"),
             subtitle = glue::glue("R squared: {Rsqd}%")) +
        theme_bw()
}


# function creates a diagnostic plot for a wine rating regression model,
# showing the relationship between actual and predicted ratings while highlighting the model's performance.
# Output: A ggplot showing:
#   Actual vs. Fitted values from the regression
#   Price trend line (to visualize bias patterns)
#   R-squared (model fit quality) in the subtitle