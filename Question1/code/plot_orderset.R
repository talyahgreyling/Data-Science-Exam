
# Great function for ordering columns when using ggplot. I use this exact function all the time.

plot_orderset <- function(df, Column, Order){

    df[,Column][[1]] <- factor(df[,Column][[1]], levels = Order)

    df

}

# df[, Column] extracts the column as a dataframe (single column).

# [[1]] converts it to a vector.

# levels = Order forces the factor to use the user-specified order.