
# Count fruit references with this function:
Fruit_extractor <- function(dff_adj, froots){
    library(xtable)

    df_sav <- dff_adj %>%
        group_by(country) %>%
        summarise(Ref_froot = sum( grepl(froots, description)) / n()) %>%
        # grepl(froots, description): Checks if the fruit term appears in each review (returns TRUE/FALSE).
        # sum(): Counts TRUE cases (total mentions of the fruit in the country).
        # / n(): Divides by the total number of reviews for that country, giving a proportion
        mutate(Fruit = froots) # Adds a column (RHS) labeling which fruit was searched

df_sav

# Calculates how often a given fruit term (froots) appears in wine descriptions,
# normalized by the total number of reviews per country
}
