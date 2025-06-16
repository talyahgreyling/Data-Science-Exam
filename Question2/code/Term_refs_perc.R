
Term_refs_perc <- function(dff_adj, Terms = c("tannins", "Tannins", "Smoke", "Smokey", "Smoke", "smoke", "smokey", "smoke", "ash", "Ash")){

    Term_ref_freq <-
        dff_adj %>%
        group_by(country) %>%
        summarise(Percent = sum( grepl( paste(Terms, collapse = "|"), description )) / n()) %>%
        # Combines all terms into a single regex pattern (e.g., "tannins|Tannins|Smoke|smokey|ash").
        # Checks if any term appears in each review (TRUE/FALSE).
        # sum(): Counts TRUE matches (reviews with at least one term)
        # / n(): Converts to a proportion
        mutate(Term = glue::glue_collapse(Terms, sep = ", ", last = " and ") ) # Creates a readable label of all searched terms
    #                                                                       (e.g., "tannins, Tannins, Smoke, Smokey, and ash").

    Term_ref_freq

}

# function calculates how frequently any of a list of wine-tasting terms appear in reviews, grouped by country,
# and returns the results as percentages.