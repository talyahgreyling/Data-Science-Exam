Term_refs_table <- function(dff_adj,
                            Latex = TRUE){ # Boolean (TRUE for PDF output, FALSE for HTML/Word)
library(xtable)
    # NB to remove xtable's comments as (https://stackoverflow.com/questions/24400308/how-to-remove-the-lines-in-xtable-table-output-by-knitr):
    options(xtable.comment = FALSE)

    Result <-
    bind_rows(
        Term_refs_perc(dff_adj, Terms = c("tannins")),
        Term_refs_perc(dff_adj, Terms = c("smoke", "smokey", "ash"))
    ) %>% mutate(Percent = paste0(round(Percent*100, 3), "%")) # Formats percentages: Converts decimals to strings (e.g., 0.15 → "15%").

    # # If using Texevier, use this:
    # addtorow <- Tab_Span_Page() # add "Continued on next page" footer

    if(Latex){ # LaTeX (PDF) option

        Tab <- xtable(Result, caption = "Long Table Example  \\label{tab1}")

    #     # If using Texevier (check):
    # Tab <-
    # print.xtable(table,
    #              tabular.environment = "longtable", # longtable environment for multi-page tables
    #              floating = FALSE, # Leave this as is.
    #              table.placement = 'H', # Leave this as is.
    #              booktabs = T, # Aesthetics
    #              include.rownames = FALSE,  # Typically you don't want this in a table.
    #              add.to.row = addtorow, # For adding the Continued on next page part...
    #              comment = FALSE,
    #              caption.placement = 'top',  # Where do you want the caption?
    #              size="\\fontsize{12pt}{13pt}\\selectfont"  # Size of text in table..
    #)

    } else { # plain text (HTML/Word)

Tab <-
    knitr::kable(Result)
    }

    Tab
}

# function generates a formatted table comparing the frequency of specific wine-tasting terms
# (like "tannins" or "smoke") across countries, with options for both LaTeX (PDF) and plain text (HTML/Word) outputs.



