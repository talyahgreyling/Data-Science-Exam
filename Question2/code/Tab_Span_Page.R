# Copied this from Texevier template...
Tab_Span_Page <- function(){

    addtorow          <- list()
    addtorow$pos      <- list()
    addtorow$pos[[1]] <- c(0)
    addtorow$command  <- c(paste("\\hline \n", # Horizontal line
                                 "\\endhead \n", # Header to repeat on subsequent pages
                                 "\\hline \n", # Line under header
                                 "{\\footnotesize Continued on next page} \n", # Footer text
                                 "\\endfoot \n", # Footer for intermediate pages
                                 "\\endlastfoot \n",sep="")) # Footer for final page
    addtorow
}

# function prepares formatting rules for LaTeX tables that span multiple pages when rendered in R Markdown/Quarto PDF outputs.
# Call this function when creating long tables with xtable or kable in R Markdown/Quarto PDFs


