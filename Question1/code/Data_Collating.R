Data_Collating <- function(Datroot){

library(tidyverse)

    # Create a silent read function (prints unnecessary things with read_csv):
    silentread <- function(x){
        hushread <- purrr::quietly(read_csv)    #suppress messages & warnings
        df <- hushread(x)   # quietly read file
        df$result       #return data without messages
    }

    datcolat <-
        list.files(Datroot, full.names = T, recursive = T) %>%  # list all files recursively
        .[!grepl(".txt", .)] %>%    # exclude .txt files (e.g., READMEs)
        as.list() %>%
        map(~silentread(.)) %>%      # read each CSV silently
        bind_rows()    # combine into 1 dataframe

    datcolat

}