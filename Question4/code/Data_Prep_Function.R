Data_Prep_Function <- function(Lst, Tournament_Inputs = c("G", "M", "F")){

    read_file_func <- function(Lst){ # safely read and preprocess RDS (R Data Storage) files while handling potential warnings/errors and ensuring proper data typing
        # listchoose <- Lst[[10]]
        listchoose <- Lst # Takes a file path or list element containing RDS path

        shhrds <- function(x, ...) {
            sjrd <- purrr::quietly(read_rds) # Quiet version of read_rds
            df <- sjrd(x, ... ) # Reads file, captures output/warnings
            df$result # Returns only the data
        }
        result <- suppressWarnings(shhrds(listchoose)) # Reads file with no warnings
        result <-
            result %>%
            mutate(across(.cols = -c(contains("tourney"), contains("name"), contains("ioc"), contains("round"), contains("score"), contains("surface")),
                          .fns = ~as.numeric(.))) %>%  # Convert MOST columns to numeric
            mutate(across(.cols = c(contains("tourney"), contains("name"), contains("ioc"), contains("round"), contains("score"), contains("surface")),
                          .fns = ~as.character(.))) # Force specific columns to stay as character
    } # ioc = country code

    result <- Lst %>% # imput RDS file paths as list
        map_df(~read_file_func(.)) #Applies read_file_func() to each file & Combines all files into one dataframe (map_df)

    # Tons of data, might require some filtering.



    # result$tourney_date %>% unique

    result <-
        result %>%
        mutate(date = ymd(tourney_date)) %>% # Convert to Date type
        select(date, everything()) # Move date column first

    # Let's only consider big events - the small ones don't matter...
    # see
    # tourney_level
    # - For men: 'G' = Grand Slams, 'M' = Masters 1000s, 'A' = other tour-level events, 'C' = Challengers, 'S' = Satellites/ITFs, 'F' = Tour finals and other season-ending events, and 'D' = Davis Cup
    # - For women, there are several additional tourney_level codes, including 'P' = Premier, 'PM' = Premier Mandatory, and 'I' = International. The various levels of ITFs are given by the prize money (in thousands), such as '15' = ITF $15,000. Other codes, such as 'T1' for Tier I (and so on) are used for older WTA tournament designations. 'D' is used for Federation/Fed/Billie Jean King Cup, and also for Wightman Cup and Bonne Bell Cup.

    # result %>% select(tourney_id, tourney_level, tourney_name) %>% unique

    result_Large_events <-
        result %>%
        mutate(date = ymd(tourney_date)) %>%
        select(date, everything()) %>%
        filter(tourney_level %in% Tournament_Inputs) # select tournament levels specified in Readme

    result_Large_events

}