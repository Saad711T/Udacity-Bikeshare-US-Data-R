#Import libraries :
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
"""Libraries Using : dplyr, readr, lubridate and ggplot2.
    dplyr : for data manipulation and transformation.
    readr : to reading the CSV files .
    lubridate : for date-time manipulation .
    ggplot2 : for data visualization.
 Project target :  explore data for bikeshare in Three cities in US (Chicago, Washington, and NYC).
"""



#Loading data from datasets 'CSVs'
CITY_DATA <- list(
  "chicago" = "chicago.csv",
  "new york" = "new_york_city.csv",
  "washington" = "washington.csv"
)

# Valid values of cities-months and days in lists.
valid_cities <- c("chicago", "new york", "washington")
valid_months <- c("january", "february", "march", "april", "may", "june", "all")
valid_days <- c("sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "all")

get_filters <- function() {
    """
    Ask user to select filters for city, month, and day.

    Returns:
        city (str): name of the city to analyze
        month (str): name of the month to filter by or 'all'
        day (str): name of the day of week to filter by or 'all'
    """
    cat("Welcome! Let's explore US bikeshare data.\n")

    # Get the input from user for a city.
    repeat {
        city <- tolower(trimws(readline("Choose a city (Chicago, New York, Washington): ")))
        if (city %in% valid_cities) break
        cat("Invalid city. Try again.\n")
    }

    # Ask for Filter type.
    repeat {
        filter_choice <- tolower(trimws(readline("Would you like to filter by 'month', 'day', or 'none'? ")))

        if (filter_choice == "month") {
            month <- tolower(trimws(readline("Enter a month (January - June) or 'all': ")))
            if (month %in% valid_months) {
                day <- "all"
                break
            }
            cat("Invalid month. Try again.\n")
        } else if (filter_choice == "day") {
            day <- tolower(trimws(readline("Enter a day of the week or 'all': ")))
            if (day %in% valid_days) {
                month <- "all"
                break
            }
            cat("Invalid day. Try again.\n")
        } else if (filter_choice == "none") {
            month <- "all"
            day <- "all"
            break
        } else {
            cat("Invalid input. Please type 'month', 'day', or 'none'.\n")
        }
    }

    cat(paste(rep("-", 40), collapse = ""), "\n")
    return(list(city = city, month = month, day = day))
}

load_data <- function(city, month, day) {
    """
    Load data for the specified city and apply filters.

    Args:
        city (str): city name
        month (str): selected month or 'all'
        day (str): selected day of the week or 'all'
    """
    df <- read_csv(CITY_DATA[[city]])
    df$start_time <- ymd_hms(df$`Start Time`)
    df$month <- month(df$start_time)
    df$day_name <- tolower(weekdays(df$start_time))

    if (month != "all") {
        month_index <- which(valid_months == month)
        df <- df %>% filter(month == month_index)
    }

    if (day != "all") {
        df <- df %>% filter(day_name == day)
    }

    return(df)
}


time_stats <- function(df) {
    """
    Display statistics on the most frequent times of travel.
    """
    cat("\nCalculating Most Frequent Travel Times...\n")
    start <- Sys.time()

    most_common_month <- names(sort(table(df$month), decreasing = TRUE))[1]
    cat("Most Common Month:", tools::toTitleCase(valid_months[as.numeric(most_common_month)]), "\n")

    most_common_day <- names(sort(table(df$day_name), decreasing = TRUE))[1]
    cat("Most Common Day of Week:", tools::toTitleCase(most_common_day), "\n")

    df$hour <- hour(df$start_time)
    common_hour <- as.numeric(names(sort(table(df$hour), decreasing = TRUE))[1])
    suffix <- ifelse(common_hour < 12, "AM", "PM")
    display_hour <- ifelse(common_hour == 0, 12, ifelse(common_hour <= 12, common_hour - 12))
    cat(sprintf("Most Common Start Hour: %d %s\n", display_hour, suffix))

    cat(sprintf("\nThis took %.2f seconds.\n", as.numeric(difftime(Sys.time(), start, units = "secs"))))
    cat(paste(rep("-", 40), collapse = ""), "\n")


# Plotting the most common start hour.
print(
  ggplot(df, aes(x = factor(month, levels = 1:6), fill = factor(month))) +
    geom_bar() +
    scale_x_discrete(labels = tools::toTitleCase(valid_months[1:6])) +
    labs(title = "Trip Counts by Month", x = "Month", y = "Count") +
    theme_minimal() +
    theme(legend.position = "none")
)


}

station_stats <- function(df) {
    """
    Display statistics on the most popular stations and trips.
    """
    cat("\nCalculating Most Popular Stations and Trips...\n")
    start <- Sys.time()

    start_station <- names(sort(table(df$`Start Station`), decreasing = TRUE))[1]
    cat("Most Frequent Start Station:", start_station, "\n")

    end_station <- names(sort(table(df$`End Station`), decreasing = TRUE))[1]
    cat("Most Frequent End Station:", end_station, "\n")

    df$trip <- paste(df$`Start Station`, "to", df$`End Station`)
    common_trip <- names(sort(table(df$trip), decreasing = TRUE))[1]
    cat("Most Frequent Trip:", common_trip, "\n")


# Plotting the top 5 start stations
cat(sprintf("\nThis took %.2f seconds.\n", as.numeric(difftime(Sys.time(), start, units = "secs"))))
cat(paste(rep("-", 40), collapse = ""), "\n")

top_starts <- df %>%
  count(`Start Station`, sort = TRUE) %>%
  head(5)

print(
  ggplot(top_starts, aes(x = reorder(`Start Station`, n), y = n, fill = `Start Station`)) +
    geom_col() +
    coord_flip() +
    labs(title = "Top 5 Start Stations", x = "Start Station", y = "Count") +
    theme_minimal() +
    theme(legend.position = "none")
)


}




trip_duration_stats <- function(df) {
    """
    Display total and average trip durations.
    """
    cat("\nCalculating Trip Durations...\n")
    start <- Sys.time()

    total_duration <- sum(df$`Trip Duration`, na.rm = TRUE)
    minutes <- total_duration %/% 60
    seconds <- total_duration %% 60
    hours <- minutes %/% 60
    minutes <- minutes %% 60
    cat(sprintf("Total Travel Time: %dh %dm %ds\n", hours, minutes, seconds))

    average_duration <- mean(df$`Trip Duration`, na.rm = TRUE)
    avg_min <- average_duration %/% 60
    avg_sec <- round(average_duration %% 60)
    if (avg_min >= 60) {
        avg_hr <- avg_min %/% 60
        avg_min <- avg_min %% 60
        cat(sprintf("Average Travel Time: %dh %dm %ds\n", avg_hr, avg_min, avg_sec))
    } else {
        cat(sprintf("Average Travel Time: %dm %ds\n", avg_min, avg_sec))
    }

    cat(sprintf("\nThis took %.2f seconds.\n", as.numeric(difftime(Sys.time(), start, units = "secs"))))
    cat(paste(rep("-", 40), collapse = ""), "\n")
}

stats_user <- function(df, city) {
    """
    Display statistics on bikeshare users.
    """
    cat("\nCalculating User Stats...\n")
    start <- Sys.time()

    if ("User Type" %in% names(df)) {
        cat("User Types:\n")
        print(table(df$`User Type`))
    }

    #Statistics of gender.
    if ("Gender" %in% names(df)) {
        cat("\nGender Distribution:\n")
        print(table(df$Gender))
    } else {
        cat(sprintf("\nNo gender data available for %s\n", tools::toTitleCase(city)))
    }

    #Statistics of birth year.
    if ("Birth Year" %in% names(df)) {
        earliest <- min(df$`Birth Year`, na.rm = TRUE)
        latest <- max(df$`Birth Year`, na.rm = TRUE)
        common <- as.numeric(names(sort(table(df$`Birth Year`), decreasing = TRUE))[1])
        cat(sprintf("\nEarliest Birth Year: %d\n", earliest))
        cat(sprintf("Most Recent Birth Year: %d\n", latest))
        cat(sprintf("Most Common Birth Year: %d\n", common))
    } else {
        cat(sprintf("\nNo birth year data available for %s\n", tools::toTitleCase(city)))
    }

    cat(sprintf("\nThis took %.2f seconds.\n", as.numeric(difftime(Sys.time(), start, units = "secs"))))
    cat(paste(rep("-", 40), collapse = ""), "\n")


    
if ("Gender" %in% names(df)) {
    gender_df <- df %>%
        filter(!is.na(Gender)) %>%
        count(Gender)
    # Plotting genders distribution
    print(
      ggplot(gender_df, aes(x = Gender, y = n, fill = Gender)) +
        geom_col() +
        labs(title = "Gender Distribution", x = "Gender", y = "Count") +
        theme_minimal()
    )
}



}

individual_data <- function(df) {
    """
    Display individual trip data if requested by The User.
    """
    index <- 1
    chunk_size <- 5
    total_rows <- nrow(df)

    while (index <= total_rows) {
        show_data <- tolower(trimws(readline("Would you like to see individual trip data? yes or no : ")))
        if (show_data == "yes") {
            print(df[index:min(index + chunk_size - 1, total_rows), ])
            index <- index + chunk_size
        } else {
            break
        }



    }
}




main <- function() {
    """
    Main function to run the program .
    """
    repeat {
        # Get user filters
        filters <- get_filters()
        city <- filters$city
        month <- filters$month
        day <- filters$day

        cat(sprintf("\nYou Selected: %s | Month: %s | Day: %s\n", tools::toTitleCase(city), tools::toTitleCase(month), tools::toTitleCase(day)))



        # Load filtered data
        df <- load_data(city, month, day)


        # Display various statistics
        time_stats(df)
        station_stats(df)
        trip_duration_stats(df)
        stats_user(df, city)
        individual_data(df)



        # Ask if user wants to restart
        restart <- tolower(trimws(readline("\nWould you like to restart the program? yes or no : ")))
        if (restart != "yes") {
            cat("Goodbye!\n")
            break
        }
    }
}

if (interactive()) {
    main()
}

