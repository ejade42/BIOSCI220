calculate_active_recovered_cases <- function(my_country_data) {
    # Check correct columns are present
    for (colname in c("location", "new_cases", "date")) {
        if (!(colname %in% colnames(my_country_data))) {
            abort(paste0("Input covid data must have a '", colname, "' column."), class = "missing_column")
        }
    }
    # Check only one location is present
    location <- unique(my_country_data$location)
    if (length(location) != 1) {
        abort("Input covid data must be filtered to exactly one location", class = "bad_input")
    }



    # Replace NA values with 0
    rows_with_NA <- is.na(my_country_data$new_cases)
    my_country_data$new_cases[rows_with_NA] <- 0


    ## CALCULATING VARIABLES FOR GRAPH
    # Keep a count of active cases, and recovered cases
    active_cases    <- rep(0.0, times = nrow(my_country_data))
    recovered_cases <- rep(0.0, times = nrow(my_country_data))

    # Do a for-loop which will calculate number of active and recovered cases
    for (i in 1:nrow(my_country_data))
    {
        # Keep track of the starting row number
        if (i <= 14)
        {
            startrow = 1
            endrow = i
            recovered_cases[i] = 0
        } else {
            startrow = startrow + 1
            endrow = i
            sum_of_recovered_cases = sum(my_country_data$new_cases[1:(startrow-1)])
            recovered_cases[i] = sum_of_recovered_cases
        }

        # Add up the last 14 days of active cases
        sum_of_active_cases_14days = sum(my_country_data$new_cases[startrow:endrow])

        # Store the result:
        active_cases[i] = sum_of_active_cases_14days
    }


    # Change the shape of the data to let ggplot read it more easily
    my_graph_data <- data.frame(location = location,
                                date  = c(as.Date(my_country_data$date), as.Date(my_country_data$date)),
                                cases = c(active_cases, recovered_cases),
                                type  = rep(c("Active cases", "Recovered"), each = length(active_cases)))

    return(my_graph_data)
}


plot_example_data <- function(example_graph_data, log_scale = FALSE) {
    # Check correct columns are present
    for (colname in c("location", "date", "cases", "type")) {
        if (!(colname %in% colnames(example_graph_data))) {
            abort(paste0("Input covid data must have a '", colname, "' column."), class = "missing_column")
        }
    }
    # Check log_scale is a single bool
    if (length(log_scale) != 1 || !is.logical(log_scale)) {
        abort("log_scale must be a single TRUE/FALSE value", class = "bad_input")
    }
    # Check data is NZ
    locations <- unique(example_graph_data$location)
    if (length(locations) != 1 || locations != "New Zealand") {
        abort("Example plotting function is only allowed to be used with New Zealand data. You must make your own ggplot from scratch for your country", class = "student_not_allowed")
    }

    # Make plot
    plot <- ggplot(example_graph_data, aes(x = as.Date(date), y = cases, col = type)) +
        geom_line(linewidth = 1) +
        geom_point(size = 1) +
        scale_x_date(date_labels = "%b %Y") +
        labs(x = "Date", y = "New Zealand cases", col = "Case type") +
        theme_bw()

    # Log-scale y axis if necessary
    if (log_scale) {
        plot <- plot + scale_y_log10() + labs(y = "New Zealand cases (log scale)")
    }

    # Return plot
    return(plot)
}
