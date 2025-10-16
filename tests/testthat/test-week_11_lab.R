## Load overall covid data
covid_data <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/refs/heads/master/public/data/owid-covid-data.csv", show_col_types = F)
nz_data <- filter(covid_data, location == "New Zealand", date <= as.Date("2021-12-30"))
nz_graph_data <- calculate_active_recovered_cases(nz_data)

test_that("NZ data filtering works", {
    expect_equal(ncol(nz_graph_data), 4)
    expect_equal(colnames(nz_graph_data), c("location", "date", "cases", "type"))
    expect_equal(unique(nz_graph_data$location), "New Zealand")
})

test_that("Calculate active recovered cases rejects bad data", {
    australia_data <- filter(covid_data, location == "Australia")
    expect_error(calculate_active_recovered_cases(select(australia_data, !location)), class = "missing_column")
    expect_error(calculate_active_recovered_cases(select(australia_data, -location, -date)), class = "missing_column")
    expect_error(calculate_active_recovered_cases(select(australia_data, !date)), class = "missing_column")
    expect_error(calculate_active_recovered_cases(select(australia_data, !new_cases)), class = "missing_column")

    nz_aus_data <- filter(covid_data, location %in% c("New Zealand", "Australia"))
    expect_error(calculate_active_recovered_cases(nz_aus_data), class = "bad_input")
})

test_that("Example plot construction works", {
    expect_true("ggplot" %in% class(ggplot_nz_example(nz_graph_data)))
})
