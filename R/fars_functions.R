#' Read .csv file
#'
#' Simple function to read a .csv file from the \code{filename} path provided.
#' It first verifies if the \code{filename} path is valid, and stops the function if not.
#' the readr::read_csv function is used to read the file, with all messages suppressed and progress to FALSE.
#' @param filename string path to the file to be read.
#'
#' @return a tibble containing the data read from the filename parameter. Uses readr::read_csv to load the data.
#' @export
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' x <- fars_read("data.csv")
#' }
#'
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create \code{filename} from the \code{year} provided.
#'
#' Function that takes a year input and outputs a string with the following structure: accident_YEAR.csv.bz2
#' Made to retrieve data  from the \href{http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}{US National Highway Traffic Safety Administration's Fatality Analysis Reporting System}
#' @param year A year
#'
#' @return a string with the following structure: accident_YEAR.csv.bz2
#' @export
#'
#'@examples
#'\dontrun{
#'filepath <- make_filename(2012)
#'}
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read data from a list of years
#'
#' Made to retrieve data  from the \href{http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}{US National Highway Traffic Safety Administration's Fatality Analysis Reporting System}
#'
#' @param years a list of years to analyse. All years must be in a similar class.
#'
#'
#' @return a list of dataframes with datasets months and years columns. Returns an error if the data connot be read or if the MONTH or year columns cannot be subsetted.
#' @return Uses \link{make_filename} to create the files paths. Uses \link{fars_read} to read the data.
#' @seealso \link{make_filename} for how the create of file names, \link{fars_read} for reading the data.
#' @export
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr `%>%`
#'
#' @examples
#' \dontrun{
#' fars_read_years(list("2012", "2013"))
#' }
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Count number of accidents by year and month from list of years dataframes
#'
#' Made to retrieve data  from the \href{http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}{US National Highway Traffic Safety Administration's Fatality Analysis Reporting System}
#' @param years a list of years to analyse. All years must be in a similar class.
#'
#' @return a dataframe counting the number of accidents per month and year.
#'
#' Years are presented side by side. \link{fars_read_years} is used to read the data.
#' @seealso \link{fars_read_years}
#' @export
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr `%>%`
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(list(2012, 2013))
#' fars_summarize_years(list("2012", "2013"))
#' }
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Create a map of all accidients for a year
#'
#' Made to retrieve data  from the \href{http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}{US National Highway Traffic Safety Administration's Fatality Analysis Reporting System}
#' @param state.num unique ID number for a state (from 1 to 50)
#' @param year a year to be processed
#'
#' @return a maps::map object with the map of accidents for the provided year and state.
#'
#' Returns an error if the \code{state.num} is not in the dataframe. Returns a warning if there is no accident to plot.
#'
#' @seealso \link{make_filename}, \link{fars_read}, \link[maps]{map}
#' @export
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @examples
#' \dontrun{
#' fars_map_state("1", 2014)
#' }
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
