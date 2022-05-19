#' @title Times steps - v3 with lubridate functions, and only stores time, not durations
#' @description keep track of time spent on different steps of code
#' @param start to start a new time table from scratch
#' @param timer_table table to increment with a new timer line
#' @param message Should the function print the table for this step ?
#' @param ... any other specification. Choose name for column and value for row
#' @return the provided timer_table plus one line with time and the ... specifications
#' @examples
#' tt_tests <- timer(start = TRUE)
#' tt_tests <- timer(timer_table = tt_tests)
#' tt_tests <- timer(timer_table = tt_tests, stepName = "step x", anyVariableName = "any value")

#' @import lubridate
#' @rawNamespace import(data.table, except =  c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#' @export

# library(lubridate)
# library(data.table)
# library(R.AlphA)
# library(dplyr)

# readable period ---------------------------------------------------------
readable_period <- function(periodObj){
	if_else(
		is.na(periodObj)
		, "NA"
		, sprintf(
			fmt = paste0(
				NULL
				, ifelse(periodObj@day, '%d days ', "%.d")
				, ifelse(periodObj@hour, '%dh:', "%.d")
				, ifelse(periodObj@minute, '%02dm:', "%.d")
				# , '%02dm:'
				, '%02ds'
				, ifelse(periodObj@day, "", '.%03dms')
			)
			, periodObj@day
			, periodObj@hour
			, periodObj@minute
			, lubridate::second(periodObj) %>% trunc
			, (periodObj@.Data%%1 * 1000) %>% trunc
		)
	)
}

# testPeriod <- c(testPeriod, NA)
# readable_period(testPeriod)
