#' @title Random dates
#' @description get a vector of x random dates between min and max
#' @param x length of the vector
#' @param min opt : minimum date
#' @param max opt : maximum date
#' @param sort do you want sorted dates, default FALSE
#' @param include_hours should returned dates include time also (default FALSE : days only)
#' @return a dates vector of length x
#' @examples
#' rdate(10)
#' @importFrom lubridate decimal_date date_decimal
#' @importFrom stats runif
#' @export
rdate <- function(
	x
	, min = paste0(format(Sys.Date(), '%Y'), '-01-01')
	, max = paste0(format(Sys.Date(), '%Y'), '-12-31')
	, sort = FALSE
	, include_hours = FALSE
) {
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		print("manualrun (rdate)")
		x = 3
		testdates <- data.table(dtemin = seq.Date(as.Date("2021-01-01"), as.Date("2021-01-10"), by = "1 day"))
		testdates[, dtemax := dtemin + 50]
		testdates
		min = testdates$dtemin[1]
		max = testdates$dtemax[1]
		sort = FALSE
		include_hours = FALSE

	}
	max <- as.Date(max)
	min <- as.Date(min)
	if (!include_hours) {
		daysDiff <- max-min
		dates <- min + runif(x, 0, daysDiff + 1) %>% floor
	}
	if (include_hours) {
		dec_min <- lubridate::decimal_date(as.Date(min))
		dec_max <- lubridate::decimal_date(as.Date(max))
		dec_date <- stats::runif(x, min = dec_min, max = dec_max)
		dates <- lubridate::date_decimal(dec_date)
	}

	dates <- if (sort) sort(dates) else dates

	return(dates)
}


