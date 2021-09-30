#' @title Times steps - new version : don't create time table as global variable cause it sucks
#' @description keep track of time spent on different steps of code
#' @param start to start a new time table from scratch
#' @param timer_table table to increment with a new timer line
#' @param ... any other specification. Choose name for column and value for row
#' @return the provided timer_table plus one line with time and the ... specifications
#' @examples
#' tt_tests <- timer(start = TRUE)
#' tt_tests <- timer(timer_table = tt_tests)
#' tt_tests <- timer(timer_table = tt_tests, stepName = "step x", anyVariableName = "any value")

#' @import lubridate
#' @rawNamespace import(data.table, except =  c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#' @export
timer <- function(timer_table = data.table(),start = FALSE, ...){
	# t1, t2
		if(start) {
			t1 <- Sys.time()
		} else {
			t1 <- as.POSIXct(as.POSIXlt(format(as.POSIXct(max(timer_table$heure))))) # voir si il existe une meilleure methode...
		}
		t2 <- Sys.time()
	# dt : difference entre h_fin et h_debut	et remise en forme
		dt_interval <- t1 %--% t2
		dt_seconds <- dt_interval / dseconds() # pour recuperer une colonne numerique
		dt_period <- as.period(dt_interval)
		dt_second_part <- trunc(dt_period@.Data)
		dt_ms_part <- trunc((dt_period@.Data - dt_second_part)*1000)
		# dt <- sprintf('%02d:%02d,%03d', dt_period@minute, dt_second_part, dt_ms_part)
		if (dt_seconds<60) {
			# print("seconds only")
			dt <- sprintf('%0d.%03d', dt_second_part, dt_ms_part)
		} else if (dt_seconds<60*60){
			# print("max : minutes")
			dt <- sprintf('%0d:%02d.%03d', dt_period@minute, dt_second_part, dt_ms_part)
		} else if (dt_seconds<60*60*24){
			# print("max : hours")
			dt <- sprintf('%0d:%02d:%02d.%03d',dt_period@hour, dt_period@minute, dt_second_part, dt_ms_part)
		} else {
			# print("max : more than hours anyway")
			dt <- sprintf('%0d:%02d:%02d:%02d.%03d',dt_period@day, dt_period@hour, dt_period@minute, dt_second_part, dt_ms_part)
		}
	# time_inter : remet les resultats sous forme de table
		time_inter <- data.table(heure=t2, dt=dt, dt_seconds=dt_seconds)
	if (start) timer_table <- time_inter[0]
	# ajout des eventuelles colonnes supplementaires
		x = list(...)
		names_values <- data.table(
			name = names(x)
			, value = eval(x)
		)
		if (nrow(names_values)>0) {
			for (arg in 1:nrow(names_values)) {
				time_inter[, (names_values$name[arg]) := names_values$value[arg]]
			}
		}
	# timer_table :
		timer_table_int <- rbind(timer_table,time_inter, fill = TRUE)
		return(timer_table_int)
		# timer_table <<- timer_table_int
}

#
# t1 <- Sys.time() - hours(2)
# t2 <- Sys.time()
# dt_interval <- t1 %--% t2
# dt_seconds <- dt_interval / dseconds() # pour recuperer une colonne numerique
# dt_period <- as.period(dt_interval)
# dt_second_part <- trunc(dt_period@.Data)
# dt_ms_part <- trunc((dt_period@.Data - dt_second_part)*1000)
# timeFormat <- '% %02d,%03d'
# timeFormat <- '%02d:%02d,%03d'
# to_display <- list(dt_period@minute, dt_second_part, dt_ms_part)
# sprintf(timeFormat, dt_period@minute, dt_second_part, dt_ms_part)
# sprintf(timeFormat, to_display)


# library(data.table)
# library(lubridate)
# tt_tests <- timer(start = TRUE)
# tt_tests <- timer(timer_table = tt_tests)
# tt_tests <- timer(timer_table = tt_tests, step = "new test")
# for (log10loop in 1:8) {
# 	for(itest in 1:10**log10loop){
# 	}
# 	tt_tests <- timer(timer_table = tt_tests, logloop = log10loop)
# }
# # c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek)
#
# tt_tests
