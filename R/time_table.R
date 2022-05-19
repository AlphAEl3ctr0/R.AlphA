#' @title Times steps
#' @description previous version - deprecated - use timer
#' @param tt_message mandatory
#' @param tt_reset to start a new time table from scratch
#' @param ... any other specification. Choose name for column and value for row
#' @return nothing, creates a dt in parent environment : maybe not the best practice
#' @examples
#' time_table("step 1")
#' time_table("step 2")

#' time_table("starting new cycle", lines_volume = 100)
#' time_table("starting new cycle", lines_volume = 1000)
#' @import lubridate
#' @rawNamespace import(data.table, except =  c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#' @export
time_table <- function(tt_message, tt_reset = FALSE, ...){
	# browser()
	if(tt_reset){rm(time_all, inherits = T)}
	# t1 : heure de debut
	if(exists("time_all")){
		t1 <- max(time_all$heure)
		# t1 <- as.POSIXct(as.POSIXlt(format(as.POSIXct(max(time_all$heure)))))
	} else {
		t1 <- Sys.time()
		# t1 <- as.POSIXct(as.POSIXlt(format(as.POSIXct(Sys.time()))))
	}
	# t2 : heure de fin (maintenant)
	t2 <- Sys.time()
	# t2 <- as.POSIXct(as.POSIXlt(format(as.POSIXct(Sys.time()))))
	# dt : difference entre h_fin et h_debut	et remise en forme
	dt_interval <- t1 %--% t2
	dt_seconds <- dt_interval / dseconds() # pour recuperer une colonne numerique
	dt_period <- as.period(dt_interval)
	dt_second_part <- trunc(dt_period@.Data)
	dt_ms_part <- trunc((dt_period@.Data - dt_second_part)*1000)
	dt <- sprintf('%02d:%02d,%03d', dt_period@minute, dt_second_part, dt_ms_part)
	# time_inter : remet les resultats sous forme de table
	time_inter <- data.table(message=tt_message, heure=t2, dt=dt, dt_seconds=dt_seconds)
	if (!exists("time_all")) time_all <- time_inter[0]
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
	# time_all : variable globale
	time_all_int <- rbind(time_all,time_inter, fill = T)
	time_all <<- time_all_int
}



# c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek)
