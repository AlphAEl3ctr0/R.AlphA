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

# init --------------------------------------------------------------------
tmr_init <- function(...){
	data.table(tmrDateTime = now(), ...)
}


# tmr ---------------------------------------------------------------------
tmr <- function(tmTbl, message = F, ...){
	time_interm <- data.table(tmrDateTime = now(), ...)
	rbind(tmTbl, time_interm, fill = TRUE)
}


# compute -----------------------------------------------------------------
tmr_compute <- function(tmTbl, starts = TRUE){
	tmTbl %>%
		arrange(tmrDateTime) %>%
		mutate(
			tmrPrevTime = lag(tmrDateTime)
			, tmrNextTime = lead(tmrDateTime)
			, tmrDtToPrev = tmrDateTime - tmrPrevTime
			, tmrDtToNext = tmrNextTime - tmrDateTime
			, tmrDt = if(starts) tmrDtToNext else tmrDtToPrev
		) %>%
		select(
			- tmrPrevTime, -tmrDtToNext, -tmrDtToPrev, -tmrNextTime
		)
}


# vis ? -------------------------------------------------------------------
tmr_vis <- function(tmTblComputed){
	tmTblComputed %>%
		mutate(
			NULL
			, tmrDtPeriod = as.period(tmrDt)
			, tmrDate = date(tmrDateTime)
			, tmrHour2 = format(tmrDateTime, "%H:%M:%S")
			, test = readable_period(tmrDtPeriod)
		) %>%
		select(
			-tmrDtPeriod
			, - tmrDateTime
			, - tmrDt
			# , - tmrDtPeriod
			# , tmrDate
		)

}

# tmr_init() %>% tmr %>% tmr_compute %>% tmr_vis


# tests -------------------------------------------------------------------
# tmTbl <- tmr_init()
# for (step in 1:1E3) {
# 	tmTbl <- tmr(tmTbl, step = step)
# }
#
# tmr_vis(tmTbl %>% tmr_compute)
# max(tmTbl$tmrDateTime) - min(tmTbl$tmrDateTime)
#
# tmTblComputed <- tmTbl %>% tmr_compute
# test <- tmTblComputed$tmrDt[1] * c(1E2,1E6, 1E8);test
# testPeriod <- as.period(test);testPeriod
# readable_period(testPeriod)
#
#
# tmTblOld <- timer(start = TRUE)
# for (step in 1:1E3) {
# 	tmTblOld <- timer(tmTblOld, step = step)
# }
# max(tmTblOld$heure) - min(tmTblOld$heure)
# library(ggplot2)
# ggplot(tmTblOld, aes(step, dt_seconds))+
# 	geom_line()+
# 	ylim(c(0,0.004))
#
# t1 <- tmTbl$tmrDateTime[7]
# t2 <- tmTbl$tmrDateTime[8]
#
# tdiff <- t2 - t1
#
# microseconds(tdiff)
# firstTime %>% class()



