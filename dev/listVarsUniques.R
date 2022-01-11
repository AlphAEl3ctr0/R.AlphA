listVarsUniques <- function(...){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		warning("! parameters manually defined inside function for tests. Do not use results !")
		tablesList <- list(DT1 = DT1, DT2 = DT2, DT3 = DT3)
	} else {
		if (inherits(..., "list")){
			tablesList <- (...)
		} else {
			tablesList <- list(...)
		}
	}
	countVals <- data.table()
	
	for (i in 1:length(tablesList)) {
		# tb <- tablesList[[1]]
		tb <- tablesList[[i]]
		countVals_int <- data.table()
		tbVarNames <- names(tb)
		for (tbVarName in tbVarNames) {
			# tbVarName <- tbVarNames[1]
			tbVarLength <- length(unique(tb[, get(tbVarName)]))
			countVals_int[ , (tbVarName) :=  tbVarLength]
		}
		countVals_int[, tbName := names(tablesList)[i]]
		countVals <- rbind(countVals, countVals_int, fill = TRUE)
	}
	return(countVals)
}

# testListVars <- listVarsUniques(DT1 = DT1, DT2 = DT2, DT3 = DT3)
# 
# 
# undebug(listVarsUniques)
# debug(listVarsUniques)
# DT1 <- data.table(a = 1:10)
# DT2 <- data.table(b = 2:10)
# DT3 <- data.table(b = 2:10, c = 3:11)



# rien a voir : timer.
# library(R.AlphA)
# TestTimer <- timer(start = T)
# for(i in 1:2e4){
# 	TestTimer <- timer(TestTimer, step = i)
# }
# plot(TestTimer$step, TestTimer$dt_seconds, log = "y")
# ggplot(data = TestTimer, aes(x = step, y = log(dt_seconds)))+ geom_line()# + ylim(c(-8,-2))
# TestTimer[, log_sec := log(dt_seconds)]
# TestTimer[, trunc_log_sec := trunc(log_sec)]
# table(TestTimer$trunc_log_sec)
# TestTimer[trunc_log_sec >= -4]