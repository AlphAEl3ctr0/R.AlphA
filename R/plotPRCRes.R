#' @title Describe IRP results
#' @description Quick plots describing IRP (PRC in french) results
#' @param data the final table giving IRP
#' @param var default age_vis, the variable on x axis
#' @return a plot of IRP by age (or by chosen var)
#' @import ggplot2
# #' @rawNamespace import(data.table, except =  c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#' @export
plotPRCRes <- function(
	data
	, var = "age_vis"
){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		message("! parameters manually defined inside function for tests. Do not use results !")
		data = test$results$t_VAP_gie_dep
		data = wk$results$t_VAP_gie_dep
		var = "age_vis"
		var = age_vis_name
	}
	sumUp <- data[
		!is.na(VAP_garantie_dep)
		, .(
			meanVAP = mean(VAP_garantie_dep, na.rm = T)
			, medianVAP = median(VAP_garantie_dep, na.rm = T)
			, minVAP = min(VAP_garantie_dep, na.rm = T)
			, maxVAP = max(VAP_garantie_dep, na.rm = T)
		)
		, by = var
		]
	ggplot(data = sumUp, aes(x = sumUp[, get(var)], sumUp$meanVAP)) +
		xlab(var) +
		geom_line(color = "green") +
		geom_line(aes(sumUp[, get(var)], sumUp$medianVAP), color = "grey") +
		geom_line(aes(sumUp[, get(var)], sumUp$minVAP), color = "blue") +
		geom_line(aes(sumUp[, get(var)], sumUp$maxVAP), color = "red")

}
