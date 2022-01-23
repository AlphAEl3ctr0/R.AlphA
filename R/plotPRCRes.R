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
		data = copy(test$results$t_VAP_gie_dep)
		data = copy(wk$results$t_VAP_gie_dep)
		var = "age_vis"
		var = "dim_dif_age__anc_ct"
		var = age_vis_name
		var = "inc_age"
	}

	for (quant in 0:10) {
		quantName <- paste0("quant_", quant)
		quantProb = quant/10
		data[
			!is.na(VAP_garantie_dep) # pas sur que ce soit tjrs utile
			, (quantName) := quantile(VAP_garantie_dep, probs = quantProb)
			, by = var
			]
	}
	data[inc_age < 52]
	quantNames <- grep("quant_", names(data), value = TRUE)
	sumUp <- data[
		!is.na(VAP_garantie_dep)
		, .(
			meanVAP = mean(VAP_garantie_dep, na.rm = T)
			, sum_nb_lines = sum(nb_lines)
		)
		, by = c(var, quantNames)
		]
	sumUp
	# bien moche mais en attendant de savoir le faire mieux
	ggplot(data = sumUp, aes(x = sumUp[, get(var)], sumUp$meanVAP)) +
		xlab(var) +
		geom_line(color = "green") +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_0), color = "blue", alpha = 0.2) +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_1), color = "blue", alpha = 0.3) +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_2), color = "blue", alpha = 0.4) +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_3), color = "blue", alpha = 0.5) +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_4), color = "blue", alpha = 0.6) +
		# geom_line(aes(sumUp[, get(var)], sumUp$quant_5), color = "blue", alpha = 0.5) +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_6), color = "red" , alpha = 0.6) +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_7), color = "red" , alpha = 0.5) +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_8), color = "red" , alpha = 0.4) +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_9), color = "red" , alpha = 0.3) +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_10), color = "red" , alpha = 0.2)

}
