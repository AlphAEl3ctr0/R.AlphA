#' @title Describe IRP results
#' @description Quick plots describing IRP (PRC in french) results
#' @param data the final table giving IRP
#' @param var default age_vis, the variable on x axis
#' @return a plot of IRP by age (or by chosen var)
#' @import ggplot2
#' @export
plotPRCRes <- function(
	data
	, var = "age_vis"
){

################################################################################
{
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		message("! parameters manually defined inside function for tests. Do not use results !")
		{
			root <- dirname(rstudioapi::getSourceEditorContext()$path)
			workRRoot <- stringr::str_extract(root, ".*WorkR")
			library(R.AlphA)
			library(data.table)
			library(ggplot2)
			library(stringr)
			library(lubridate)
			tbls <- readRDS(file.path(workRRoot, "pop stats", "INPUTS","tables.rds"))
		} # initialisation - tables loading
		# data = copy(test$results$t_VAP_gie_dep)
		# data = copy(wk$results$t_VAP_gie_dep)
		var = "age_vis"
		var = "dim_dif_age__anc_ct"
		var = "inc_age"
		data <- dep_table(tbls$classic) ; var = "inc_age"
	}
} # manual run
wkData <- copy(data)
{
	for (quant in 0:10) {
		quantName <- paste0("quant_", quant)
		quantProb = quant/10
		wkData[
			!is.na(VAP_garantie_dep) # pas sur que ce soit tjrs utile
			, (quantName) := quantile(VAP_garantie_dep, probs = quantProb)
			, by = var
			]
	}
	quantNames <- grep("quant_", names(wkData), value = TRUE)
	sumUp <- wkData[
		!is.na(VAP_garantie_dep)
		, .(
			meanVAP = mean(VAP_garantie_dep, na.rm = T)
			, sum_nb_lines = sum(nb_lines)
		)
		, by = c(var, quantNames)
		]
} # ajout des quantiles
{
	# bien moche mais en attendant de savoir le faire mieux
	p <- ggplot(data = sumUp, aes(x = sumUp[, get(var)], sumUp$meanVAP)) +
		xlab(var) +
		geom_line(color = "green") +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_0), color = "black", alpha = 0.5, linetype = "dashed") +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_1), color = "blue", alpha = 0.3) +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_2), color = "blue", alpha = 0.4) +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_3), color = "blue", alpha = 0.5) +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_4), color = "blue", alpha = 0.6) +
		# geom_line(aes(sumUp[, get(var)], sumUp$quant_5), color = "blue", alpha = 0.5) +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_6), color = "red" , alpha = 0.6) +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_7), color = "red" , alpha = 0.5) +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_8), color = "red" , alpha = 0.4) +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_9), color = "red" , alpha = 0.3) +
		geom_line(aes(sumUp[, get(var)], sumUp$quant_10), color = "black" , alpha = 0.5, linetype = "dashed")

	p  + theme_dark()+ theme(plot.background = element_rect(fill = "dark grey"))
} # plot


################################################################################
}
