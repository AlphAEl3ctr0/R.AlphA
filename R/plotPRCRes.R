# importer ggplot
plotPRCRes <- function(
	data
	, var = "age_vis"
){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		message("! parameters manually defined inside function for tests. Do not use results !")
		data = test$results$t_VAP_gie_dep
		var = "age_vis"
	}
	sumUp <- data[
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
