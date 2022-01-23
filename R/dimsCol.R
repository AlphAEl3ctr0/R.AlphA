#' Long-term care EPV
#' @description give a column with concatenation of other columns by pattern
#' @param data a data set in which columns are searched
#' @param separator to separate column values
#' @param pattern default is "dim_" to get dimensions. use another one if u like
#' @importFrom data.table copy
#' @export
dimsCol <- function(data, separator = "_", pattern = "dim_"){
	# voir si on a besoin d'importer data.table en entier ?
	# data <- m_VIR # for manualRun

	wkTbl <- as.data.table(copy(data))
	wkTbl [, dimsCols_index := 1:.N]
	{
		dimColumns <- grep(names(wkTbl), pattern = pattern, value = T)
		wkTbl[
			, dims := do.call(paste, c(.SD, sep = separator))
			, .SDcols = dimColumns
			] # verifier avec un vecteur vide
	} # add a "dims" column with all dims

	indexOK <- all.equal(1:nrow(wkTbl), wkTbl$dimsCols_index)
	if (!indexOK) {
		stop("error in dimsCol() : line order has changed")
	} else {
		return(wkTbl$dims)
	}
}
