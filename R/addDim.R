# rm(list = ls())
#' adds a dimension to a table - for testing purposes on cross joins
#' @description add dim to a table
#'
#'
#'
#' @param DT the DataTable you want to manipulate
#' @param dim unique values you want your table to be duplicated over
#' @param dimName name of the new column
# #' @importFrom stringr str_extract str_detect
#' @export
addDim <- function(DT, dim, dimName){ # serait plus propre avec ... mais balec
	# dim = c("A","B")
	# DT <- data.table(a = 1:2)
	# dimName <- "jeanPierre"
	copyDT <- copy(DT)
	tabledDim <- as.data.table(dim)
	setnames(tabledDim, "dim", dimName)
	result <- merge(
		copyDT[, cst := T]
		, tabledDim[, cst := T]
		, by = "cst", allow.cartesian = T
	)[, cst := NULL]

	return(result)
}
