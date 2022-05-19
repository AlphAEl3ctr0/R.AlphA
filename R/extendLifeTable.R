#' @title extend a life Table by qx
#' @description add lines at the end of a life table, based on the last qx
#' @param lifeTbl the table to extend
#' @param nRowsAdd number of rows to add
#' @param reOrder should the data be reordered by dims + inc after treatment
#' @return the same columns, with more rows
#' @importFrom data.table copy
#' @export
extendLifeTable <- function(
	lifeTbl
	, nRowsAdd
	, reOrder = TRUE
){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		warning("! parameters manually defined inside function for tests. Do not use results !")
		rm(list = ls())
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
		lifeTbl <- copy(tbls$classic$t_inc[dim_type_dep == "dp"])[inc_age>90]
		nRowsAdd <- 5
		reOrder <- T
	}


	tblToExtend <- copy(lifeTbl)
	incName <- grep("inc_", names(tblToExtend), value = TRUE)
	tblToExtend[, tmpInc := get(incName)]
	tblToExtend$qx <- Complete_commut(tblToExtend)$qx
	tblToExtend$lx <- NULL
	maxInc <- max(tblToExtend[!is.na(qx)]$tmpInc)
	nwLines <- tblToExtend[tmpInc == maxInc]
	extension <- merge(
		nwLines[, cst := T]
		, data.table(incAdd=1:nRowsAdd, cst = T)
		, by = "cst"
		, allow.cartesian = T
	)[, cst := NULL]
	extension[, (incName) := tmpInc+incAdd]
	extension$incAdd <- NULL
	fullTable <- rbind(tblToExtend, extension)[!is.na(qx)]
	fullTable[, tmpDims := dimsCol(fullTable)] # autoriser une pattern
	fullTable$lx <- qx_to_lx(fullTable, incName = incName, dimsNames = "tmpDims")
	if(reOrder) fullTable <- fullTable[order(tmpDims, tmpInc)]
	fullTable$tmpDims <- NULL
	fullTable$tmpInc <- NULL
	fullTable$qx <- NULL
	return(fullTable)
}

