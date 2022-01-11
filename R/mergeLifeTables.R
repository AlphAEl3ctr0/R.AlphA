# rm(list = ls())
#' merge (cross-join ?) life tables
#' @description given 2 prefixed life tables, merges them on common dims and
#' makes a cross-join for exclusive dims
#' @param XTbl "left" table
#' @param YTbl "right" table
#' @param valPatt pattern for value columns
#' @param xName name of X tabl, used to rename duplicate columns
#' @param yName name of Y tabl, used to rename duplicate columns
#' @export
mergeLifeTables <- function(XTbl, YTbl, valPatt = "^qx$", xName, yName){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		warning("! parameters manually defined inside function for tests. Do not use results !")
		{
			library(R.AlphA)
			# library(data.table)
			root <- dirname(rstudioapi::getSourceEditorContext()$path)
			workRRoot <- stringr::str_extract(root, ".*WorkR")
			setwd(root)
			# lum_0_100(20)
			inputPath <- file.path(workRRoot, "pop stats", "INPUTS") #chemin dossier inputs
		}
		tbls <- readRDS(file.path(inputPath,"tables.rds"))
		tbls$classic$t_vie
		nRowsDftTable <- 100
		t_res <- data.table(
			inc_anc_ct = 1:nRowsDftTable
			, res_rate = seq(from = 0.12, to = 0.03, length.out = nRowsDftTable)
		)
		XTbl <- Complete_commut(tbls$classic$t_vie)
		YTbl <- t_res[, qx := res_rate]
		valPatt <- "^qx$"
		xName <- "test"
		yName <- "aussitesT"
	}


	dimsXY <- compareVars(XTbl, YTbl, pattern = "^dim_")
	incsXY <- compareVars(XTbl, YTbl, pattern = "^inc_")
	valCols <- compareVars(XTbl, YTbl, valPatt)
	# if (length(valCols$common)!=1) {
	# 	if (length(valCols$xVars) == 0) XTbl <- Complete_commut(XTbl)
	# 	if (length(valCols$yVars) == 0) YTbl <- Complete_commut(YTbl)
	# }

	xPart <- XTbl[, .SD, .SDcols = c(dimsXY$xVars, incsXY$xVars, valCols$xVars)]
	yPart <- YTbl[, .SD, .SDcols = c(dimsXY$yVars, incsXY$yVars, valCols$yVars)]
	# m_vie_inc <- merge(
	# 	all=F
	# 	, x = t_vie_commut_filter[, c(.SD, .(qx_vie = qx, age = inc)), .SDcols = c(dims_vie_inc$xVars)]
	# 	, y = t_inc_commut_filter[, c(.SD, .(qx_inc = qx, age = inc)), .SDcols = c(dims_vie_inc$yVars)]
	# 	, by = c("age",dims_vie_inc$common)
	# )
	XY <- merge(
		all=F
		, x = xPart[, cst := TRUE]
		, y = yPart[, cst := TRUE]
		, by = c("cst", dimsXY$common, incsXY$common)
		, allow.cartesian = TRUE
	)[, cst := NULL]
	if (!missing(xName)) {
		names(XY) <- str_replace_all(names(XY), "\\.x", paste0("_", xName))
	}
	if (!missing(yName)) {
		names(XY) <- str_replace_all(names(XY), "\\.y", paste0("_", yName))
	}
	# showNA <- XY[is.na(qx_X)]

	return(XY)
}



