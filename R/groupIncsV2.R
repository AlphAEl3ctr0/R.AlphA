# rm(list = ls())
#' function to group incs when a life table has 2 incs
#' @description given a table, return the same table with a new dim and an
#' incgrp (and sorted)
#' @param data the life table with 2 incs
#' @param incPattern default : "inc_". Pattern to identify inc columns
#' @importFrom stringr str_extract str_detect
#' @importFrom data.table copy
#' @export
groupIncsV2 <- function(data, incPattern = "inc_"){

###############

{
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		warning("! parameters manually defined inside function for tests. Do not use results !")
		{
			library(R.AlphA)
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
		t_vie <- copy(tbls$c$t_vie)
		dims_vie_res <- compareVars(
			pattern = "dim_|inc_"
			, t_vie
			, t_res
		)
		# on cherche a avoir 2 colonnes d'inc
		m_vie_res <- merge(
			all=F
			, x = t_vie[, c(.SD, .(cst = T, lx_vie = val_lx)), .SDcols = c(dims_vie_res$xVars)]
			, y = t_res[, c(.SD, .(cst = T, qx_res = res_rate)), .SDcols = c(dims_vie_res$yVars)]
			, by = c("cst",dims_vie_res$common)
			, allow.cartesian = T
		)[, cst := NULL]
		data <- copy(m_vie_res)
	}
} # manualRun
{
	incs <- grep(names(data), pattern = "inc_", value = T)
	if (length(incs) == 0) {
		message("no inc column")
		return(NULL)
	} else if (length(incs)== 1) {
		message("only one inc column")
		return(NULL)
	} else if (length(incs) > 2) {
		stop("more than 2 incs : too much")
	}
} # spot inc Columns and check if there are exactly 2 (else : return NULL)
{
	dimDiffName <-  paste(
		collapse = "__"
		, str_remove_all(incs, "^inc_")
	)
	dimDiffPrefix <- "dim_dif_"
	dimDiffFullName <- paste0(dimDiffPrefix, dimDiffName)
} # build name for the new dim column
dimDiffVector <- data[, get(incs[1]) - get(incs[2])]
dimDiffDT <- data.table(
	x = data[, get(incs[1]) - get(incs[2])]
)
setnames(dimDiffDT, "x", dimDiffFullName)
{
	# data[, (dimDiffFullName) := get(incs[1]) - get(incs[2])] # add the dim col
	# data$dim_dif_age__anc_ct <- NULL
	# data
	# cbind(data, dimDiffDT)
	# cbind(data, NULL)
	# data[, testCol := dimDiffDT]
} # tests
{
	# {
	# 	dimColumns <- grep(names(data), pattern = "dim_|dif_", value = T)
	# 	separator <- "_"
	# 	data[
	# 		, dims := do.call(paste, c(.SD, sep = separator))
	# 		, .SDcols = dimColumns
	# 		] # verifier avec un vecteur vide
	# } # add a "dims" column with all dims
	# group_ordered <- data[order(dims, get(incs[1]))] # order by dims and inc
	#
	# {
	# 	incName <- paste(
	# 		str_remove_all(incs, "^inc_")
	# 		, collapse = "__"
	# 	)
	# 	incName_full <- paste0("inc_grp_", incName)
	# } # build name for the new inc column
	# group_ordered[, (incName_full) := 1:.N, by = .(dims)]
	# return(group_ordered)
} # ajout d'une inc_grp --> finalement inutile
return(dimDiffDT)

}
