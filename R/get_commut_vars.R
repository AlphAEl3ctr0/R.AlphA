# rm(list = ls())
#fonction de jointure avec la table de commut :  definition de la cle, choix des variables à récupérer--------
# réfléchir au format que devrait avoir la sortie de cette fonction
#' ajouter des nombres de commut a une table
#' @description obtenir les nombres de commutation a partir d'une table de pop
#' @param popTable pop table used as input
#' @param commutTable mortality table to get commut from
#' @param commutVars required commut columns
#' @param cle_commut how to join tables, default = by age
#' @param cle_commut_pop to use if key is not the same for pop and commut
#' @param cle_commut_com to use if key is not the same for pop and commut
# #' @importFrom stats runif
#' @export

get_commut_vars <- function(
	popTable
	, commutTable
	, commutVars
	, cle_commut = "age"
	, cle_commut_pop = cle_commut
	, cle_commut_com = cle_commut
	, key_pattern = "dim_|inc_"
){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		message("! parameters manually defined inside function for tests. Do not use results !")
		root <- dirname(rstudioapi::getSourceEditorContext()$path)
		workRRoot <- stringr::str_extract(root, ".*WorkR")
		tbls <- readRDS(file.path(workRRoot, "pop stats", "INPUTS", "tables.rds"))
		library(R.AlphA)
		commutTable = Complete_commut(tbls$classic$t_vie)
		popTable = generate_pop(10)[, age := year(dteVision) - year(dteNais)]
		setnames(popTable, old = c("sexe", "age"), new = c("dim_sexe", "inc_age"))
		popTable[ dim_sexe == "H", dim_sexe := "M"]
		popTable
		# popTable = get_commut_vars(popTable, tbls$TH, "ax") # tests pour gerer le cas ou on a deja un ax : il faudrait l'ecraser au lieu de buguer
		commutVars = "ax"
		cle_commut = "age"
		cle_commut_pop = cle_commut
		cle_commut_com = cle_commut
		key_pattern = "dim_|inc_"
	}
	# 1 - index
	popTable$popIndex = 1:nrow(popTable) # ok : ne modifie pas la table passee en argument

	popTable
	commutTable
	library(rlang) # pour missing_arg() --> a etudier

	# 2 - key
	commutTable$gc_key <- dimsCol(commutTable, pattern = key_pattern)
	popTable$gc_key    <- dimsCol(popTable   , pattern = key_pattern)

	# 3 - merge
	# plutot utiliser mergeLifeTable ?
	# mergedPopCom <- merge(popTable, commutTable, by  = "gc_key", all.x = TRUE)
	# anyDuplicated(mergedPopCom$popIndex)
	variables_table_commut <- c("gc_key", commutVars)
	variables_table_pop <- c("gc_key", "popIndex")
	# popTable[, ..variables_table_pop]
	merged_table <- merge(
		x = popTable[ , ..variables_table_pop]
		# x = sPop
		, y = commutTable[, ..variables_table_commut]
		# , y = table_commut_finale[, ..variables_table_commut]
		# , by.x = cle_commut_pop
		# , by.y = cle_commut_com
		, by = "gc_key"
		, all.x = TRUE # 2021.09.29
	)

	# 4 - reorder, check, select
	res_order <- merged_table[order(popIndex)]
	checkIndex <- all.equal(res_order$popIndex, popTable$popIndex)
	if (!checkIndex) {
		stop("index has changed during merge")
	} else {
		res_order_select <- merged_table[order(popIndex), ..commutVars]
	}
	as.vector(res_order_select)

	if (length(commutVars) >1) {
		# print(paste0(
		# 	"length commutVars : ", length(commutVars), " - returning table"
		# ))
		gcResult <- res_order_select
	} else {
		# print(paste0(
		# 	"length commutVars : ", length(commutVars), " - returning vector"
		# ))
		gcResult <- res_order_select[, result := res_order_select[, 1]]$result # assez moche, a corriger si possible (permet de renvoyer un vecteur plutot qu'une data.table)
	}

	return(gcResult)
}

# rm(list = ls())
# library(R.AlphA)
# testPop <- generate_pop(10)[, age := year(dteVision) - year(dteNais)]
# root <- dirname(rstudioapi::getSourceEditorContext()$path)
# inputPath <- file.path(root, "../../pop stats/INPUTS")
# # print("/Users/Raphael/Google Drive/WorkR/pop stats/INPUTS/tables.rds")
# tables <- readRDS(file.path(inputPath, "tables.rds"))
#
# testPop$lx <- get_commut_vars(testPop, Complete_commut(tables$TH, 1/100), "lx")
#
# merge(
# 	testPop
# 	, tables$TH
# )
#
# testPop
# library(data.table)
# testPop$test <- data.table(a = 1:10)
