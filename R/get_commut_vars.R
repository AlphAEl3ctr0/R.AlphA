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
){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		message("! parameters manually defined inside function for tests. Do not use results !")
		root <- dirname(rstudioapi::getSourceEditorContext()$path)
		workRRoot <- stringr::str_extract(root, ".*WorkR")
		tbls <- readRDS(file.path(workRRoot, "pop stats", "INPUTS", "tables.rds"))
		library(R.AlphA)
		commutTable = Complete_commut(tbls$TH)
		popTable = generate_pop(10)[, age := year(dteVision) - year(dteNais)]
		# popTable = get_commut_vars(popTable, tbls$TH, "ax") # tests pour gerer le cas ou on a deja un ax : il faudrait l'ecraser au lieu de buguer
		commutVars = "ax"
		cle_commut = "age"
		cle_commut_pop = cle_commut
		cle_commut_com = cle_commut
	}
	# browser()
	# cle_commut <- c("age", "sexe") #peut-être stocker la cle dans une variable et la récupérer
	# popTable[, int_get_commut_key := 1:nrow(popTable)] # non : ca modifie la table passee en argument ce qui est de la merde
	popTable$int_get_commut_key = 1:nrow(popTable) # ok : ne modifie pas la table passee en argument
	variables_table_commut <- c(cle_commut_com, commutVars)
	variables_table_pop <- c(cle_commut_pop, "int_get_commut_key")
	popTable[, ..variables_table_pop]
	merged_table <- merge(
		x = popTable[ , ..variables_table_pop]
		# x = sPop
		, y = commutTable[, ..variables_table_commut]
		# , y = table_commut_finale[, ..variables_table_commut]
		, by.x = cle_commut_pop
		, by.y = cle_commut_com
		, all.x = TRUE # 2021.09.29
	)
	res_order_select <- merged_table[order(int_get_commut_key), ..commutVars]

	if (length(commutVars) >1) {
		# print(paste0(
		# 	"length commutVars : ", length(commutVars), " - returning table"
		# ))
		return(res_order_select)
	} else {
		# print(paste0(
		# 	"length commutVars : ", length(commutVars), " - returning vector"
		# ))
		return(res_order_select[, result := res_order_select[, 1]]$result) # assez moche, a corriger si possible (permet de renvoyer un vecteur plutot qu'une data.table)
	}

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
