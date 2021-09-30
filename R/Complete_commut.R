# rm(list = ls())
#' adding commutation numbers to a mortality table
#' @description from lxTable and given act rate, adds commut columns
#' @param lxTable 2 columns : age and lx
#' @param i actualisation rate
#' @param dimNames = c("sexe")
#' @param incName = "age"
#' @param messages = FALSE
#' @importFrom readr parse_number
#' @export
Complete_commut <- function(
	lxTable
	, i # pas de valeur par deft, pour pouvoir engueuler l'utilisateur si non rempli
	, dimNames = NULL
	, incName = NULL
	, messages = FALSE
){
	manualrun <- F
	manualrun <- T
	if (manualrun) {
		message("! parameters manually defined inside function for tests. Do not use results !")
		tbls <- readRDS(list.files("../pop stats/INPUTS", "tabl.*rds", full.names = T))
		lxTable = tbls$TE[1:10**4] ; dimNames = NULL # TE : table de survie bien longue (10k lignes)
		lxTable = rbind(
			tbls$TH[1:3][, sexe := "H"]
			, tbls$TF[1:3][, sexe := "F"]
		) ; dimNames = c("sexe") # pour commencer a bosser sur les multi dimensions
		i = 0
		# dimNames = c("sexe","lx")
		incName = "age"
		lxTable = tbls$lg_maintien_HF ; dimNames = c("x", "sexe") ; incName = "anc"
		# key_vars = c(dimNames, incName)
		# lxTable = tbls$BT1_prefixed
		library(data.table)
		messages = T
	}
	tableToFill <- as.data.table(copy(lxTable))
	if(missing(i)) {message("i is missing : set to 0 by default"); i <- 0}
	v <- 1/(1+i)
	# identifier les noms dans la table
		namesTable <- data.table(names = names(tableToFill))
		namesTable[, find_pattern := stringr::str_extract(names,"^(dim|val|inc)(?=_)")];namesTable
		autoVal <- namesTable[find_pattern == "val"]$names
		if(length(autoVal)) {
			valName <- namesTable[find_pattern == "val"]$names
		} else {
			valName <- "lx"
		}
		if (missing(incName)) incName <- namesTable[find_pattern == "inc"]$names
		if (missing(dimNames)) dimNames <- namesTable[find_pattern == "dim"]$names

		tableToFill[, lx := get(valName)]
		tableToFill[is.na(lx), lx := 0] # retraitement. # voir comment traiter le nom, peut etre pas toujours lx

	# renommer les variables
	# tip pour renommer efficacement :
		# https://stackoverflow.com/questions/46287324/concatenate-values-across-columns-in-data-table-row-by-row
		# x[, key_ := do.call(paste, c(.SD, sep = "_")), .SDcols = names(x)]
	separator <- "_"
	if (is.null(dimNames)) {
		tableToFill[, dims := ""]
	} else {
		tableToFill[, dims := do.call(paste, c(.SD, sep = separator)), .SDcols = dimNames] # verifier avec un vecteur vide
	}
	tableToFill[, inc := readr::parse_number(as.character(get(incName)))] # finalement c'est bien oblige de l'avoir en nombre (pour trier)
	tableToFill[, key := do.call(paste, c(.SD, sep = separator)), .SDcols = c("dims", "inc")]
	tableToFill[, key_names := paste(c(dimNames, incName), collapse = separator)]
	tableToFill[, inc_p1 := inc+1]
	tableToFill[, key_xp1 := do.call(paste, c(.SD, sep = separator)), .SDcols = c("dims", "inc_p1")]

	# # du coup ca a l'air de servir a rien de renommer
		# data.table::setnames(tableToFill, old =  dimNames, new =  paste0("dim_", 1:length(dimNames))
		# data.table::setnames(tableToFill, old =  incName, new =  "inc_var")
	# version 3 : 'join', jointure pour avoir lx+1, et boucle seulement pour Nx
		tableToFill_lxp1 <- merge(
			tableToFill
			, tableToFill[ , .(key, lxp1 = lx)]
			, by.x = "key_xp1"
			, by.y = "key"
			, all.x = T
		)

		tableToFill_lxp1[, px := lxp1 / lx]  			# px
		tableToFill_lxp1[, qx := 1 - px] 				# qx
		tableToFill_lxp1[, Dx := lx * v^inc]			# DX
		tableToFill_lxp1[is.na(tableToFill_lxp1)] <- 0

		dim_combinaisons <- unique(tableToFill_lxp1$dims)
		tableToFill_Nx <- tableToFill_lxp1[0]
		for (dim_combinaison in dim_combinaisons){
			# dim_combinaison <- dim_combinaisons[1]
			if (messages) print(dim_combinaison)
			tableToFill_lxp1_ext <- copy(tableToFill_lxp1[dims == dim_combinaison][order(inc)]) # peut etre mettre un "order(inc)" ici ? Bah ouais
			rows_index_decr <- nrow(tableToFill_lxp1_ext):1
			# attention cumsum bien pratique mais super risque des qu'il y aura plusieurs dimensions, a surveiller --> Simplement trier et traiter par combinaisons de dimensions
			tableToFill_lxp1_ext$Nx <- cumsum(tableToFill_lxp1_ext$Dx[rows_index_decr])[rows_index_decr] # plus efficace. juste, il faut renverser le vecteur le temps de sommer puis re renverser, mais ça va

			tableToFill_Nx <- rbind(tableToFill_Nx, tableToFill_lxp1_ext, fill = T)
		}# toute cette boucle est probablement peu efficace + verifier divers problemes possibles (lignes dans le desordre par ex ? ou autres incoherences)

		tableToFill_Nx[, a_pp_x := Nx/Dx] 			# äx
		tableToFill_Nx[, ax := a_pp_x - 1] 			# ax
		tableToFill_Nx[is.na(tableToFill_Nx)] <- 0
	# remise dans une forme plus pratique / lisible / simple
		tableToFill_Nx[
			, `:=` (
				key_xp1	= NULL
				, inc_p1	= NULL
				, lxp1		= NULL
				, inc		= NULL
				, dims		= NULL
			)
		]
		# tableToFill_Nx[, key_names := NULL]
		# tableToFill_Nx[, key_names := paste(key, dims, collapse = "°°")]
		tableToFill_Nx

	return(tableToFill_Nx)
}
# # tests
# tbls <- readRDS(list.files("../pop stats/INPUTS", "tabl.*rds", full.names = T))
# library(data.table)
# test <- Complete_commut(tbls$TH)
# generate_pop(10)

# # test de rapidite de calcul
# 	tableToFill_test <- copy(tableToFill[1:10**4])
# 	log_n_times <- 3
# 	system.time(for (i in 1:10**log_n_times) tableToFill_test[age == 2])
# 	system.time(for (i in 1:10**log_n_times) tableToFill_test[2]) # ---> donc selectionner direct la ligne au lieu de filtrer est 10 fois + rapide
# 	# par contre le nombre de ligne de la table ne change pas la difference de rapidite (ni meme la rapidite tout court) ???? completement illogique.
# 	# voir si a terme on peut pas faire une mega table avec les dimensions simplement triees et parcourir toutes les lignes de la derniere a la premiere


# test modif pour commit

# # est-ce qu'un argument est bien "missing" meme si il a une valeur par defaut ?
# 	testFun <- function(a = 2, b){
# 		print(paste0("a missing : ", missing(a)))
# 		print(paste0("b missing : ", missing(b)))
# 	}
# 	testFun()
# 	# --> oui
