# rm(list = ls())
#' adding commutation numbers to a mortality table
#' @description from lxTable and given act rate, adds commut columns
#' @param lxTable 2 columns : age and lx
#' @param i actualisation rate
#' @param dimNames parameters other than inc and val (such as generation, sex, age when turning dependent)
#' @param incName the x in "lx". Most of the time, age. for dependence tables, usually the nb of years since turning dependent
#' @param messages = FALSE
#' @importFrom readr parse_number
#' @importFrom stringr str_extract
#' @export
Complete_commut <- function(
	lxTable
	, i # pas de valeur par deft, pour pouvoir engueuler l'utilisateur si non rempli
	, dimNames = NULL
	, incName = NULL
	, messages = FALSE
){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		message("! parameters manually defined inside function for tests. Do not use results !")
		library(data.table)
		tbls <- readRDS(list.files("../pop stats/INPUTS", "tabl.*rds", full.names = T))
		# lxTable = tbls$TE[1:10**4] ; dimNames = NULL # TE : table de survie bien longue (10k lignes)
		# lxTable = rbind(
		# 	tbls$TH[1:3][, sexe := "H"]
		# 	, tbls$TF[1:3][, sexe := "F"]
		# ) ; dimNames = c("sexe") # pour commencer a bosser sur les multi dimensions
		# dimNames = c("sexe","lx")
		# incName = "age"
		# lxTable = tbls$lg_maintien_HF ; dimNames = c("x", "sexe") ; incName = "anc"
		# lxTable = tbls$bigTables$bigTable_200 ; dimNames = c("x", "sexe", "newDimTest") ; incName = "anc"
		# key_vars = c(dimNames, incName)
		lxTable = tbls$BT1_prefixed
		messages = T
		i = 0
	}
	tableToFill <- as.data.table(copy(lxTable))
	if(missing(i)) {message("i is missing : set to 0 by default"); i <- 0}
	v <- 1/(1+i)
	# identifier les noms dans la table
		namesTable <- data.table(names = names(tableToFill))
		namesTable[, find_pattern := stringr::str_extract(names,"^(dim|val|inc)(?=_)")]

		# value (lx)
			autoVal <- namesTable[find_pattern == "val"]$names
			if(length(autoVal)) {
				if (messages) print(paste0("val column found : ", autoVal))
				valName <- namesTable[find_pattern == "val"]$names
			} else {
				if (messages) print("val column not found : default lx")
				valName <- "lx"
			}
		if (missing(incName)) incName <- namesTable[find_pattern == "inc"]$names
		if (missing(dimNames)) dimNames <- namesTable[find_pattern == "dim"]$names

	tableToFill[, lx := get(valName)]
	tableToFill[is.na(lx), lx := 0] # retraitement

	separator <- "_"
	if (is.null(dimNames)) {
		if (messages) print("no dims used, only inc and value")
		tableToFill[, dims := ""]
	} else {
		if (messages) print(paste0(c("dimNames : ", dimNames), collapse = " - "))
		tableToFill[, dims := do.call(paste, c(.SD, sep = separator)), .SDcols = dimNames] # verifier avec un vecteur vide
	}
	tableToFill[, inc := readr::parse_number(as.character(get(incName)))] # finalement c'est bien oblige de l'avoir en nombre (pour trier)
	tableToFill[, key := do.call(paste, c(.SD, sep = separator)), .SDcols = c("dims", "inc")]
	tableToFill[, key_names := paste(c(dimNames, incName), collapse = separator)]
	tableToFill[, inc_p1 := inc+1]
	tableToFill[, key_xp1 := do.call(paste, c(.SD, sep = separator)), .SDcols = c("dims", "inc_p1")]

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


	# V4 : meme pour les Nx, pas de boucle mais plutot un "by"
		tableToFill_lxp1[
			order(dims, -inc)
			, Nx := cumsum(Dx)
			, by = .(dims)
		]

		tableToFill_Nx <- tableToFill_lxp1[order(dims, inc)] # a remettre bien apres au niveau des noms
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
