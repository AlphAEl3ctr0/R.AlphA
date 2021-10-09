# rm(list = ls())
#' adding commutation numbers to a mortality table
#' @description from lxTable and given act rate, adds commut columns
#' @param lxTable 2 columns : age and lx
#' @param i actualisation rate
#' @param dimNames parameters other than inc and val (such as generation, sex, age when turning dependent)
#' @param incName the x in "lx". Most of the time, age. for dependence tables, usually the nb of years since turning dependent
#' @param messages = FALSE
#' @importFrom readr parse_number
#' @importFrom stringr str_extract str_detect
#' @export
Complete_commut <- function(
	lxTable
	, i = 0 # pas de valeur par deft, pour pouvoir engueuler l'utilisateur si non rempli --> on peut quand meme le faire avec une valeur par defaut
	, dimNames	= "dim_.*"
	, incName	= "(inc_.*)|age|x"
	, valName	= "(val_.*)|lx"
	, messages	= FALSE
){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		message("! parameters manually defined inside function for tests. Do not use results !")
		library(data.table)
		dimNames	= "dim_.*"
		incName		= "(inc_.*)|age|x"
		valName		= "(val_.*)|lx"
		tbls <- readRDS(list.files("../pop stats/INPUTS", "tabl.*rds", full.names = T))
		# lxTable = tbls$TE[1:10**4] ; dimNames = NULL # TE : table de survie bien longue (10k lignes)
		# lxTable = tbls$lg_maintien_HF ; dimNames = c("x", "sexe") ; incName = "anc"
		# lxTable = tbls$BT1_prefixed
		# lxTable = tbls$bigTables$bigTable_200 ; dimNames = c("x", "sexe", "newDimTest") ; incName = "anc"
		# lxTable = tbls$woPrefixes$TH ;  # voir si on gere les tables classiques automatiquement (sans preciser qui est inc/val)
		lxTable = tbls$woPrefixes$bigTable_10 ; incName = "anc" ; dimNames = c("x", "sexe", "newDimTest")
		messages = T
		i = 0
	}
	tableToFill <- as.data.table(copy(lxTable))
	if(missing(i)) {message("i is missing : set to 0 by default"); i <- 0}
	v <- 1/(1+i)
	# identifier les colonnes de la table : inc / val / dim / autres ----
		namesTable <- data.table(names = names(tableToFill))
		# namesTable <- rbind(namesTable, data.table(names = "val_lxtestintrus"))
		# valName <- "val_lx"
		valName_patterned <- paste0("^(","(val_)*", valName,")$")
		incName_patterned <- paste0("^(","(inc_)*", incName,")$")
		dimNames_patterned <- paste0("^(","(dim_)*", dimNames,")$")
		alldims <- paste(dimNames_patterned, collapse = "|")
		namesTable[stringr::str_detect(names,valName_patterned), type := "val"]
		namesTable[stringr::str_detect(names,incName_patterned), type := "inc"]
		namesTable[stringr::str_detect(names,alldims), type := "dim"]
		# namesTable[, find_pattern := stringr::str_extract(names,"^(dim|val|inc)(?=_)")]


		# value (lx)
			valColumn <- namesTable[type == "val"]$names
			if (length(valColumn) > 1) stop("more than 1 value column found")
			if (length(valColumn) < 1) stop("no value column found")
			tableToFill[, lx := get(valColumn)]
		# increment
			incColumn <- namesTable[type == "inc"]$names
			if (length(incColumn) > 1) stop("more than 1 increment column found")
			if (length(incColumn) < 1) stop("no increment column found")
			tableToFill[, inc := readr::parse_number(as.character(get(incColumn)))]
		# dimensions
			dimColumns <- namesTable[type == "dim"]$names
			# if (missing(dimNames)) dimNames <- namesTable[find_pattern == "dim"]$names
			separator <- "_"
			if (length(dimColumns) == 0) {
				if (messages) print("no dims used, only inc and value")
				tableToFill[, dims := ""]
			} else {
				if (messages) print(paste0(c("dimNames : ", dimNames), collapse = " - "))
				tableToFill[
					, dims := do.call(paste, c(.SD, sep = separator))
					, .SDcols = dimColumns
					] # verifier avec un vecteur vide
			}


	# retraitements
		# value
			tableToFill[is.na(lx), lx := 0] # passe les NA a 0

		# key
			tableToFill[, key := do.call(paste, c(.SD, sep = separator)), .SDcols = c("dims", "inc")] # TODO: gerer le cas ou il n'y a pas de dims, juste une table normale
			tableToFill[, key_names := paste(c(dimColumns, incColumn), collapse = separator)]


	tableToFill[, inc_p1 := inc+1]
	tableToFill[, key_xp1 := do.call(paste, c(.SD, sep = separator)), .SDcols = c("dims", "inc_p1")]

	# lx+1
		tableToFill_lxp1 <- merge(
			tableToFill
			, tableToFill[ , .(key, lxp1 = lx)]
			, by.x = "key_xp1"
			, by.y = "key"
			, all.x = T
		)

	# px, qx, Dx
		tableToFill_lxp1[, px := lxp1 / lx]  			# px
		tableToFill_lxp1[, qx := 1 - px] 				# qx
		tableToFill_lxp1[, Dx := lx * v^inc]			# DX
		tableToFill_lxp1[is.na(tableToFill_lxp1)] <- 0


	# Nx
		tableToFill_lxp1[
			order(dims, -inc)
			, Nx := cumsum(Dx)
			, by = .(dims)
		]
		tableToFill_Nx <- tableToFill_lxp1[order(dims, inc)] # a remettre bien apres au niveau des noms

	# äx et ax
		tableToFill_Nx[, a_pp_x := Nx/Dx] 			# äx
		tableToFill_Nx[, ax := a_pp_x - 1] 			# ax
		tableToFill_Nx[is.na(tableToFill_Nx)] <- 0
	# remise dans une forme plus pratique / lisible / simple
		tableToFill_Nx[
			, `:=` (
				key_xp1	= NULL
				, inc_p1	= NULL
				, lxp1		= NULL
				, dims		= NULL
			)
		]
		# tableToFill_Nx[, key_names := NULL]
		# tableToFill_Nx[, key_names := paste(key, dims, collapse = "°°")]
		tableToFill_Nx

	return(tableToFill_Nx)
}
