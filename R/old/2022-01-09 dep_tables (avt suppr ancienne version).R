# rm(list = ls())
#' Long-term care EPV
#' @description compute Expected Present Value of long-term care annuities, in
#'  order to help for PRC (risk-increase provision ?)
#' @param inputs 3 tables : incidence rate / mortality / dependents mortality
#' @param i actualisation rate --> not yet
#' @param detailedRes if FALSE : only returns the table of EPV by age
#' @param dim_age_dep_name name of the column giving age when turning dependent
#' @param timer_messages Should we call messages from the timer function ?
#' @importFrom readr parse_number
#' @importFrom stringr str_extract str_detect
#' @importFrom data.table copy
#' @importFrom stats median reorder
#' @import ggplot2
#' @export
dep_table <- function(
	# encore beaucoup d'elements a corriger suite prise en compte des resils
	inputs # objet contenant les 3 tables d'inputs
	, detailedRes = F
	, dim_age_dep_name = "dim_age_dep"
	, i = 0
	, timer_messages = F
){
	wk <- list()
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		message("! parameters manually defined inside function for tests. Do not use results !")
		# wk <- list()
		# library(data.table)
		# library(R.AlphA)
		# library(stringr)
		# library(ggplot2)
		detailedRes = T
		timer_messages = T
		{
			library(R.AlphA)
			root <- dirname(rstudioapi::getSourceEditorContext()$path)
			workRRoot <- stringr::str_extract(root, ".*WorkR")
			setwd(root)
			# lum_0_100(20)
			# dteVision_inc <- date(ISOdate(2010,01,01)) # date de vision
			# annees <- 60 # nombre d'années sur lesquelles projeter
			inputPath <- file.path(workRRoot, "pop stats", "INPUTS") #chemin dossier inputs
			# popFilesPath <- file.path(root, "fichiers_pop")
} # paramètres (root)
		source(list.files("../dev", "groupIncs", full.names = TRUE))
		source(list.files("../dev", "merge", full.names = TRUE))
		i <- 0
		# inputs : t_vie / t_inc / lg_maintien
		inputs <- list()
		tbls <- readRDS(file.path(inputPath,"tables.rds"))
		inputs$lg_maintien <- tbls$lg_maintien; dim_age_dep_name <- "dim_x" # version longue
		inputs$lg_maintien <- tbls$bigTables$bigTable_1000; dim_age_dep_name <- "dim_x" # version longue et large
		inputs <- tbls$STMult_start50 ; dim_age_dep_name = "dim_age_dep" # tables d'input : ST depart 50 pr lg_maint
		inputs <- tbls$smallTables; dim_age_dep_name = "dim_age_dep"   # tables d'input : smallTables
		inputs <- tbls$STPrefix   ; dim_age_dep_name = "dim_age_dep"   # tables d'input : smallTables, avec prefixes
		# names(tbls$bigTables$bigTable_1)
		{
			inputs <- list()
			tbls <- readRDS(file.path(inputPath,"tables.rds"))
			inputs$t_vie <- tbls$qalydays$lg_survie_hors_dep
			inputs$t_inc <- tbls$qalydays$incidence
			inputs$lg_maintien <- tbls$qalydays$lg_qx_mens_dep ; dim_age_dep_name = "dim_age_dep"
} # tables d'input : qalydays
		inputs <- tbls$STMultiVar;dim_age_dep_name = "dim_age_dep"     # tables d'input : smallTables - multivars
		inputs <- tbls$simpleTables; dim_age_dep_name <- "dim_x"
	};{
	}
	wk$timer <- timer(start = T, message = timer_messages)
	wk$timer <- timer(wk$timer, step = "startfun", message = timer_messages)
	# 1 - inputs / ajout commuts ---------------------------------
	wk$inputs <- inputs
	if(is.null(wk$inputs$t_res)){
		print("no t_res")
		nRowsDftTable <- 50
		t_res <- data.table(
			inc_anc_ct = 1:nRowsDftTable
			, res_rates = seq(from = 0, to = 0.0, length.out = nRowsDftTable)
			# , res_rates = seq(from = 0.12, to = 0.03, length.out = nRowsDftTable)
		)
		t_res$lx <- qx_to_lx(t_res, incName = "inc_anc_ct", qxName = "res_rates")
		wk$inputs$t_res <- t_res[, .(inc_anc_ct, lx)]
	}

	if (missing(i)) {
		message("actualisation rate (i) not provided : set to 0 by default")
	} else if (i>0) {
		warning("you provided i : it is not correctly supported yet ",
				"so do not use results.")
	};{ # warning about i
	}

	# ajout nbs de commut sur toutes les tables
	{
		wk$timer <- timer(wk$timer, step = "start adding commuts", message = timer_messages)
		# debug(Complete_commut)
		t_vie_commut <- Complete_commut(wk$inputs$t_vie, incName = "age_vis|inc_.*|age", i = i)
		t_inc_commut <- Complete_commut(wk$inputs$t_inc, incName = "age_vis|inc_.*|age", i = i)
		t_res_commut <- Complete_commut(wk$inputs$t_res, incName = "age_vis|inc_.*|age", i = i)
		t_res_commut[1:5]
		lg_maintien_commuts <- Complete_commut(wk$inputs$lg_maintien, i = i)
	} ;{
		wk$timer <- timer(wk$timer, step = "commuts OK", message = timer_messages)
	}

	# 2 - t_pres : lx incluant les P de décès, d'entrée en dep et de résil ----
	{
		{
					# Il faut fusionner 2 tables avec largeur ET longueur differentes
					{
						# TODO: ajouter un avertissement si on supprime des lignes
						commonInc <- intersect(t_vie_commut$inc, t_inc_commut$inc)
						t_vie_commut_filter <- t_vie_commut[inc %in% commonInc]
						t_inc_commut_filter <- t_inc_commut[inc %in% commonInc]
			} ; {
						wk$timer <- timer(wk$timer, step = "length OK", message = timer_messages)
					} # etape "length" a virer ? Apres passage a la fonction mergeLifeTables
					dims_vie_inc <- compareVars(t_vie_commut_filter, t_inc_commut_filter, "dim_") # pattern inc viree pour l'ajouter manuellement
					# verif : les dimensions en commun ont elles les memes valeurs ?
					for (testVar in dims_vie_inc$common){
						# testVar <- "dim_sexe"
						xVals <- unique(t_vie_commut_filter[, get(testVar)])
						yVals <- unique(t_inc_commut_filter[, get(testVar)])
						if(length(setdiff(xVals, yVals))) {
							message("for column : ", testVar)
							message("some values are not in common ")
							message("vals t_vie : ", paste(xVals, collapse = ","))
							message("vals t_inc : ", paste(yVals, collapse = ","))
							message(
								"values not in common : "
								, paste(
									collapse = ","
									, setdiff(xVals, yVals)
									, setdiff(yVals, xVals)
								) # attention bug, a revoir
							)
						}
			}
					# verif : meme largeur ?
					if (length(dims_vie_inc$exclusive)>0){
						warning(call. = T
								, "incidence and mortality table have different sets of dimension"
								, "\n exclusive cols : "
								, paste(dims_vie_inc$exclusive, collapse = ","))
					}


					# merge sur base des dimensions en commun (+ inc)
					# On garde quand meme toutes les dimensions (yc les exclusives)
					# cas ou chaque table a des dimensions exlusives : non etudie

					# _2.1 - m_vie_inc ----
					{
						wk$timer <- timer(wk$timer, step = "start merging vie_inc", message = timer_messages)
						m_vie_inc <- merge(
							all=F
							, x = t_vie_commut_filter[, c(.SD, .(qx_vie = qx, age = inc)), .SDcols = c(dims_vie_inc$xVars)]
							, y = t_inc_commut_filter[, c(.SD, .(qx_inc = qx, age = inc)), .SDcols = c(dims_vie_inc$yVars)]
							, by = c("age",dims_vie_inc$common)
						)
			} # fin 2.1
						# debug(mergeLifeTables)

} # 2.1 prec --> a virer
		# _2.1 bis - m_vie_inc_vfun ----
			m_vie_inc_vfun <- mergeLifeTables(
				t_vie_commut, xName = "vie"
				, t_inc_commut, yName = "inc"
			)
			wk$timer <- timer(wk$timer, step = "m_vie_inc OK", message = timer_messages)
			# m_vie_inc <- copy(m_vie_inc_vfun) # temporairement pour checks
		# _2.2 - m_vie_inc_resils ----
		{
			# ajout des resiliations
			# longueur : non geree, on fait toujours un produit cartesien
			setnames(t_res_commut, "inc_anc_ct", "anc_ct") # anc_ct est une inc dans la table de resil, mais une dim dans la table mergee
			dimsVIR <- compareVars(m_vie_inc, t_res_commut, "dim")
			qxVIR <- compareVars(m_vie_inc, t_res_commut, "qx") # all qx vars
			m_VIR <- merge(
				by = c("cst", dimsVIR$common)
				, m_vie_inc[, c(.SD, .(cst = T)), .SDcols = c(dimsVIR$xVars, qxVIR$xVars, "age")]
				, t_res_commut[, c(.SD, .(cst = T, qx_res = qx)), .SDcols = c("anc_ct",dimsVIR$yVars)]
				, allow.cartesian = T
			)
			m_VIR[, dim_age_sous := age - anc_ct]

			m_VIR_vfun <- mergeLifeTables(
				# m_vie_inc[ , .(inc_age = age, qx = qx_vie + qx_inc)]
				m_vie_inc_vfun
				, Complete_commut(wk$inputs$t_res)[, qx_res := qx]
				, valPatt = "^qx_"
				# , yName = "res"
			)
			m_pres_vfun <- groupIncs(m_VIR_vfun)
			m_pres_vfun[, qx := qx_vie+ qx_inc + qx_res]
			incgrp <- grep("inc_grp_", names(m_pres_vfun), value = TRUE)
			m_pres_vfun$lx <- qx_to_lx(m_pres_vfun, incName = incgrp, dimsNames = "dims")
			m_pres_vfun[is.na(lx)] # devrait etre vide

			wk$timer <- timer(wk$timer, step = "m_VIR OK", message = timer_messages)
} # fin 2.2
		m_pres <- copy(m_VIR) # vu la taille, p-e pas une bonne idee
		# _2.3 - calcul somme des qx ----
		{
			# bug si on a des dimensions : a gerer
			# Finalement --> a gerer si on n'a pas de dimensions du coup
			# somme des qx puis calcul du lx correspondant
			# somme...
			qxNames <- grep("^qx_", names(m_pres), value = T)
			m_pres[, qx_pres := rowSums(.SD, na.rm = T), .SDcols = qxNames ]

			# ...lx
			m_pres$lx_pres <- qx_to_lx(
				m_pres
				, dimsNames = c(dimsVIR$all, "dim_age_sous") # il reste un gros travail de generalisation a faire
				, incName = "anc_ct"
				, qxName = "qx_pres"
			)

			dimDifNm <- grep("dim_dif_", names(m_pres_vfun), value = TRUE)
			incGrpNm <- grep("inc_grp_", names(m_pres_vfun), value = TRUE)
			testCompareLx <- data.table(
				lx_m_pres = m_pres[order(dim_age_sous, age)]$lx_pres
				, lx_v_fun = m_pres_vfun[order(get(dimDifNm), get(incGrpNm))]$lx
			)
			testCompareLx[, diff := lx_m_pres - lx_v_fun]
			table(testCompareLx$diff, useNA = "ifany") # mergeLifeTables corrige 99 lignes de NA en plus de gagner en generalisation
} # somme des qx puis reconstitution lx (a virer ? deja fait )

		wk$timer <- timer(wk$timer, step = "m_pres OK", message = timer_messages)
		wk$interm$t_pres <- m_pres[, .(lx = lx_pres, dim_age_sous), by = c("anc_ct", "age",dimsVIR$all)] # attention il reste des bugs sur les dernieres lignes de la table
		wk$interm$t_pres_vfun <- m_pres_vfun
} # fin 2 - t_pres
	# Jointure t_pres et lg_maintien, afin d'afficher pour chaque age vu
	# aujourd'hui, ts les ages possibles d'entree en dep, + ax correspondants
	# 3 - lg_rente_dep ----
	lg_maintien_commuts[1:6]
	t_pres_commut <-      Complete_commut(wk$interm$t_pres     , incName = "age"       , i = i)
	t_pres_commut_vfun <- Complete_commut(wk$interm$t_pres_vfun, incName = "inc_grp_.*", i = i)
	wk$timer <- timer(wk$timer, step = "t_pres_commut OK", message = timer_messages)
	{
		# on verifie si dim_age_dep est renseigne ou non dans lg_maintien
		{
			if (missing(dim_age_dep_name)) {
				if (!"dim_age_dep" %in% names(lg_maintien_commuts)){
					message("column dim_age_dep not in lg_maintien :")
					message("please provide the name for corresponding column")
				}
			} else {
				if (!dim_age_dep_name %in% names(lg_maintien_commuts)){
					message("column ", dim_age_dep_name, " not in lg_maintien :")
					message("please provide an existing name for the dim_age_dep column")
				} else {
					# lg_maintien_commuts[, dim_age_dep := get(dim_age_dep_name)] # nop : mieux vaut ne pas dupliquer des colonnes
					setnames(lg_maintien_commuts, dim_age_dep_name, "dim_age_dep")
					message("dim_age_dep set to ", dim_age_dep_name)
				}
			}
		}
		# pdt cartesien
		# pour chaque age_vis : liste de tous les age_dep possibles et de l'ax correspondant
		# dimsList_maintien <- compareVars(wk$interm$t_pres, lg_maintien_commuts, "dim_|^inc$")
		dimsList_maintien      <- compareVars(wk$interm$t_pres     , lg_maintien_commuts, "dim_")
		dimsList_maintien_vfun <- compareVars(wk$interm$t_pres_vfun, lg_maintien_commuts, "dim_")

		# d'abord : ax pour chaque age_dep. On va considerer que dim_age_dep
		# est obligatoire pour l'instant
		wk$interm$t_ax <- lg_maintien_commuts[
			readr::parse_number(as.character(inc))==0
			, c(.SD, .(cst = T, a_pp_x_dep = a_pp_x))
			, .SDcols = c(dimsList_maintien_vfun$yVars)
			][, cst := NULL]


		wk$timer <- timer(wk$timer, step = "starting merge pres maintien", message = timer_messages)
		merge_pres_tax <- merge(
			wk$interm$t_pres[
				, c(.SD, .(cst = T, age_vis = age, lx_age_vis = lx))
				, .SDcols = c(dimsList_maintien$xVars)
				]
			, wk$interm$t_ax[, cst := TRUE]
			, by = c("cst", setdiff(dimsList_maintien$common, "inc"))
			, all=F
			, allow.cartesian = T
		)[order(age_vis, dim_age_dep)][, cst := NULL] # TODO: dim_age_dep a generaliser

		merge_pres_tax_vfun <- mergeLifeTables(
			wk$interm$t_pres_vfun[, lx_age_vis := lx]
			, wk$interm$t_ax
			, valPatt = "a_pp_x|^lx_"
		)

		# test <- Complete_commut(merge_pres_tax, valName = "lx_.*", incName = "age_vis")
		# test_vfun <- Complete_commut(merge_pres_tax_vfun, valName = "lx_.*", incName = "inc_grp_.*")
		# merge_pres_tax     [T & age_vis     == 57 & dim_age_sous            == 56]
		# merge_pres_tax_vfun[T & inc_age     == 57 & dim_dif_age__anc_ct     == 56]
		# test               [T & age_vis     == 57 & dim_age_sous            == 56]
		# test_vfun          [T & inc_age     == 57 & dim_dif_age__anc_ct     == 56]
		age_vis_name <- grep("inc_", names(wk$inputs$t_vie), value = TRUE)
		# merge_pres_tax_vfun <- merge(
		# 	wk$interm$t_pres_vfun[
		# 		, c(.SD, .(cst = T, age_vis = age, lx_age_vis = lx))
		# 		, .SDcols = c(dimsList_maintien_vfun$xVars)
		# 		]
		# 	, wk$interm$t_ax
		# 	, by = c("cst", setdiff(dimsList_maintien$common, "inc"))
		# 	, all=F
		# 	, allow.cartesian = T
		# )[order(age_vis, dim_age_dep)][, cst := NULL] # TODO: dim_age_dep a generaliser
		wk$timer <- timer(wk$timer, step = "merge_pres_tax OK", message = timer_messages)

		dimsList_maintien$all
		# par quelles variables on peut merge ici ? repertorier les variables qui auront tjrs le meme nom, et celles qu'il faut generaliser
		# du coup dim_age_dep, age_vis sont a peu pres obligatoires
		dimList_merge <-      compareVars(merge_pres_tax     , wk$interm$t_pres     , "dim_")
		dimList_merge_vfun <- compareVars(merge_pres_tax_vfun, wk$interm$t_pres_vfun, "dim_")
		# merge_pres_tax[dim_age_dep>=age_vis] # plus tard : filtrer pour efficacité
		# on re-merge pour avoir le lx de l'age_dep et non de l'age vis (+ tard : Dx)
		# pour t_pres il faut :
			# info : lx_pres
			# et la cle : age_vis (joint avec age_dep), + dims
			# age_vis sera utilisé comme "age_dep" lors de la jointure avec
			# m_pres_tax, afin d'obtenir le lx de chaque age_dep et donc la
			# proba d'etre toujours present a l'arrivee a chaque age dep
		dimsButAge <- setdiff(dimList_merge$xVars, "dim_age_dep")
		t_pres_select <- wk$interm$t_pres[
			, c(.SD, .(lx_pres = lx, age_vis = age))
			, .SDcols = dimList_merge$yVars
			][order(dim_age_sous, age_vis)]
		t_pres_select_vfun <- wk$interm$t_pres_vfun[
			, c(.SD, .(lx_pres = lx, age_vis = get(age_vis_name)))
			, .SDcols = dimList_merge_vfun$yVars
			][order(get(dimDifNm), age_vis)]
		wk$timer <- timer(wk$timer, step = "starting m_pres_maint_lx_age_dep", message = timer_messages)


		# on a deja merge_pres_tax donnant le lx aujourd'hui
		# on ajoute la colonne lx_pres de la table t_pres_select pour avoir
		# le lx a chaque age d'entrée en dep potentiel --> proba d'etre tjrs
		# present a chacun de ces ages
		unique(merge_pres_tax$dim_age_dep)
		unique(t_pres_select$age_vis)

		merge_pres_tax_lx_age_dep <- merge(
			x = merge_pres_tax
			, y = t_pres_select
			, by.x = c("dim_age_dep", dimList_merge$common)
			, by.y = c("age_vis"    , dimList_merge$common)
			# , all = T
		)[order(age_vis, dim_age_dep)]
		merge_pres_tax_lx_age_dep_vfun <- merge(
			x = merge_pres_tax_vfun#[, xTest := 1]
			, y = t_pres_select_vfun#[, yTest := 1]
			, by.x = c("dim_age_dep", dimList_merge_vfun$common)
			, by.y = c("age_vis"    , dimList_merge_vfun$common)
			# , all = T # reste a voir si all doit etre T ou F
		)
		{
			# unique(t_pres_select_vfun$dim_dif_age_vis__anc_ct)
			# unique(t_pres_select_vfun$age_vis)
			# unique(merge_pres_tax_vfun$dim_age_dep)
			# test_vfun <- merge_pres_tax_lx_age_dep_vfun[
			# 	TRUE
			# 	& dim_age_dep >= 29
			# 	& dim_dif_age_vis__anc_ct >= dim_age_dep
			# ]# reste a voir d'ou viennent ces NAs
			# test      <- merge_pres_tax_lx_age_dep[
			# 	TRUE
			# 	& dim_age_dep >= 29
			# 	& dim_age_sous >= dim_age_dep
			# ]
			# merge_pres_tax_lx_age_dep_vfun[is.na(yTest)] # lx_pres non trouve dans t_pres_select
			# merge_pres_tax_lx_age_dep_vfun[is.na(xTest)]
			# # explication pour les is.na(xTest) :
			# 	# certains "age_vis" de y (t_pres) ne sont pas trouves dans les
			# 	# "dim_age_dep" de x (merge_pres_tax) d'ou xtest = NA
			# t_pres_commut_vfun[is.na(lx)]
			# t_pres_select_vfun[is.na(lx_pres)]
			# merge_pres_tax_lx_age_dep_vfun[is.na(a_pp_x_dep)]
			# merge_pres_tax_lx_age_dep_vfun[is.na(inc_anc_ct)]
			# merge_pres_tax_lx_age_dep_vfun[is.na(dim_age_dep)]
			# anyNA(
			# 	merge_pres_tax_lx_age_dep_vfun[, 4]
			# )
			# unique(merge_pres_tax_vfun$dim_age_dep)
			# unique(t_pres_select_vfun$age_vis)
		} # recherches sur les NAs avec all = T
		wk$timer <- timer(wk$timer, step = "merge_pres_tax_lx_age_dep OK", message = timer_messages)


	# on rajoute la proba de tomber en dep a chaque age_vis
	# nouvelle version : en cours
		compare_dims_merge_inc <- compareVars(
			merge_pres_tax_lx_age_dep
			, t_inc_commut
			, "dim_"
		)
		merge_pres_tax_p_dep <- merge(
			merge_pres_tax_lx_age_dep
			, t_inc_commut[, c(.SD, .(dim_age_dep = inc, p_dep = qx)), .SDcols = c(compare_dims_merge_inc$yVars) ]
			, by = c("dim_age_dep", compare_dims_merge_inc$common)
		)[order(age_vis, dim_age_dep)] #age dep >= age vis

		# # getDepProb <- mergeLifeTables(
		# merge_pres_tax_p_dep_vfun <- mergeLifeTables(
		# 	merge_pres_tax_lx_age_dep_vfun
		# 	, t_inc_commut[, p_dep := qx]
		# 	# , valPatt = "p_dep"
		# 	, valPatt = "p_dep|lx_|a_pp_x"
		# )
		# merge_pres_tax_lx_age_dep_vfun$p_dep <- getDepProb$p_dep # bonne pratique mais pr l'instant ne fonctionne pas a cause de l'ordre de la table
		# merge_pres_tax_p_dep_vfun <- copy(merge_pres_tax_lx_age_dep_vfun)
		compare_dims_merge_inc_vfun <- compareVars(
			merge_pres_tax_lx_age_dep_vfun
			, t_inc_commut
			, "dim_"
		)
		merge_pres_tax_p_dep_vfun <- merge(
			merge_pres_tax_lx_age_dep_vfun
			, t_inc_commut[, c(.SD, .(dim_age_dep = inc, p_dep = qx)), .SDcols = compare_dims_merge_inc_vfun$yVars]
			, by = c("dim_age_dep", compare_dims_merge_inc_vfun$common)
		)

		merge_pres_tax_p_dep[, p_survie := lx_pres / lx_age_vis]
		merge_pres_tax_p_dep[, VAP_dep := p_dep * a_pp_x_dep * p_survie]
		merge_pres_tax_p_dep_vfun[, p_survie := lx_pres / lx_age_vis]
		merge_pres_tax_p_dep_vfun[, VAP_dep := p_dep * a_pp_x_dep * p_survie]
		unique(merge_pres_tax_p_dep[, .(dim_age_dep, p_dep)])
		unique(merge_pres_tax_p_dep_vfun[, .(dim_age_dep, p_dep)])
		wk$results$lg_rente_dep <- merge_pres_tax_p_dep[dim_age_dep>=age_vis]
		wk$results$lg_rente_dep_vfun <- merge_pres_tax_p_dep_vfun[dim_age_dep>=get(age_vis_name)]



		# TODO: gerer aussi la longueur
		wk$timer <- timer(wk$timer, step = "m_pres_tax_p_dep OK", message = timer_messages)
} # fin 3 - lg_rente_dep : tables de VAP des rentes dep

	# t_VAP_gie_dep
	dimCols <- grep("^dim_",names(wk$results$lg_rente_dep), value = T)
	dimCols_vfun <- grep("^dim_",names(wk$results$lg_rente_dep_vfun), value = T)
	res_dimsButAge <- setdiff(dimCols, "dim_age_dep")
	res_dimsButAge_vfun <- setdiff(dimCols_vfun, "dim_age_dep")
	res_incNames <- grep("^inc_", names(wk$results$lg_rente_dep_vfun), value = T)
	wk$results$t_VAP_gie_dep <- wk$results$lg_rente_dep[
		, .(VAP_garantie_dep = sum(VAP_dep))
		, by = c("age_vis", res_dimsButAge)
	]
	wk$results$t_VAP_gie_dep_vfun <- wk$results$lg_rente_dep_vfun[
		, .(VAP_garantie_dep = sum(VAP_dep))
		# , by = c(incGrpNm, age_vis_name, res_dimsButAge_vfun) # "inc_age_vis" a generaliser --> incGrpNm ? Vérifier si c'est bon
		, by = c(res_incNames, res_dimsButAge_vfun) # "inc_age_vis" a generaliser --> incGrpNm ? Vérifier si c'est bon
	]

	{
	# tst_age <- 60
	# tst_sex <- "M"
	# wk$results$t_VAP_gie_dep[
	# 	TRUE
	# 	# & age_vis == tst_age
	# 	& dim_sexe == tst_sex
	# 	& dim_annee_en_cours == "X2008"
	# 	& dim_age_sous == 60
	# 	& dim_type_dep == "dem"
	# ]
	# wk$results$t_VAP_gie_dep_vfun[
	# 	TRUE
	# 	# & get(incGrpNm) == 1
	# 	& dim_sexe == tst_sex
	# 	& dim_annee_en_cours == "X2008"
	# 	& dim_dif_age__anc_ct == 55
	# 	& dim_type_dep == "dem"
	# ]
	# wk$results$lg_rente_dep[
	# 	TRUE
	# 	& dim_sexe == tst_sex
	# 	& dim_annee_en_cours == "X2008"
	# 	& dim_age_sous == 60
	# 	& dim_type_dep == "dem"
	# 	]
	# wk$results$lg_rente_dep_vfun[
	# 	TRUE
	# 	& dim_sexe == tst_sex
	# 	& dim_annee_en_cours == "X2008"
	# 	& dim_dif_age__anc_ct == 55
	# 	& dim_type_dep == "dem"
	# 	]
	} # tests

	# plots

	wk$plots <- list()
	p_PRC <- plotPRCRes(wk$results$t_VAP_gie_dep)
	wk$plots$PRC <- p_PRC
	wk$timer <- timer(wk$timer, step = "all OK", message = timer_messages)
	p_timer <- ggplot(wk$timer, aes(x = reorder(step, - wk$timer$heure_seconds), y = dt_seconds)) +
		geom_col() +
		coord_flip()
	wk$plots$timer <- p_timer
	# wk$results$t_VAP_gie_dep_vfun[
	# 	, .(nb_lines = .N, maxAgeSous = max(dim_dif_age_vis__anc_ct))
	# 	, by =  .(
	# 		inc_age_vis
	# 		, dim_sexe
	# 		, VAP_garantie_dep
	# 		# , dim_dif_age_vis__anc_ct
	# 	)
	# 	][nb_lines > 1]
	# wk$results$lg_rente_dep_vfun[
	# # wk$results$t_VAP_gie_dep_vfun[
	# 	TRUE
	# 	& dim_sexe == "H"
	# 	& inc_anc_ct == 47
	# 	& inc_age_vis == 30
	# 	# & inc_grp_age_vis__anc_ct == 7
	# 	] # reste a corriger : il ne faut pas supprimer les differentes inc

	if (detailedRes) return(wk) else return(wk$results$t_VAP_gie_dep)
	# library(openxlsx)
	write.xlsx(listVarsUniques(wk$inputs), "uniqueVals.xlsx")
	wk$results
	merge_pres_tax_p_dep_vfun
	listVarsUniques(list(merge_pres_tax_p_dep_vfun, merge_pres_tax_p_dep_vfun))
	wk$interm$t_pres
	wk$results$t_VAP_gie_dep_vfun
}


#TODO :
# dimensions : vraiment ok ? a verifier
# gerer l'exactitude : rentes en milieu d'annee ? dc en milieu d'annee ? etc
