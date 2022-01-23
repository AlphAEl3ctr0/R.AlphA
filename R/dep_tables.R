# rm(list = ls())
#' Long-term care EPV
#' @description compute Expected Present Value of long-term care annuities, in
#'  order to help for PRC (risk-increase provision ?)
#' @param inputs 3 tables : incidence rate / mortality / dependents mortality
#' @param i actualisation rate --> not yet
#' @param detailedRes if FALSE : only returns the table of EPV by age
#' @param dim_age_dep_name name of the column giving age when turning dependent
#' @param timer_messages Should we call messages from the timer function ?
#' @param merge_messages Should we print tables sizes ?
#' @param maxRows set a limit for the number of lines to avoid crashes
#' @param dftResRate if no t_res provided, which res rate should be used (cst)
#' @param filterAgeSous should the function filter on (age_vis - anc_ct)
#' @param ageSousMin if filterAgeSous, which age (age_vis - anc_ct) is the minimum
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
	, merge_messages = F
	, maxRows = 1e7
	, dftResRate = 0
	, filterAgeSous = T
	, ageSousMin = 0
){
################################################################################
################################################################################

# 0 - init ---------------------------------------------------------------------
{
	wk <- list()
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		message("! parameters manually defined inside function for tests. Do not use results !")
		# wk <- list()
		require(data.table)
		require(R.AlphA)
		require(stringr)
		require(ggplot2)
		detailedRes = T
		timer_messages = T
		merge_messages = T
		maxRows = 1e7
		dftResRate = 0
		filterAgeSous = T
		ageSousMin = 0
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
		i <- 0
		# inputs : t_vie / t_inc / lg_maintien
		inputs <- list()
		tbls <- readRDS(file.path(inputPath,"tables.rds"))
		inputs$lg_maintien <- tbls$lg_maintien; dim_age_dep_name <- "dim_x" # version longue
		inputs$lg_maintien <- tbls$bigTables$bigTable_1000; dim_age_dep_name <- "dim_x" # version longue et large
		inputs <- tbls$STPrefix   ; dim_age_dep_name = "dim_age_dep"   # tables d'input : smallTables, avec prefixes
		{
			inputs <- list()
			tbls <- readRDS(file.path(inputPath,"tables.rds"))
			inputs$t_vie <- tbls$qalydays$lg_survie_hors_dep
			inputs$t_inc <- tbls$qalydays$incidence
			inputs$lg_maintien <- tbls$qalydays$lg_qx_mens_dep ; dim_age_dep_name = "dim_age_dep"
		} # tables d'input : qalydays
		inputs <- tbls$STMult_start50 ; dim_age_dep_name = "dim_age_dep" # tables d'input : ST depart 50 pr lg_maint
		inputs <- tbls$smallTables; dim_age_dep_name = "dim_age_dep"   # tables d'input : smallTables
		# names(tbls$bigTables$bigTable_1)
		inputs <- tbls$simpleTables; dim_age_dep_name <- "dim_x"
		inputs <- tbls$classic; dim_age_dep_name <- "dim_age_dep"
		inputs <- tbls$STMRes;dim_age_dep_name = "dim_age_dep"     # tables d'input : smallTables - multivars
	}
	wk$timer <- timer(start = T, message = timer_messages)
	wk$timer <- timer(wk$timer, step = "startfun", message = timer_messages)
} # manualrun + init wk and timer

# 1 - inputs / ajout commuts ---------------------------------------------------
wk$inputs <- inputs
{
	if(is.null(wk$inputs$t_res)){
		print("no t_res")
		nRowsDftTable <- 100
		t_res <- data.table(
			inc_anc_ct = 1:nRowsDftTable
			, res_rates = seq(
				from = dftResRate, to = dftResRate, length.out = nRowsDftTable
			)
		)
		t_res$lx <- qx_to_lx(t_res, incName = "inc_anc_ct", qxName = "res_rates")
		wk$inputs$t_res <- t_res[, .(inc_anc_ct, lx)]
	}
} # default t_res
{
	if (missing(i)) {
		message("actualisation rate (i) not provided : set to 0 by default")
	} else if (i>0) {
		warning("you provided i : it is not correctly supported yet ",
				"so do not use results.")
	}
} # warning about i
{
	wk$timer <- timer(wk$timer, step = "start adding commuts", message = timer_messages)
	t_vie_commut <- Complete_commut(wk$inputs$t_vie, incName = "age_vis|inc_.*|age", i = i)
	t_inc_commut <- Complete_commut(wk$inputs$t_inc, incName = "age_vis|inc_.*|age", i = i)
	t_res_commut <- Complete_commut(wk$inputs$t_res, incName = "age_vis|inc_.*|age", i = i)
	lg_maintien_commuts <- Complete_commut(wk$inputs$lg_maintien, i = i)
	wk$timer <- timer(wk$timer, step = "commuts OK", message = timer_messages)
} # ajout nbs de commut sur toutes les tables

# 2 - t_pres : lx incluant les P de décès, d'entrée en dep et de résil ---------
{
	m_vie_inc <- mergeLifeTables(
		t_vie_commut, xName = "vie"
		, t_inc_commut, yName = "inc"
		, maxRows = maxRows
		, message = merge_messages
	)
	wk$timer <- timer(wk$timer, step = "m_vie_inc OK", message = timer_messages)
} # _2.1 - m_vie_inc
{
	# ajout des resiliations
	# longueur : non geree, on fait toujours un produit cartesien
	m_VIR <- mergeLifeTables(
		# m_vie_inc[ , .(inc_age = age, qx = qx_vie + qx_inc)]
		m_vie_inc
		, Complete_commut(wk$inputs$t_res)[, qx_res := qx]
		, valPatt = "^qx_"
		# , yName = "res"
		, maxRows = maxRows
		, message = merge_messages
	)

	wk$timer <- timer(wk$timer, step = "m_VIR OK", message = timer_messages)
} # _2.2 - m_vie_inc_resils
{
	age_vis_name <- grep("inc_", names(wk$inputs$t_vie), value = TRUE)
	if (filterAgeSous) {
		anc_ct_name <- grep("inc_", names(wk$inputs$t_res), value = TRUE)
		m_VIR <- m_VIR[(get(age_vis_name) - get(anc_ct_name)) >= ageSousMin]
	}
	# merge_pres_tax[dim_age_dep>=age_vis] # plus tard : filtrer pour efficacité

	m_pres <- cbind(m_VIR, groupIncsV2(m_VIR))
	m_pres[, qx := qx_vie+ qx_inc + qx_res]
	m_pres$dims <- dimsCol(m_pres)
	m_pres$lx <- qx_to_lx(m_pres, incName = age_vis_name, dimsNames = "dims")
	if (nrow(m_pres[is.na(lx)])) warning(
		"some lines in m_pres don't have an lx : expect bugs"
	)  # devrait etre vide
	dimDifNm <- grep("dim_dif_", names(m_pres), value = TRUE)

	wk$timer <- timer(wk$timer, step = "m_pres OK", message = timer_messages)
	wk$interm$t_pres <- m_pres
} # _2.3 - t_pres


# 3 - lg_rente_dep -------------------------------------------------------------
# Jointure t_pres et lg_maintien, afin d'afficher pour chaque age vu
# aujourd'hui, ts les ages possibles d'entree en dep, + ax correspondants
{
	t_pres_commut <- Complete_commut(wk$interm$t_pres, incName = age_vis_name, i = i)
	wk$timer <- timer(wk$timer, step = "t_pres_commut OK", message = timer_messages)
} # add commuts to t_pres
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
} # on verifie si dim_age_dep est renseigne ou non dans lg_maintien
{
	# pdt cartesien
	# pour chaque age_vis : liste de tous les age_dep possibles et de l'ax correspondant
	# d'abord : ax pour chaque age_dep. On va considerer que dim_age_dep
	# est obligatoire pour l'instant
	dimsList_maintien <- compareVars(wk$interm$t_pres, lg_maintien_commuts, "dim_")
	wk$interm$t_ax <- lg_maintien_commuts[
		readr::parse_number(as.character(inc))==0
		, c(.SD, .(a_pp_x_dep = a_pp_x))
		, .SDcols = c(dimsList_maintien$yVars)
		]
} # t_ax : ax pour chaque age_dep
{
	wk$timer <- timer(wk$timer, step = "starting merge pres t_ax", message = timer_messages)
	merge_pres_tax <- mergeLifeTables(
		wk$interm$t_pres[, lx_age_vis := lx]
		, wk$interm$t_ax
		, valPatt = "a_pp_x|^lx_"
		, maxRows = maxRows
		, message = merge_messages
	)
	wk$timer <- timer(wk$timer, step = "merge_pres_tax OK", message = timer_messages)
} # merge t_pres and t_ax
{
	# par quelles variables on peut merge ici ? repertorier les variables qui auront tjrs le meme nom, et celles qu'il faut generaliser
	# du coup dim_age_dep, age_vis sont a peu pres obligatoires
	dimList_merge <- compareVars(merge_pres_tax, wk$interm$t_pres, "dim_")
	# on re-merge pour avoir le lx de l'age_dep et non de l'age vis (+ tard : Dx)
	# pour t_pres il faut :
		# colonne d'info : lx_pres
		# et la cle : age_vis (joint avec age_dep), + dims
		# age_vis sera utilisé comme "age_dep" lors de la jointure avec
		# m_pres_tax, afin d'obtenir le lx de chaque age_dep et donc la
		# proba d'etre toujours present a l'arrivee a chaque age dep
	t_pres_select <- wk$interm$t_pres[
		, c(.SD, .(lx_pres = lx, age_vis = get(age_vis_name)))
		, .SDcols = dimList_merge$yVars
		]#[order(get(dimDifNm), age_vis)] # inutile de trier normalement ?
} # t_pres_select : selection / renommage colonnes
{
	wk$timer <- timer(wk$timer, step = "starting m_pres_maint_lx_age_dep", message = timer_messages)
	# on a deja merge_pres_tax donnant le lx aujourd'hui
	# on ajoute la colonne lx_pres de la table t_pres_select pour avoir
	# le lx a chaque age d'entrée en dep potentiel --> proba d'etre tjrs
	# present a chacun de ces ages
	merge_pres_tax_lx_age_dep <- merge(
		x = merge_pres_tax
		, y = t_pres_select
		, by.x = c("dim_age_dep", dimList_merge$common)
		, by.y = c("age_vis"    , dimList_merge$common)
		# , all = T # reste a voir si all doit etre T ou F
	)
	wk$timer <- timer(wk$timer, step = "merge_pres_tax_lx_age_dep OK", message = timer_messages)
	if(merge_messages) {message(
		"rows m_pres_tax_lx_age_dep : ",sepThsd(nrow(merge_pres_tax_lx_age_dep))
	)}
} # merge : m_pres_tax and t_pres_select
{
	# merge_pres_tax_lx_age_dep$p_dep <- getDepProb$p_dep # bonne pratique mais pr l'instant ne fonctionne pas a cause de l'ordre de la table
	compare_dims_merge_inc <- compareVars(
		merge_pres_tax_lx_age_dep
		, t_inc_commut
		, "dim_"
	)
	merge_pres_tax_p_dep <- merge(
		merge_pres_tax_lx_age_dep
		, t_inc_commut[
			, c(.SD, .(dim_age_dep = inc, p_dep = qx))
			, .SDcols = compare_dims_merge_inc$yVars
			]
		, by = c("dim_age_dep", compare_dims_merge_inc$common)
	)
	merge_pres_tax_p_dep[, p_survie := lx_pres / lx_age_vis]
	merge_pres_tax_p_dep[, VAP_dep := p_dep * a_pp_x_dep * p_survie]
	if(merge_messages) {message(
		"rows merge_pres_tax_p_dep : ",sepThsd(nrow(merge_pres_tax_p_dep))
	)}
	wk$results$lg_rente_dep <- merge_pres_tax_p_dep[dim_age_dep>=get(age_vis_name)]
	if(merge_messages) {message(
		"rows lg_rente_dep : ",sepThsd(nrow(wk$results$lg_rente_dep))
	)}
	# TODO: gerer aussi la longueur
	wk$timer <- timer(wk$timer, step = "m_pres_tax_p_dep OK", message = timer_messages)
} # on rajoute la proba de tomber en dep a chaque age_vis


# 4 - t_VAP_gie_dep ------------------------------------------------------------
{
	dimCols <- grep("^dim_",names(wk$results$lg_rente_dep), value = T)
	res_dimsButAge <- setdiff(dimCols, "dim_age_dep")
	res_incNames <- grep("^inc_", names(wk$results$lg_rente_dep), value = T)
	wk$results$t_VAP_gie_dep <- wk$results$lg_rente_dep[
		, .(
			VAP_garantie_dep = sum(VAP_dep, na.rm = TRUE)
			, nb_lines = .N
		)
		, by = c(res_incNames, res_dimsButAge) # "inc_age_vis" a generaliser --> res_incNames ? Vérifier si c'est bon
		]
	if(merge_messages) {message(
		"rows t_VAP_gie_dep : ",sepThsd(nrow(wk$results$t_VAP_gie_dep))
	)}
} # t_VAP_gie_dep

# 5 - plots --------------------------------------------------------------------
{
	wk$plots <- list()
	p_PRC <- plotPRCRes(wk$results$t_VAP_gie_dep, var = age_vis_name)
	wk$plots$PRC <- p_PRC
	wk$timer <- timer(wk$timer, step = "all OK", message = timer_messages)
	p_timer <- ggplot(
		wk$timer
		, aes(x = reorder(step, - wk$timer$heure_seconds), y = dt_seconds)
	) +
		geom_col() +
		coord_flip()
	wk$plots$timer <- p_timer
}

# tests ------------------------------------------------------------------------
{
	# wk$results$t_VAP_gie_dep[
	# 	, .(nb_lines = .N, maxAgeSous = max(dim_dif_age_vis__anc_ct))
	# 	, by =  .(
	# 		inc_age_vis
	# 		, dim_sexe
	# 		, VAP_garantie_dep
	# 		# , dim_dif_age_vis__anc_ct
	# 	)
	# 	][nb_lines > 1]
	# wk$results$lg_rente_dep[
	# # wk$results$t_VAP_gie_dep[
	# 	TRUE
	# 	& dim_sexe == "H"
	# 	& inc_anc_ct == 47
	# 	& inc_age_vis == 30
	# 	# & inc_grp_age_vis__anc_ct == 7
	# 	] # reste a corriger : il ne faut pas supprimer les differentes inc
}


if (detailedRes) return(wk) else return(wk$results$t_VAP_gie_dep)
################################################################################
################################################################################
} # fin fonction

#TODO :
# dimensions : vraiment ok ? a verifier
# gerer l'exactitude : rentes en milieu d'annee ? dc en milieu d'annee ? etc
