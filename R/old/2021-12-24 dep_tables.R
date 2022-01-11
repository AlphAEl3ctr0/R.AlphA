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
		i <- 0
		# inputs : t_vie / t_inc / lg_maintien
		inputs <- list()
		tbls <- readRDS(file.path(inputPath,"tables.rds"))
		inputs <- tbls$STMultiVar # tables d'input : smallTables - multivars
		inputs$lg_maintien <- tbls$lg_maintien; dim_age_dep_name <- "dim_x" # version longue
		inputs$lg_maintien <- tbls$bigTables$bigTable_1000; dim_age_dep_name <- "dim_x" # version longue et large
		{
			inputs <- list()
			tbls <- readRDS(file.path(inputPath,"tables.rds"))
			inputs$t_vie <- tbls$qalydays$lg_survie_hors_dep
			inputs$t_inc <- tbls$qalydays$incidence
			inputs$lg_maintien <- tbls$qalydays$lg_qx_mens_dep ; dim_age_dep_name = "dim_age_dep"
} # tables d'input : qalydays
		inputs <- tbls$STMult_start50 ; dim_age_dep_name = "dim_age_dep" # tables d'input : ST depart 50 pr lg_maint
		inputs <- tbls$smallTables; dim_age_dep_name = "dim_age_dep"   # tables d'input : smallTables
		inputs <- tbls$STPrefix   ; dim_age_dep_name = "dim_age_dep"   # tables d'input : smallTables, avec prefixes
		# names(tbls$bigTables$bigTable_1)
	};{
	}
	wk$timer <- timer(start = T, message = timer_messages)
	wk$timer <- timer(wk$timer, step = "startfun", message = timer_messages)
	# 1 - inputs / ajout commuts ---------------------------------
	wk$inputs <- inputs
	if(is.null(wk$inputs$t_res)){
		print("no t_res")
		nRowsDftTable <- 100
		t_res <- data.table(
			inc_anc = 1:nRowsDftTable
			, res_rates = seq(from = 0.12, to = 0.03, length.out = nRowsDftTable)
		)
		t_res$lx <- qx_to_lx(t_res, incName = "inc_anc", qxName = "res_rates")
		wk$inputs$t_res <- t_res[, .(inc_anc, lx)]
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
		t_vie_commut <- Complete_commut(wk$inputs$t_vie, incName = "age_vis|inc_.*|age", i = i)
		t_inc_commut <- Complete_commut(wk$inputs$t_inc, incName = "age_vis|inc_.*|age", i = i)
		t_res_commut <- Complete_commut(wk$inputs$t_res, incName = "age_vis|inc_.*|age", i = i)
		lg_maintien_commuts <- Complete_commut(wk$inputs$lg_maintien, i = i)
	} ;{
		wk$timer <- timer(wk$timer, step = "commuts OK", message = timer_messages)
	}

	# 2 - t_pres : lx incluant les P de décès, d'entrée en dep et de résil ----
	{
		# Il faut fusionner 2 tables avec largeur ET longueur differentes
		{
			# TODO: ajouter un avertissement si on supprime des lignes
			commonInc <- intersect(t_vie_commut$inc, t_inc_commut$inc)
			t_vie_commut_filter <- t_vie_commut[inc %in% commonInc]
			t_inc_commut_filter <- t_inc_commut[inc %in% commonInc]
} ; {
			wk$timer <- timer(wk$timer, step = "length OK", message = timer_messages)
		}
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
			# debug(mergeLifeTables)
			m_vie_inc_vfun <- mergeLifeTables(
				t_vie_commut
				, t_inc_commut
			)
			Complete_commut(wk$inputs$t_inc)
			wk$timer <- timer(wk$timer, step = "m_vie_inc OK", message = timer_messages)
} # fin 2.1
		# _2.2 - m_vie_inc_resils ----
		{
			# ajout des resiliations
			# TODO : verifier que l'on prend bien en compte une chronique de taux de resils et non juste un seul taux
			# longueur : non geree, on fait toujours un produit cartesien
			setnames(t_res_commut, "inc_anc", "anc") # anc est une inc dans la table de resil, mais une dim dans la table mergee
			dimsVIR <- compareVars(m_vie_inc, t_res_commut, "dim")
			qxVIR <- compareVars(m_vie_inc, t_res_commut, "qx") # all qx vars
			m_VIR <- merge(
				by = c("cst", dimsVIR$common)
				, m_vie_inc[, c(.SD, .(cst = T)), .SDcols = c(dimsVIR$xVars, qxVIR$xVars, "age")]
				, t_res_commut[, c(.SD, .(cst = T, qx_res = qx)), .SDcols = c("anc",dimsVIR$yVars)]
				, allow.cartesian = T
			)
			m_VIR[, dim_age_sous := age - anc]
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
				, incName = "anc"
				, qxName = "qx_pres"
			)
} # somme des qx puis reconstitution lx

		wk$timer <- timer(wk$timer, step = "m_pres OK", message = timer_messages)
		wk$interm$t_pres <- m_pres[, .(lx = lx_pres, dim_age_sous), by = c("anc", "age",dimsVIR$all)] # attention il reste des bugs sur les dernieres lignes de la table
} # fin 2 - t_pres
	# Jointure t_pres et lg_maintien, afin d'afficher pour chaque age vu
	# aujourd'hui, ts les ages possibles d'entree en dep, + ax correspondants
	# 3 - lg_rente_dep ----
	lg_maintien_commuts[1:6]
	t_pres_commut <- Complete_commut(wk$interm$t_pres, incName = "age", i = i)
	wk$timer <- timer(wk$timer, step = "t_pres_commut OK", message = timer_messages)
	{
		table(lg_maintien_commuts$inc)
		wk$timer <- timer(wk$timer, step = "table lg maintien OK (a virer mais juste pour voir)", message = timer_messages)


		# on verifie si dim_age_dep est renseigne ou non
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
		dimsList_maintien <- compareVars(wk$interm$t_pres, lg_maintien_commuts, "dim_|^inc$")
		dimsList_maintien <- compareVars(wk$interm$t_pres, lg_maintien_commuts, "dim_")

		# d'abord : ax pour chaque age_dep. On va considerer que dim_age_dep
		# est obligatoire pour l'instant
		wk$interm$t_ax <- lg_maintien_commuts[
			readr::parse_number(as.character(inc))==0
			, c(.SD, .(cst = T))
			, .SDcols = c(dimsList_maintien$yVars, "a_pp_x")
			]


		wk$timer <- timer(wk$timer, step = "starting merge pres maintien", message = timer_messages)
		merge_pres_maintien <- merge(
			wk$interm$t_pres[
				, c(.SD, .(cst = T, age_vis = age, lx_age_vis = lx))
				, .SDcols = c(dimsList_maintien$xVars)
				]
			, wk$interm$t_ax
			, by = c("cst", setdiff(dimsList_maintien$common, "inc"))
			, all=F
			, allow.cartesian = T
		)[order(age_vis, dim_age_dep)][, cst := NULL] # TODO: dim_age_dep a generaliser
		wk$timer <- timer(wk$timer, step = "merge pres maintien OK", message = timer_messages)

		dimsList_maintien$all
		# par quelles variables on peut merge ici ? repertorier les variables qui auront tjrs le meme nom, et celles qu'il faut generaliser
		# du coup dim_age_dep, age_vis sont a peu pres obligatoires
		dimList_merge <- compareVars(merge_pres_maintien, wk$interm$t_pres, "dim_")
		# merge_pres_maintien[dim_age_dep>=age_vis] # plus tard : filtrer pour efficacité
		dimList_merge$all
		# on re-merge pour avoir le lx de l'age_dep et non de l'age vis
		# pour t_pres il faut
		# info : lx_pres
		# cle : age_vis (avec age_dep), + dims
		dimsButAge <- setdiff(dimList_merge$xVars, "dim_age_dep")
		t_pres_select <- wk$interm$t_pres[
			, c(.SD, .(lx_pres = lx, age_vis = age))
			, .SDcols = dimList_merge$yVars
			]
		wk$timer <- timer(wk$timer, step = "starting m_pres_maint_lx_age_dep", message = timer_messages)
		merge_pres_maintien_lx_age_dep <- merge(
			x = merge_pres_maintien
			, y = t_pres_select
			, by.x = c("dim_age_dep", dimList_merge$common)
			, by.y = c("age_vis", dimList_merge$common)
		)[order(age_vis, dim_age_dep)]
		wk$timer <- timer(wk$timer, step = "m_pres_maint_lx_age_dep OK", message = timer_messages)


	# on rajoute la proba de tomber en dep a chaque age_vis
	# nouvelle version : en cours
		compare_dims_merge_inc <- compareVars(
			merge_pres_maintien_lx_age_dep
			, t_inc_commut
			, "dim_"
		)
		merge_pres_maintien_p_dep <- merge(
			merge_pres_maintien_lx_age_dep
			, t_inc_commut[, c(.SD, .(dim_age_dep = inc, p_dep = qx)), .SDcols = c(compare_dims_merge_inc$yVars) ]
			, by = c("dim_age_dep", compare_dims_merge_inc$common)
		)[order(age_vis, dim_age_dep)] #age dep >= age vis
		# TODO: gerer aussi la longueur
		wk$timer <- timer(wk$timer, step = "m_pres_maintien_p_dep OK", message = timer_messages)

		merge_pres_maintien_p_dep[, p_survie := lx_pres / lx_age_vis]
		merge_pres_maintien_p_dep[, VAP_dep := p_dep * a_pp_x * p_survie]
		wk$results$lg_rente_dep <- merge_pres_maintien_p_dep[dim_age_dep>=age_vis]
} # fin 3 - lg_rente_dep : tables de VAP des rentes dep

	# t_VAP_gie_dep
	dimCols <- grep("^dim_",names(wk$results$lg_rente_dep), value = T)
	res_dimsButAge <- setdiff(dimCols, "dim_age_dep")
	wk$results$t_VAP_gie_dep <- wk$results$lg_rente_dep[
		, .(VAP_garantie_dep = sum(VAP_dep))
		, by = c("age_vis", res_dimsButAge)
	]

	# plots

	wk$plots <- list()
	p_PRC <- plotPRCRes(wk$results$t_VAP_gie_dep)
	wk$plots$PRC <- p_PRC
	wk$timer <- timer(wk$timer, step = "all OK", message = timer_messages)
	p_timer <- ggplot(wk$timer, aes(x = reorder(step, - wk$timer$heure_seconds), y = dt_seconds)) +
		geom_col() +
		coord_flip()
	wk$plots$timer <- p_timer

	if (detailedRes) return(wk) else return(wk$results$t_VAP_gie_dep)

}


#TODO :
# dimensions : vraiment ok ? a verifier
# gerer l'exactitude : rentes en milieu d'annee ? dc en milieu d'annee ? etc
