# rm(list = ls())
#' Long-term care EPV
#' @description compute Expected Present Value of long-term care annuities, in
#'  order to help for PRC (risk-increase provision ?)
#' @param inputs 3 tables : incidence rate / mortality / dependents mortality
#' @param i actualisation rate --> not yet
#' @param detailedRes if FALSE : only returns the table of EPV by age
#' @param dim_age_dep_name name of the column giving age when turning dependent
#' @importFrom readr parse_number
#' @importFrom stringr str_extract str_detect
#' @importFrom data.table copy
#' @export
dep_table <- function(
	inputs # objet contenant les 3 tables d'inputs
	, detailedRes = F
	, dim_age_dep_name = "dim_age_dep"
	, i = 0
){
	wk <- list()
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		message("! parameters manually defined inside function for tests. Do not use results !")
		wk <- list()
		# library(data.table)
		# library(stringr)
		detailedRes = T
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
		# inputs : t_vie / t_inc / lg_maintien
		{
			inputs <- list()
			tbls <- readRDS(file.path(inputPath,"tables.rds"))
			inputsPRC <- tbls$qalydays
			inputs$t_vie <- inputsPRC$lg_survie_hors_dep
			inputs$t_inc <- inputsPRC$incidence
			inputs$lg_maintien <- inputsPRC$lg_qx_mens_dep
} # tables d'input : qalydays
		inputs <- list()
		tbls <- readRDS(file.path(inputPath,"tables.rds"))
		inputs <- tbls$smallTables  # tables d'input : smallTables
		inputs <- tbls$STMultiVar # tables d'input : smallTables - multivars
		inputs <- tbls$STMult_start50 # tables d'input : ST depart 50 pr lg_maint
		inputs$lg_maintien <- tbls$lg_maintien; dim_age_dep_name <- "dim_x" # version longue
		inputs$lg_maintien <- tbls$bigTables$bigTable_1000; dim_age_dep_name <- "dim_x" # version longue et large
	}

	wk$inputs <- inputs
	if (missing(i)) {
		message("actualisation rate (i) not provided : set to 0 by default")
	}

	# ajout nbs de commut
	{
		t_vie_commut <- Complete_commut(wk$inputs$t_vie, incName = "age_vis", i = i)
		t_inc_commut <- Complete_commut(wk$inputs$t_inc, incName = "age_vis", i = i)
		lg_maintien_commuts <- Complete_commut(wk$inputs$lg_maintien, i = i)
}

	# 1 - lg_maintien : deja ok (+ ce sera plutot lg_all car ttes concernees)
	# 2 - t_pres : lx incluant les P de décès et d'entrée en dep
	{
		# Il faut fusionner 2 tables avec largeur ET longueur differentes
		compare_dims <- compareVars(t_inc_commut, t_vie_commut, pattern = "^dim_")
		# verif : meme largeur ?
		if (length(compare_dims$exclusive)>0){
			message("incidence and mortality table have different sets of dimension")
		}

		# gestion de la longueur differente
		# TODO: ajouter un avertissement si on supprime des lignes
		commonInc <- intersect(t_vie_commut$inc, t_inc_commut$inc)
		t_vie_commut_filter <- t_vie_commut[inc %in% commonInc]
		t_inc_commut_filter <- t_inc_commut[inc %in% commonInc]


		# merge sur base des dimensions en commun (+ inc)
		# On garde quand meme toutes les dimensions (yc les exclusives)
		# cas ou chaque table a des dimensions exlusives : non etudie
		VarList <- compareVars(t_vie_commut_filter, t_inc_commut_filter, "dim_|^inc$") # doublon : a simplifier
		testMerge <- merge(
			  t_vie_commut_filter[, .SD, .SDcols = c(VarList$xVars, "qx")]
			, t_inc_commut_filter[, .SD, .SDcols = c(VarList$yVars, "qx")]
			, by = VarList$common
			, all=F
		)

		# test d'un graphique
		{
			# library(ggplot2)
			# lum_0_100(29)
			# ggplot2::ggplot(
			# 	t_inc_commut
			# 	, aes(inc, qx)
			# 	# , line_type = dim_type_dep
			# 	) + geom_line(aes(
			# 		col = dim_type_dep
			# 		, line_type = dim_sexe
			# 	)) + xlim(c(60,90))
			# [is.na(lx.x) OR is.na(lx.y)]
			# testMerge[is.na(lx.x)]
			# testMerge[is.na(lx.y)]
			# testMerge[!is.na(lx.y)&!is.na(lx.x)]
}
		merge_tables <- copy(testMerge)
		# bug si on a des dimensions : a gerer
		# Finalement --> a gerer si on n'a pas de dimensions du coup
		# somme des qx puis calcul du lx correspondant
		# somme...
		merge_tables[
			, qx_pres := rowSums(merge_tables[, .(qx.x, qx.y)], na.rm = T)
			]

		# ...lx
		dimVars <- setdiff(VarList$all, "inc")
		merge_tables$lx_pres <- qx_to_lx(
			merge_tables
			, dimsNames = dimVars
			, incName = "inc"
			, qxName = "qx_pres"
		)


		# merge_tables[
		# 	, dims := do.call(paste, .SD)
		# 	, .SDcols = c(setdiff(VarList$all, "inc"))
		# 	]
		t_pres <- merge_tables[, .(lx = lx_pres), by = eval(VarList$all)]
} # fin 2 - t_pres
	# Jointure t_pres et lg_maintien, afin d'afficher pour chaque age vu
	# aujourd'hui, ts les ages possibles d'entree en dep, + ax correspondants
	# lg_rente_dep
	lg_maintien_commuts[1:6]
	t_pres_commut <- Complete_commut(t_pres, incName = "inc", i = i)
	{
		table(lg_maintien_commuts$inc)
		{
			# on verifie si dim_age_dep est renseigne ou non
			if (missing(dim_age_dep_name)) {
				if (!"dim_age_dep" %in% names(lg_maintien_commuts)){
					message("column dim_age_dep not in lg_maintien :")
					message("please provide the name for corresponding column")
				}
			} else {
				if (!dim_age_dep_name %in% names(lg_maintien_commuts)){
					message("column ", dim_age_dep_name, " not in lg_maintien :")
					message("please provide the name for corresponding column")
				} else {
					# lg_maintien_commuts[, dim_age_dep := get(dim_age_dep_name)] # nop : mieux vaut ne pas dupliquer des colonnes
					setnames(lg_maintien_commuts, dim_age_dep_name, "dim_age_dep")
					message("dim_age_dep set to ", dim_age_dep_name)
				}
			}
			wk$inputs
			# pdt cartesien
			# pour chaque age_vis : liste de tous les age_dep possibles et de l'ax correspondant
			dimsList_maintien <- compareVars(t_pres, lg_maintien_commuts, "dim_|^inc$")
			dimsList_maintien <- compareVars(t_pres, lg_maintien_commuts, "dim_")

			# d'abord : ax pour chaque age_dep. On va considerer que dim_age_dep
			# est obligatoire pour l'instant
			interm_t_ax <- lg_maintien_commuts[
				readr::parse_number(as.character(inc))==0
				, c(.SD, .(cst = T))
				, .SDcols = c(dimsList_maintien$yVars, "a_pp_x")
				]


			merge_pres_maintien <- merge(
				t_pres[
					, c(.SD, .(cst = T, age_vis = inc, lx_age_vis = lx))
					, .SDcols = c(dimsList_maintien$xVars)
					]
				, interm_t_ax
				, by = c("cst", setdiff(dimsList_maintien$common, "inc"))
				, all=F
				, allow.cartesian = T
			)[order(age_vis, dim_age_dep)][, cst := NULL] # TODO: dim_age_dep a generaliser

			dimsList_maintien$all
			# par quelles variables on peut merge ici ? repertorier les variables qui auront tjrs le meme nom, et celles qu'il faut generaliser
			# du coup dim_age_dep, age_vis sont a peu pres obligatoires
			dimList_merge <- compareVars(merge_pres_maintien, t_pres, "dim_")
			# merge_pres_maintien[dim_age_dep>=age_vis] # plus tard : filtrer pour efficacité
			dimList_merge$all
			# on re-merge pour avoir le lx de l'age_dep et non de l'age vis
			# pour t_pres il faut
			# info : lx_pres
			# cle : age_vis (avec age_dep), + dims
			dimsButAge <- setdiff(dimList_merge$xVars, "dim_age_dep")
			interm_t_pres <- t_pres[
				, c(.SD, .(lx_pres = lx, age_vis = inc))
				, .SDcols = dimList_merge$yVars
				]
			merge_pres_maintien_lx_age_dep <- merge(
				x = merge_pres_maintien
				, y = interm_t_pres
				, by.x = c("dim_age_dep", dimList_merge$common)
				, by.y = c("age_vis", dimList_merge$common)
			)[order(age_vis, dim_age_dep)]
		} # v_precedente : 2 etapes mais 1 seule suffit --> non il faut bien les deux

		{
		# 	merge_pres_maintien <- merge(
		# 		t_pres[, .(age_vis, lx_age_vis = lx, cst=T, lx_pres = lx)]
		# 		, lg_maintien_commuts[
		# 			readr::parse_number(as.character(inc_anc)) == 0
		# 			, .(cst = T, a_pp_x, dim_age_dep = dim_age_dep)
		# 			]
		# 		, by = "cst"
		# 		, allow.cartesian = T
		# 		# , all.x = T
		# 	)[order(age_vis, dim_age_dep)][, cst := NULL]
} # version en 1 seule etape : ne fonctionne pas

		# on rajoute la proba de tomber en dep a chaque age_vis
		merge_pres_maintien_p_dep <- merge(
			merge_pres_maintien_lx_age_dep
			, wk$inputs$t_inc[, .(age_vis, p_dep = tx)]	# TODO : gerer les dimensions egalement sur cette partie
			, by.x = "dim_age_dep"
			, by.y = "age_vis"
		)[order(age_vis, dim_age_dep)] #age dep >= age vis

		merge_pres_maintien_p_dep[, p_survie := lx_pres / lx_age_vis]
		merge_pres_maintien_p_dep[, VAP_dep := p_dep * a_pp_x * p_survie]
		wk$results$lg_rente_dep <- merge_pres_maintien_p_dep[dim_age_dep>=age_vis]
} # fin x - lg_rente_dep : tables de VAP des rentes dep

	# t_VAP_gie_dep
	dimCols <- grep("^dim_",names(wk$results$lg_rente_dep), value = T)
	res_dimsButAge <- setdiff(dimCols, "dim_age_dep")
	wk$inputs$lg_maintien
	wk$results$lg_rente_dep
	wk$results$t_VAP_gie_dep <- wk$results$lg_rente_dep[
		, .(VAP_garantie_dep = sum(VAP_dep))
		, by = c("age_vis", res_dimsButAge)
	]

	if (detailedRes) return(wk) else return(wk$results$t_VAP_gie_dep)

}


# quelques tests... ------------------------------------------------------------
{
	library(R.AlphA)
	library(data.table)

	root <- dirname(rstudioapi::getSourceEditorContext()$path)
	workRRoot <- stringr::str_extract(root, ".*WorkR")
	tbls <- readRDS(file.path(workRRoot, "pop stats", "INPUTS","tables.rds"))

	compareVars(tbls$STMultiVar, tbls$smallTables)
	assumptions <- list()
	assumptions <- tbls$smallTables # Small Tables, simple
	assumptions <- tbls$STMultiVar # Small Tables, multiVars
	assumptions <- tbls$STMult_start50 # Small Tables, simple
	assumptions$lg_maintien <- tbls$lg_maintien
	assumptions$lg_maintien <- tbls$bigTables$bigTable_1000 # Big Tables
	assumptions$lg_maintien <- tbls$bigTables$bigTable_200 # Big Tables
} # tables d'input
test <- dep_table(assumptions)
test <- dep_table(assumptions, dim_age_dep_name = "dim_x")
test <- dep_table(assumptions, dim_age_dep_name = "dim_x", detailedRes = T)
test <- dep_table(assumptions, detailedRes = T)
test$results$t_VAP_gie_dep
# test$results$lg_rente_dep


#TODO :
# dimensions : vraiment ok ? a verifier
# gerer l'exactitude : rentes en milieu d'annee ? dc en milieu d'annee ? etc
