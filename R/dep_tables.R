# rm(list = ls())
#' Long-term care EPV
#' @description compute Expected Present Value of long-term care annuities, in order to help for PRC (risk-increase provision ?)
#' @param inputs 3 tables : incidence rate / mortality / dependents mortality
#' @param i actualisation rate --> not yet
#' @param detailedRes if FALSE : only returns the table of EPV by age
#' @importFrom readr parse_number
#' @importFrom stringr str_extract str_detect
#' @export
dep_table <- function(
	inputs # objet contenant les 3 tables d'inputs
	, detailedRes = T
){
	wk <- list()
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		message("! parameters manually defined inside function for tests. Do not use results !")
		detailedRes = T
		{
			root <- dirname(rstudioapi::getSourceEditorContext()$path)
			workRRoot <- stringr::str_extract(root, ".*WorkR")
			setwd(root)
			# dteVision_inc <- date(ISOdate(2010,01,01)) # date de vision
			# annees <- 60 # nombre d'années sur lesquelles projeter
			inputPath <- file.path(root, "INPUTS") #chemin dossier inputs
			# popFilesPath <- file.path(root, "fichiers_pop")
		} # paramètres (root)
		# inputs : t_vie / t_inc / lg_maintien
		{
			tbls <- readRDS(file.path(inputPath,"tables.rds"))
			inputsPRC <- tbls$smallTables
			inputs$t_vie <- inputsPRC$TH
			inputs$t_inc <- inputsPRC$inc_dep
			inputs$lg_maintien <- tbls$lg_maintien
			inputs$lg_maintien <- inputsPRC$lg_maintien
		} # tables d'input
	}

	wk$inputs <- inputs

	# ajout nbs de commut
	{
		t_vie_commut <- Complete_commut(wk$inputs$t_vie, incName = "age_vis")[, .(age_vis, qx)]
		t_inc_commut <- Complete_commut(wk$inputs$t_inc, incName = "age_vis")[, .(age_vis, qx)]
		lg_maintien_commuts <- Complete_commut(wk$inputs$lg_maintien)
}
	# t_pres : lx prenant en compte les probabilités de décès et d'entrée en dépendance
	{
		merge_tables <- merge(t_vie_commut,t_inc_commut, by = "age_vis")
		merge_tables[, qx_pres := rowSums(merge_tables[, .(qx.x, qx.y)], na.rm = T)]
		merge_tables[, lx_pres := 10**5]
		for (i in 2:nrow(merge_tables)) {
			print(i)
			lx_prec <- merge_tables[i-1]$lx_pres
			qx_prec <- merge_tables[i-1]$qx_pres
			merge_tables[i, lx_pres := lx_prec * (1-qx_prec)]
		}
		t_pres <- merge_tables[, .(lx = lx_pres, age_vis)]
}
	# Jointure t_pres et lg_maintien, afin d'afficher pour chaque age vu aujourd'hui
	#, tous les ages possible d'entree en dep et les ax correspondants
	# lg_rente_dep
	{
		{
			merge_pres_maintien <- merge(
				t_pres[, .(age_vis, lx_age_vis = lx, cst=T)]
				, lg_maintien_commuts[
					readr::parse_number(as.character(inc_anc)) == 0
					, .(cst = T, a_pp_x, dim_age_dep = dim_age_dep)
					]
				, by = "cst"
				, allow.cartesian = T
				# , all.x = T
			)[order(age_vis, dim_age_dep)][, cst := NULL]

			merge_pres_maintien <- merge( # attention : recursif
				x = merge_pres_maintien
				, y = t_pres[, .(lx_pres = lx, age_vis)]
				, by.x = "dim_age_dep"
				, by.y = "age_vis"
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

		merge_pres_maintien_p_dep <- merge(
			merge_pres_maintien
			, wk$inputs$t_inc[, .(age_vis, p_dep = tx)]
			, by.x = "dim_age_dep"
			, by.y = "age_vis"
		)[order(age_vis, dim_age_dep)] #age dep >= age vis

		merge_pres_maintien_p_dep[, p_survie := lx_pres / lx_age_vis]
		merge_pres_maintien_p_dep[, VAP_dep := p_dep * a_pp_x * p_survie]
		wk$results$lg_rente_dep <- merge_pres_maintien_p_dep[dim_age_dep>=age_vis]
}

	# t_VAP_rente_dep
	wk$results$t_VAP_rente_dep <- wk$results$lg_rente_dep[
		, .(VAP_garantie_dep = sum(VAP_dep))
		, by = .(age_vis)
	]

	if (detailedRes) return(wk) else return(wk$results$t_VAP_rente_dep)

}


# {
# 	root <- dirname(rstudioapi::getSourceEditorContext()$path)
# 	tbls <- readRDS(file.path(root, "INPUTS","tables.rds"))
# 	inputsPRC <- tbls$smallTables
# 	assumptions <- list()
# 	assumptions$t_vie <- inputsPRC$TH
# 	assumptions$t_inc <- inputsPRC$inc_dep
# 	assumptions$lg_maintien <- tbls$lg_maintien
# 	assumptions$lg_maintien <- inputsPRC$lg_maintien
# } # tables d'input
# test <- dep_table(assumptions)
# test <- dep_table(assumptions, detailedRes = T)
# test$results$t_VAP_rente_dep
# test$results$lg_rente_dep
