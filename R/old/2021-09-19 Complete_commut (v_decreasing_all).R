# rm(list = ls())
#' adding commutation numbers to a mortality table
#' @description from lxTable and given act rate, adds commut columns
#' @param lxTable 2 columns : age and lx
#' @param i actualisation rate
# #' @importFrom stats runif
#' @export
Complete_commut <- function(lxTable, i){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		message("! parameters manually defined inside function for tests. Do not use results !")
		tbls <- readRDS(list.files("../pop stats/INPUTS", "tabl.*rds", full.names = T))
		lxTable = tbls$TE[1:10**3] # TE : table de survie bien longue (10k lignes)
		i = 0
	}
	tableToFill <- as.data.table(copy(lxTable))
	if(missing(i)) {message("i is missing : set to 0 by default"); i <- 0}
	v <- 1/(1+i)
	tableToFill[is.na(lx), lx := 0] # retraitement
	# ages <- tableToFill$age
	# ages_but_last <- min(ages):(max(ages)-1)# correction
	#
	tableToFill[ , `:=` (
		px = 0
		, qx = 0
		, Dx = 0
		, Nx = 0
		, a_pp_x = 0
		, ax = 0
	)]
	{
	# # --> bcp plus efficace de tout affecter en une seule ligne
	# 	log_nb_loop <- 3
	# 	system.time(for (i_loop in 1:10**log_nb_loop) {
	# 		tableToFill[ , px := 0]
	# 		tableToFill[ , qx := 0]
	# 		tableToFill[ , Dx := 0]
	# 		tableToFill[ , Nx := 0]
	# 		tableToFill[ , a_pp_x := 0]
	# 		tableToFill[ , ax := 0]
	# 	}) # premiere version
	# 	system.time(for (i_loop in 1:10**log_nb_loop) {
	# 		tableToFill[ , `:=` (
	# 			px = 0
	# 			, qx = 0
	# 			, Dx = 0
	# 			, Nx = 0
	# 			, a_pp_x = 0
	# 			, ax = 0
	# 		)]
	# 	}) # deuxieme version --> et bah c'est beaucoup plus rapide comme ca (presque 10 x plus)
	# 	system.time(for (i_loop in 1:10**log_nb_loop) {
	# 		tableToFill[ , px := 0][ , qx := 0][ , Dx := 0][ , Nx := 0][ , a_pp_x := 0][ , ax := 0]
	# 	}) # troisieme version : comme si il y avait plein de lignes, mais toutes sur la meme --> un touuut petit peu plus rapide
} # test de rapidite entre affecter toutes les variables d'un coup ou sur plusieurs lignes
	####### v_decr_method #####
		# dttest <- data.table(a = 1)		# petit test....
		# dttest[ , `:=` (b = 2, c = b+a)]	# ... ne marche pas. Chercher un equivalent de CALCULATED
		rows_index_decr <- nrow(tableToFill):1

		for (i_row in rows_index_decr){
			# i_row <- rows_index_decr[1]
			tbl_xp1	<- tableToFill[i_row+1]
			tbl_x 	<- tableToFill[i_row]
			# tbl_x[, `:=` (
			# 	px = tbl_xp1$lx / lx  #px
			# 	, qx = 1 - px
			# )]
			tbl_x[, px := tbl_xp1$lx / lx]  	# px
			tbl_x[, qx := 1 - px] 				# qx - sortir ? (de la boucle, pour etre plus efficace)
			tbl_x[, Dx := lx * v^age]			# DX - sortir ?
			tbl_x[, Nx := tbl_xp1$Nx + Dx] 		# Nx ---> c est la qu'il y a un bon risque de foirage
			tbl_x[, a_pp_x := Nx/Dx] 			# äx - sortir ?
			tbl_x[, ax := a_pp_x - 1] 			# ax - sortir ?
			tbl_x[is.na(tbl_x)] <- 0
			tableToFill[i_row] <- tbl_x
		} # boucle sur les lignes de la table
		for (i_row in rows_index_decr){
			# i_row <- rows_index_decr[1]
			tbl_xp1	<- tableToFill[i_row+1]
			tbl_x 	<- tableToFill[i_row]
			# tbl_x[, `:=` (
			# 	px = tbl_xp1$lx / lx  #px
			# 	, qx = 1 - px
			# )]
			tbl_x[, px := tbl_xp1$lx / lx]  	# px
			tbl_x[, qx := 1 - px] 				# qx - sortir ? (de la boucle, pour etre plus efficace)
			tbl_x[, Dx := lx * v^age]			# DX - sortir ?
			tbl_x[, Nx := tbl_xp1$Nx + Dx] 		# Nx ---> c est la qu'il y a un bon risque de foirage
			tbl_x[, a_pp_x := Nx/Dx] 			# äx - sortir ?
			tbl_x[, ax := a_pp_x - 1] 			# ax - sortir ?
			tbl_x[is.na(tbl_x)] <- 0
			tableToFill[i_row] <- tbl_x
		} # boucle sur les lignes de la table
	####### ############ #####

	# version precedente, avec filtre sur l'age a chaque fois
		# for(i_age in ages_but_last){
		# 	tableToFill[age == i_age, px := tableToFill[age == i_age+1]$lx / lx] 	# px
		# }
		# tableToFill[, qx := 1 - px]
		# tableToFill[, Dx := lx * v^age]
		# for (i_age in ages) {
		# 	tableToFill[age == i_age, Nx := sum(tableToFill[age >= i_age, Dx])]		# Nx
		# }
		# tableToFill[, a_pp_x := Nx/Dx] # trouver une vraie notation					# äx
		# tableToFill[, ax := a_pp_x - 1] # plus simple qu'une boucle					# ax

	return(tableToFill)
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
