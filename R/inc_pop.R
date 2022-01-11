
#' increment a pop Table
#' @description From a pop table with "generate_pop()" format, simulate the new population at N+1
#' @param old_pop starting pop table
#' @param p_resilRate cancellation rate of people in the table
#' @param p_subRate new rows to add - depending on the current table size
#' @param messages should the function print messages while running (info only)
#' @param age_min will be passed to generate_pop
#' @param age_max will be passed to generate_pop
#' @importFrom stats runif rpois
#' @export

inc_pop <- function(
	old_pop = generate_pop(100)
	, p_resilRate = 2/100
	, p_subRate = 2/100
	, messages = F
	, age_min = 18
	, age_max = 20
){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		print("manualrun")
		old_pop = generate_pop(100)
		p_resilRate = 10/100
		p_subRate = 10/100
		messages = F
	}

	# calculs----
	oldvis <- max(old_pop$dteVision) #prov: vérifier si bien le meilleur moyen de récupérer la date de vision
	newVision <- oldvis %m+% months(12) #prov : ne sera pas toujours un an exactement

	# resiliations----
		# new_stock <- old_pop[T] # [T] sert simplement à "séparer" new_stock de old_pop --> ca ne marche pas
		new_stock <- copy(old_pop) # copy separe vraiment les deux tables
		new_stock[, dteVision := newVision]
		new_stock[, random := runif(.N)]

		#augmentation proba de resil pour ages eleves
		new_stock[, age_vis := decimal_date(dteVision) - decimal_date(dteNais)]
		new_stock[, excess_of_age := pmax(0, age_vis - 100) ]
		new_stock[, random:= random / (1+excess_of_age) ]

		new_stock[, canceled := random < p_resilRate]
		table(new_stock$canceled)

		# suppression des contrats résiliés----
		new_stock <- new_stock[canceled == F]
		if (nrow(new_stock)<5) {
			print(paste0("rows left : ", nrow(new_stock)))
		}

	# nouvelles souscriptions----
	pop_length <- length(unique(old_pop$iD))
	nb_new_subs <- rpois(1, pop_length * p_subRate)

	oldpop_maxiD <- max(old_pop$iD)
	new_subs <- generate_pop(
		nb_new_subs
		# , browse = T
		, min_iD = oldpop_maxiD + 1000 #prov: reflechir a une autre methode pour eviter de repasser sur les memes iD (par ex : ID commence par la date de vision)
		, dteVision = newVision
		, min_sub = oldvis
		, max_sub = newVision
		, messages = messages
		, age_min = age_min
		, age_max = age_max
	) # --> il faut pouvoir gérer les dates de souscription facilement

	# concatenation des nouveaux et des anciens
	popvars <- c(
		NULL
		,"iD"
		,"dteVision"
		,"dteNais"
		,"sexe"
		,"dteSouscription"
		, use.names = T
	)
	new_pop <- rbind(
		new_stock[, ..popvars]
		, new_subs[, ..popvars]
	)
}


# # tests - inutile ici, conserve dans le dossier pop_stats
# library(R.AlphA)
#
# R.AlphA::lum_0_100(70)
# init_pop <- R.AlphA::generate_pop(40, messages = T, age_min = 18, age_max = 20)
# # next_pop <- inc_pop(init_pop, p_subRate = 0.2, p_resilRate = 0.2)
#
# # table(init_pop$dteVision)
# # table(next_pop$dteVision)
# # bind <- rbind(init_pop, next_pop)
# # table(bind$dteVision)
#
# init_pop
#
# # simulation tables successives
# pop_0 <- copy(init_pop)
# for(year_vis in 1:9){
# 	print(year_vis)
# 	assign(
# 		paste0("pop_", year_vis)
# 		, inc_pop(get(paste0("pop_", year_vis-1)), p_subRate = 0.1, p_resilRate = 0.1)
# 	)
# }
# # pop_0[, pres_0 := 1]
# # pop_1[, pres_1 := 1]
# # pop_2[, pres_2 := 1]
#
# # test via merge
# popStep <- 0
# popAll <- copy(get(paste0("pop_",popStep)))
# setnames(popAll, old = "dteVision", new = paste0("vision_", popStep))
#
# for (popStep in 1:9) {
# 	# popStep <- 3
# 	popTable <- get(paste0("pop_",popStep))
# 	popAll <- merge(
# 		all = T
# 		, popAll
# 		, popTable
# 		, by = c("iD", "dteSouscription", "sexe", "dteNais")
# 	)
# 	setnames(popAll, old = "dteVision", new = paste0("vision_", popStep))
# }
# popAll
# # test via rbind
# popList <- list()
# for (popName in ls(pattern = "pop_[0-9]")){
# 	print(popName)
# 	popList[[popName]] <- get(popName)
# }
#
# bindPop <- pop_0[0]
# for(popTable in popList){
# 	bindPop <- rbind(bindPop, popTable)
# }
#
# bindPop[ , minVis := min(dteVision), by = iD]
# bindPop[ , maxVis := max(dteVision), by = iD]
# bindPop[ , resil := maxVis==dteVision]
# bindPop[ , newsub := minVis==dteVision]
# mvtSummary <- bindPop[
# 	, .(
# 		resils = sum(resil)
# 		, subs = sum(newsub)
# 		, nb_iD = length(unique(iD))
# 	)
# 	, by = .(dteVision)
# ][ , resilRatePC := round(resils / nb_iD * 100, 2)]
# mvtSummary
# sum(bindPop$newsub)
#
# merge(
# 	# NULL
# 	all = T
# 	, pop_0
# 	, pop_1
# 	, by = c("iD", "dteSouscription", "sexe", "dteNais")
# )
#
# get("pop_1")[, a := 2]
# next_pop
# # recherche sur affichage des resils et des souscriptions
# 	init_ids <- data.table(iD = init_pop$iD, present_init = T)
# 	next_ids <- data.table(iD = next_pop$iD, present_next = T)
# 	compare_ids <- merge.data.table(
# 		all = T
# 		, init_ids
# 		, next_ids
# 		, by = "iD"
# 	)
# 	compare_ids[, new_sub := (!present_init|is.na(present_init)) & present_next]
# 	compare_ids[, resil   := (!present_next|is.na(present_next)) & present_init]
# 	compare_ids[
# 		, .(nb_iD_unique = .N)
# 		, by = .(resil, new_sub)
# 	]
#
# 	table(compare_ids$present_init, compare_ids$present_next, useNA = "always")
#
#
# # autres trucs
# for (i in 1:20) {
# 	next_pop <- inc_pop(
# 		next_pop
# 		, p_subRate = 16/100
# 		# , p_resilRate = 1/100
# 		, p_resilRate = 15/100
# 		# , messages = F
# 	)
# 	next_pop[, age_vis := round(decimal_date(dteVision) - decimal_date(dteNais))]
# 	plot(table(next_pop$age_vis), main = str_c("year_vis : ", year(max(next_pop$dteVision))))
# 	Sys.sleep(0.1)
# }
#
# #
# # for (i in 1:50) {
# # 	print(year(max(next_pop$dteSouscription)))
# # 	# if (max(next_pop$dteSouscription) > max(next_pop$dteVision) %m+% months(12)) {print("yes")}
# # 	next_pop <- inc_pop(next_pop, p_resilRate = 2/100)
# # }
# #
# # # table(year(next_pop$dteNais))
# # # table(year(next_pop$dteSouscription))
# # table(
# # 	deparse.level = 2
# # 	, year(init_pop$dteNais)
# # 	, year(init_pop$dteSouscription)
# # 	)
# # table(
# # 	deparse.level = 2
# # 	, year(next_pop$dteNais)
# # 	, year(next_pop$dteSouscription)
# # 	)
# # old_pop[1:20]
#
# # # tests loi de poisson
# # sample_collect <- data.table()
# # for (i in 1:10000) {
# # 	sample_collect <-  rbind(sample_collect, rpois(1, pop_length * p_subRate))
# # }
# # sample_collect
# # mean(sample_collect$x)
# # barplot(table(sample_collect$x))
# # barplot(height =  sample_collect$x)
#
#
#
#
