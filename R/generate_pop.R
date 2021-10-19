#' generating random population
#' @description création ou import d'une table de pop --> sPop contenant les 5 variables sources nécessaires
#' @param tableLength number of rows
#' @param dteVision vision date
#' @param prop_F proportion of women in population, betwenn 0 and 1
#' @param min_iD starting iD. Will be incremented by 1 on each new row
#' @param min_sub minimum subscription date
#' @param max_sub maximum subscription date
#' @param browse for debugging purposes
#' @param age_min minimum age
#' @param age_max maximum age
#' @param messages should the function print messages while running (info only)
#' @param getTime if True, returns an object containing generated data and timing of different steps
#' @importFrom stats runif
#' @export
generate_pop <- function(
	# construction fonction : 5 variables
	# variables independantes
		# ID
		# sexe
	# variables dependantes
		# date de vision (dtv) --> cf autres dates
		# dtv >= dts > dtn
		# date de naissance (dtn) / date de souscription (dts)
		# dts > dtn + 18 ans
	tableLength = 1
	, dteVision = Sys.Date()
	# parametres à travailler
	, prop_F = 0.5
	, min_iD = 1
	, min_sub = ISOdate(year(Sys.Date()), 1,1)
	, max_sub = Sys.Date()
	# options
	, browse = F
	, age_min = 18
	, age_max = 30
	, messages = F
	, getTime = F
){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		print("manualrun")
		tableLength = 0
		tableLength = 10**1
		dteVision = Sys.Date()
		prop_F = 0.5
		browse = F
		min_iD = 1
		min_sub = ISOdate(year(Sys.Date())-7, 1,1)
		max_sub = ISOdate(year(Sys.Date())-3, 1,1)
		age_min = 18
		age_min = 59
		age_max = 61
		messages = F
		getTime = F
	}

	if (browse){browser()}
	if(getTime) steps_time <- timer(start = TRUE)

	# 1 iD (la base) : longueur de la table ----
		if(getTime) steps_time <- timer(timer_table = steps_time, step = 1, stepName = "iD")
		sPop <- data.table(iD = min_iD:(min_iD+tableLength-1))
		sPop <- data.table(iD = seq(min_iD, length.out = tableLength))
		# 2 dteVision ----
		if (messages) {print("dteVision")}
		sPop$dteVision <- as_date(dteVision)
		sPop[ , dteVision_dec := decimal_date(dteVision)]
		# 3 sexe----
		if (messages) {print("sexe")}
		sPop[, sexe := ifelse(runif(.N)>prop_F, "H", "F")]

	# (old) 4 date de naissance ----
		# # en fait c'est pas la date de naiss qui doit etre choisie mais la date de sous et l'age
		# sPop$dteNais <- rdate(
		# 	tableLength
		# 	, yearmin = BirthYearRef - birthsDispersion
		# 	, yearmax = BirthYearRef + birthsDispersion
		# )

	# 4 dteSouscription ----
		# methode en fonction de min et max
		if(getTime) steps_time <- timer(timer_table = steps_time, step = 2, stepName = "dteSouscription")
		if (messages) {print("dteSouscription")}
		sPop$dteSouscription <- rdate(
			tableLength
			, min = min_sub
			, max = max_sub
		)
		if (anyNA(sPop$dteSouscription)) {
			print("NAs in dteSouscription")
			browser()
		}
		sPop$dteSouscription_dec <- decimal_date(sPop$dteSouscription)

	# 5 dteNais  ----
		if(getTime) steps_time <- timer(timer_table = steps_time, step = 3, stepName = "dteNais")
		if(messages) {print("dteNais")}
		# sPop[ , age_souscription := runif(.N, min = age_min, max = age_max)] # 2021.09.27 ne respecte pas l'age min/max demande
		# sPop[ , dteNais_dec := dteSouscription_dec - age_souscription]
		sPop[ , age_vision := runif(.N, min = age_min, max = age_max)]
		sPop[ , dteNais_dec := dteVision_dec - age_vision]
		sPop[ , dteNais := as_date(date_decimal(dteNais_dec))]


	# # dteSouscription en fonction de dteNais
	# sPop[, dteSouscription := as_date(ISOdate(
	# 	year = year(dteNais) + sample(18:40, .N, replace = T)
	# 	, month = sample(1:12, .N, replace = T)
	# 	, day = sample(1:28, .N, replace = T) # attention methode bien foireuse a modifier
	# ))
	# ]
	if (messages) {print("check if null")}
	# if (is.null(dteVision)|is.null(max(sPop$dteSouscription))){print("one is null")}
	# if (missing(dteVision)){print("one is missing")}
	if(getTime) steps_time <- timer(timer_table = steps_time, step = 3.5, stepName = "checking if dteSous > dte Vis")
	if (messages) {print("check if dteSouscription > dteVision")}
	if (max(sPop$dteSouscription)>dteVision) {
		print("max(sPop$dteSouscription)>dteVision : correcting")
		sPop[, dteSouscription := pmin(dteVision, dteSouscription)]
	}
	# table(year(sPop$dteNais), year(sPop$dteSouscription)) #PI

	as.data.table(colnames(sPop))
	var_pop <- c(
		NULL
		, "iD"
		, "dteVision"
		, "sexe"
		, "dteSouscription"
		# , "dteSouscription_dec"
		# , "age_souscription"
		# , "dteNais_dec"
		, "dteNais"
	)
	sPop_export <- sPop[, ..var_pop]

	if (getTime) {
		export <- list()
		export$sPop <- sPop_export
		export$steps_time <- steps_time
	} else {
		export <- sPop_export
	}
	return(export)
}



# tests -------------------------------------------------------------------



# library(data.table)
# library(lubridate)
# library(R.AlphA)
# generate_pop(10**5, getTime = T)
# dep_pop <- generate_pop(
# 	10**6
# 	, age_min = 58
# 	, age_max = 61
# 	, min_sub = ISOdate(2010,1,1)
# 	, max_sub = ISOdate(2010,1,1)
# )
# dep_pop[, age := year(dteVision)- year(dteNais)]
# dep_pop[
# 	, .(
# 		nb_lignes = .N
# 	)
# 	, by = .(age)
# ]
# # generate_pop(10**7)


# tests, recherches...



# listcols <- function(data, pattern, ignore.case = T, getnames = F){
# 	tabled_data <- as.data.table(data)
# 	reg_pattern <- regex(pattern)
# 	found_names <- grep(colnames(tabled_data), pattern = pattern, ignore.case = ignore.case, value = T)
# 	# tabled_data[, get(found_names)]
# 	if (getnames) {
# 		return(found_names)
# 	} else {
# 		return(tabled_data[, ..found_names])
# 	}
# }
#
#
# listcols(sPop, "DATE|id")
# listcols(sPop, "DATE")
# listcols(sPop, "DATE", getnames = T)
# listcols(sPop, "qsdlkfjqmsfjm", getnames = T)
# listcols(sPop, "qsdlkfjqmsfjm")
# listcols(sPop, "num.{0,3}c(on)?t(rat)?|id")
#
# format_pop <- function(
# 	pop_in
# 	, id  # le = NULL est-il obligatoire pour accepter les valeurs manquantes ?
# 	, dte_vision
# 	, dte_naiss
# 	, sexe
# 	, dte_sousc
# ){
# 	# browser()
# 	# avant les retraitements : identification des colonnes
# 	test <- c(id, dte_vision, dte_naiss, sexe, dte_sousc) # liste des noms rentres en parametres
#
# 	pop_out <- as.data.table(pop_in)
# 	pop_out <- pop_out[, ..test]
# 	setnames(pop_out, old = test, new = c("id", "dte_vision", "dte_naiss", "sexe", "dte_sousc"))
# 	# 1 iD (num de contrat/d'assure, ou concatenation de differentes infos, )
# 		if(anyDuplicated(sPop$iD)){
# 			print("duplicates found in iD variable. Better double check results : ")
# 			print(sPop[duplicated(iD)])
# 		}
# 	# 2 dte_vision
# 		print(str_c("dte_vision missing ", missing(dte_vision)))
#
# 	# 3 dte_naiss
# 		# guess_dte_naiss <- grep(colnames(pop_in), pattern = "d.?te.{0,2}nais", ignore.case = T, value = T)
# 		# pop_out$dte_naiss <- pop_in[, get(guess_dte_naiss)]
# 		print(pop_out$dte_naiss)
# 		print(as_date(pop_out$dte_naiss))
# 		test <- listcols(pop_in, "d.?te.{0,2}nais")
# 		# pop_out$dte_naiss <- listcols(pop_in, "d.?te.{0,2}nais")
# 	# 4 sexe
# 	# 5 dte_sousc
#
# 	return(pop_out)
# }
#
# sPop[1:10]
# format_pop(
# 	sPop
# 	, id = "iD"
# 	, dte_vision = "dteVision"
# 	, dte_naiss = "dteNais"
# 	, sexe = "sexe"
# 	, dte_sousc = "dteSouscription"
# )
# as.Date()
# as.Date("01/10/1972")
# as_date("01/10/1972")
#
#
