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
# # ====================================================================================='
# #============================= RECHERCHES ============================================='
# # ====================================================================================='
# # recherches sur comment ressortir uniquement les doublons d'un df
# 	sPop[iD>5, iD:= 5]
# 	duplicated.data.frame(sPop$iD)
# 	duplicated(sPop$iD)
# 	sPop[duplicated(iD)]
# 	print(sPop[duplicated(iD)])
# 	if(anyDuplicated(sPop$dteNais)){print("yo")}
# 	if(anyDuplicated(sPop$iD)){print(sPop[duplicated(iD)])}
#
#
#
# # recherches sur le format date
# 	sPop$dteNais_txt <- as.character(sPop$dteNais)
# 	dateColstris <- listcols(sPop, "^dte")
# 	str(dateColstris)
# 	# differentes fonctions...
# 		as_date("1971-02-10 12:00:00")
# 		as.Date("1971-02-10 12:00:00")
# 		as.Date("1971-02-20 12:00:00")
# 		ymd("1971-20-02 12:00:00") # non : normal
# 		ymd("1971-02-20 12:00:00") # non : a cause des heures ?
# 		ymd("1971-02-20") # et oui
# 		ymd("19710220")
# 		ymd("1971.02.20")
# 		ISOdate(1971,2,20)
# 		as_date(ISOdate(1971,2,20))
# 		as.Date(ISOdate(1971,2,20))
# 		?ymd
# 		# as_date est plus puissant apparemment car accepte autre chose que les -
# 		as_date("1971-02-20")
# 		as_date("19710220")
# 		as_date("1971.02.20")
# 		as.Date("1971-02-20")
# 		as.Date("19710220") # nope
# 		as.Date("1971.02.20") # nope
#
# 		# gestion de l'ambiguité ?
# 			# déjà : sans forcer
# 			as_date("1971-02-10")
# 			as_date("19710210")
# 			as_date("1971.02.10")
# 			as.Date("1971-02-10")
# 			# en forçant
# 			as_date("1971-22-10")
# 			as_date("19712210")
# 			as_date("1971.22.10")
# 			as.Date("1971-22-10")
# 			# --> pas mal : on n'accepte pas le ydm donc c'est qu'avant il n'y avait pas d'ambiguité
#
# 		# autre informat courant jjmmaaaa (spoiler : ne marche pas)
# 			as_date("10-02-1971")
# 			as_date("10021971")
# 			as_date("10/02/1971")
# 			as_date("10.02.1971")
# 			as.Date("10-02-1971")
# 			dmy("10-02-1971")
# 			dmy("10021971")
# 			dmy("10/02/1971")
# 			# --> ok : donc on n'accepte que le ymd dans as_date, si c'est un autre format il faut utiliser la fonction spécifique
# 			# as.Date ne renvoie pas d'erreur malgré une date pourrie : nul
#
#
# 		# format de sortie
# 			typeof(as_date("1971.02.20")) # double... ok
# 			class(as_date("1971.02.20"))  # type "double" mais class "Date"... ok ok ok ok...
#
# 			typeof(as.Date("1971-02-20"))  # pareil que as_date
# 			class(as.Date("1971-02-20"))   # pareil que as_date
#
# 			typeof(ISOdate(1971,2,20)) # toujours pareil (double)
# 			class(ISOdate(1971,2,20))  # tiens tiens ! cette fois c'est POSIXct et POSIXt.
#
# 			# as_date d'une ISOdate ?
# 			testiso <- ISOdate(1971,2,20)
# 			typeof(as_date(testiso)) # on revient à la routine : type double ...
# 			class(as_date(testiso))  # ... et classe Date
#
#
#
# 		# en terme de nombres
# 			dated <- as_date("1971-02-20")
# 			dated > 10
# 			dated > 100
# 			dated > 1000
# 			dated - 1
# 			renummed <- as.numeric(dated)
# 			as_date(renummed) # tiens tiens tiens comme ça se trouve
# 			as.Date(renummed) # tss j'en etais sur ça marche pas
# 			as_date(1)
# 			as_date(0) # donc le jour de départ est 01/01/1970
# 			as_date(-1) # normal
# 			as.numeric(as_date("20201001"))
# 			as.numeric(as_date("19201001"))
# 			as_date(-20000)
# 			as_date(Sys.Date()-40000) #donc déjà on peut mettre des warning si date du jour - 40 000
# 			as_date(20000)
# 			as.numeric(dated)
#
# 			dte1 <- as_date("1920.10.01")
# 			dte2 <- as_date("1930.10.09")
#
#
# 			dte2 - dte1
#
#

# # ajouter/retirer un nombre décimal d'années à une date
# 	initdate <- as_date(ISOdate(2020,01,12))
# 	initdate
# 	initdate %m+% 2
# 	initdate %m+% months(2) # ok
# 	initdate %m+% months(2.5) # non
# 	initdate %m+% year(2) # non
# 	initdate %m+% years(2) # OK
# 	initdate %m+% years(2.5) # non
# 	years(1.5) #non
# 	add_with_rollback(initdate, years(2.5))
#
#
# 	initdate %m-% years(12) %m-% months(-1)
# 	yearsless <- 19.45
# 	initdate %m-% years(floor(yearsless))
#
# 	date_decimal(2020.9781298)
# 	decimal_date(Sys.Date())
# 	yearsless %% 1
# 	initdate %m-% months(floor(yearsless%%1 * 12))
# 	initdate %m-% years(floor(yearsless)) %m-% months(floor(yearsless%%1 * 12))
#
# 	initdate
# 	age <- 19.3
# 	to_dec <- decimal_date(initdate)
# 	to_dec
# 	newdate_dec <- to_dec - age
# 	newdate_dec
# 	newdate_date <- date_decimal(newdate_dec)
# 	newdate_date


# a <- decimal_date(Sys.time())
# b <- decimal_date(Sys.time())
# dur_years <- b-a
# dur_years * 3600 * 24 * 365 * 1000
# as.duration(date_decimal(b-a)) # ne marche pas
