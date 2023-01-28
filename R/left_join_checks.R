#' @title left_join + checks if everything went correctly
#' @description typically, check that you did not duplicate your lines
#' @param x left table
#' @param y right table
#' @param ... additionnal arguments passed to dplyr's left_join
#' @param req_preserved_x boolean : do you want to ensure that xTable rows are the same after join (no duplicates) ?
#' @param req_xAllMatch boolean : do you want to ensure that all x values have found a match in y ?
#' @param behavior character : warning, or error
#' @return the joined table
#' @importFrom tibble rowid_to_column rownames_to_column
#' @importFrom tidyr replace_na
#' @importFrom stringr str_remove
#' @import dplyr
#' @export
# library(dplyr)
# library(data.table)
# library(tidyr)
# library(tidyverse)
# library(R.AlphA)
# library(R.AlphA.Life)
# install.packages("xml2")


left_join_checks <- function(
		x
		,y
		, ...
		, req_xAllMatch = 1
		, req_preserved_x = 1
		, behavior = "error"
){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		rm(list = ls())
		warning("! parameters manually defined inside function 'left_join_checks' for tests. Do not use results !")
		workRRoot <- root() %>% str_extract(".*WorkR")
		tbls <- workRRoot %>%
			file.path("pop stats", "INPUTS") %>%
			list.files(pattern = "tables\\.rds", full.names = T) %>%
			readRDS
		x <- data.table(a = 1, age = c(1:4)) %>% print
		y <- data.table(a = 1, age = c(2:4, 4), result = "ok") %>% print

		x <- generate_pop(2E5, age_min = 30, age_max = 36) %>%
			mutate(inc_age_vis = age_dec(dteNais, dteVision) %>% round,
				   dim_sexe = sexe)
		y <- tbls$STMRes$t_vie
		req_xAllMatch = 1
		req_preserved_x = 1
		req_yNotFound = 0
	}
	# a voir plus tard - verif que pas de vars deja avec ljc_
	R.AlphA::compareVars(x, y, pattern = "ljc_")

	# preparation pour merge
	xMerge <- x %>% rowid_to_column(var = "ljc_xID") %>% mutate(ljc_inX = 1)
	yMerge <- y %>% rowid_to_column(var = "ljc_yID") %>% mutate(ljc_inY = 1)

	# merge
	joinXY <- left_join(
		xMerge
		, yMerge
		, ...
	) %>% replace_na(list(ljc_inX = 0, ljc_inY = 0))

	# check
	xMerge;yMerge;joinXY

	chk_preserved_x <- all.equal(joinXY$ljc_xID, xMerge$ljc_xID) %>% isTRUE
	chk_dups_x <- duplicated(joinXY$ljc_xID) %>% sum
	chk_preserved_y <- all.equal(joinXY$ljc_yID, yMerge$ljc_yID) %>% isTRUE
	chk_dups_y <- duplicated(joinXY$ljc_yID) %>% sum
	joinMatch_prep <- joinXY %>% count(ljc_inX, ljc_inY)
	joinMatch <- expand.grid(ljc_inX = 0:1, ljc_inY = 0:1) %>%
		left_join(joinMatch_prep) %>%
		suppressMessages %>%
		replace_na(list(n=0))
	chk_yNotFound <- joinMatch %>% filter(!ljc_inY) %>% pull(n) %>% sum
	chk_xAllMatch <- chk_yNotFound == 0

	chk_preserved_x
	chk_preserved_y
	chk_dups_x
	chk_dups_y
	chk_xAllMatch
	chk_yNotFound

	valuesTable <- mget(ls(pattern = "^chk_")) %>%
		unlist %>%
		data.frame(value = .) %>%
		rownames_to_column("check") %>%
		mutate(key = str_remove(check, "^chk_|^req_"))

	reqsTable <- mget(ls(pattern = "^req_")) %>%
		unlist %>%
		data.frame(req = .) %>%
		rownames_to_column("check") %>%
		mutate(key = str_remove(check, "^chk_|^req_"))

	checksTable <- full_join(valuesTable, reqsTable, by = "key") %>%
		mutate(is_problem = value != req) %>%
		select(key, value, req, is_problem) %>%
		arrange(is_problem %>% desc)

	nbPbs <- sum(checksTable$is_problem, na.rm = T)

	if (nbPbs) {
		commonMsg <- paste0(nbPbs, " problem(s) during merge",
							"\nsee report for details")
		if (behavior == "warning") {
			print(checksTable)
			warning(commonMsg)
		} else if (behavior == "error") {
			print(checksTable)
			stop(commonMsg)
		}
	}

	# return joined table but the intermediary columns
	return(joinXY %>% select(-starts_with("ljc_")))
}


#
#
# workRRoot <- root() %>% str_extract(".*WorkR")
# tbls <- workRRoot %>%
# 	file.path("pop stats", "INPUTS") %>%
# 	list.files(pattern = "tables\\.rds", full.names = T) %>%
# 	readRDS
# x <- data.frame(a = 1, age = c(1:4))
# y <- data.frame(a = 1, age = c(2:4, 4), result = "ok")
#
# testXTbl <- generate_pop(2E3, age_min = 30, age_max = 36) %>%
# 	mutate(inc_age_vis = age_dec(dteNais, dteVision) %>% round,
# 		   dim_sexe = sexe)
# testYTbl <- tbls$STMRes$t_vie
# test <- left_join_checks(
# 	# testXTbl, testYTbl
# 	x,y
# 	, by = c("inc_age_vis", "dim_sexe")
# 	# , behavior = "warning", req_preserved_x = 0
# 	)
#
