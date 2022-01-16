# rm(list = ls())
#' merge (cross-join ?) life tables
#' @description given 2 prefixed life tables, merges them on common dims and
#' makes a cross-join for exclusive dims
#' @param XTbl "left" table
#' @param YTbl "right" table
#' @param valPatt pattern for value columns
#' @param xName name of X tabl, used to rename duplicate columns
#' @param yName name of Y tabl, used to rename duplicate columns
#' @param maxRows if expected rows after merge are higher than maxRows, function
#' @param message show expected and observed lines to check calculations
#' will stop executing
#' @export
mergeLifeTables <- function(
	XTbl
	, YTbl
	, valPatt = "^qx$"
	, xName
	, yName
	, maxRows = 1e7
	, message = F
){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		warning("! parameters manually defined inside function for tests. Do not use results !")
		{
			library(R.AlphA)
			# library(data.table)
			root <- dirname(rstudioapi::getSourceEditorContext()$path)
			workRRoot <- stringr::str_extract(root, ".*WorkR")
			setwd(root)
			# lum_0_100(20)
			inputPath <- file.path(workRRoot, "pop stats", "INPUTS") #chemin dossier inputs
		}
		tbls <- readRDS(file.path(inputPath,"tables.rds"))
		nRowsDftTable <- 100
		t_res <- data.table(
			inc_anc_ct = 1:nRowsDftTable
			, res_rate = seq(from = 0.12, to = 0.03, length.out = nRowsDftTable)
		)
		XTbl <- Complete_commut(tbls$classic$t_vie)
		YTbl <- t_res[, qx := res_rate]
		valPatt <- "^qx$"
		xName <- "test"
		yName <- "aussitesT"
		XTbl <- tbls$qalydays$lg_survie_hors_dep
		YTbl <- tbls$qalydays$incidence
	}


	dimsIncsXY <- compareVars(XTbl, YTbl, pattern = "^dim_|^inc_")
	valCols <- compareVars(XTbl, YTbl, valPatt)

	intersects <- data.table(
		varName = character()
		, Xlength = numeric()
		, Ylength = numeric()
		, intersectLength = numeric()
	)
	# Il faut fusionner 2 tables avec largeur ET longueur differentes
	# TODO: ajouter un avertissement si on supprime des lignes
	# verif : les dimensions en commun ont elles les memes valeurs ?
	for (testVar in dimsIncsXY$common){
		# testVar <- "dim_sexe"
		xVals <- unique(XTbl[, get(testVar)])
		yVals <- unique(YTbl[, get(testVar)])

		intersects_add <- data.table(
			varName = testVar
			, intersectLength = length(intersect(xVals, yVals))
		)
		intersects <- rbind(intersects, intersects_add, fill = TRUE)
		if(length(setdiff(xVals, yVals))) {
			message("for column : ", testVar)
			message("some values are not in common ")
			message("vals X : ", paste(xVals, collapse = ","))
			message("vals Y : ", paste(yVals, collapse = ","))
			message(
				"values not in common : "
				, paste(
					collapse = ","
					, setdiff(xVals, yVals)
					, setdiff(yVals, xVals)
				) # attention bug, a revoir
			)
		} # prevenir si des valeurs pas en commmun : pas tres utile
	}

	for (testVar in dimsIncsXY$onlyX){
		# testVar <- "dim_annee_en_cours"
		xVals <- unique(XTbl[, get(testVar)])
		intersects_add <- data.table(
			varName = testVar
			, Xlength = length(xVals)
		)
		intersects <- rbind(intersects, intersects_add, fill=TRUE)
	}
	for (testVar in dimsIncsXY$onlyY){
		# testVar <- "dim_annee_en_cours"
		yVals <- unique(YTbl[, get(testVar)])
		intersects_add <- data.table(
			varName = testVar
			, Ylength = length(yVals)
		)
		intersects <- rbind(intersects, intersects_add, fill=TRUE)
	}
	intersects
	intersects[, testMax := pmax(intersectLength, Xlength, Ylength, na.rm = T)]
	intersectsButGrp <- intersects[!str_detect(varName, "dim_dif_|inc_grp_")]
	expectedLines <- prod(intersectsButGrp$testMax)
	if(expectedLines > maxRows){
		stop("expected lines (", sepThsd(expectedLines), ") is greater than maxRows ("
			 , sepThsd(maxRows), ") : increase maxRows or reduce tables size."
		)
	}
	if(message) message("expected lines : ", sepThsd(expectedLines))

	xPart <- XTbl[, .SD, .SDcols = c(dimsIncsXY$xVars, valCols$xVars)]
	yPart <- YTbl[, .SD, .SDcols = c(dimsIncsXY$yVars, valCols$yVars)]
	XY <- merge(
		all=F
		, x = xPart[, cst := TRUE]
		, y = yPart[, cst := TRUE]
		, by = c("cst", dimsIncsXY$common)
		, allow.cartesian = TRUE
	)[, cst := NULL]
	if (!missing(xName)) {
		names(XY) <- str_replace_all(names(XY), "\\.x", paste0("_", xName))
	}
	if (!missing(yName)) {
		names(XY) <- str_replace_all(names(XY), "\\.y", paste0("_", yName))
	}
	if(message) message("observed lines : ", sepThsd(nrow(XY)))
	return(XY)
}



