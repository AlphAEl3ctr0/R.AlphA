#' @title qx to lx
#' @description given a mortality table with qx, deduce the lx
#' @param data the table
#' @param dimsNames should be close to parameters from complete_commut function
#' @param incName should be close to parameters from complete_commut function
#' @param qxName default : "qx". if the name is different, provide it
#' @return the lx column
#' @export


qx_to_lx <- function(
	data
	, dimsNames = NULL
	, incName
	, qxName = "qx"
){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		message("! parameters manually defined inside function for tests. Do not use results !")
		data <- data.table(
			inc_anc = 1:10
			, res_rates = seq(from = 0.12, to = 0.03, length.out = 10)
		)
		incName = "inc_anc"
		qxName = "res_rates"
		dimsNames <- ""
		rm(dimsNames)
	}
	melt_maintien <- copy(data)

	melt_maintien[, retain_order := 1:.N]
	melt_maintien[, px := 1-get(qxName)]
	separator = "__"
	melt_maintien
	if (missing(dimsNames)){
		melt_maintien[
			, dims := ""
		]
	} else {
		melt_maintien[
			, dims := do.call(paste, c(.SD, sep = separator))
			, .SDcols = dimsNames
			]
	}
	melt_maintien[, inc := get(incName)]
	melt_maintien[
		, key := do.call(paste, c(.SD, sep = separator))
		, .SDcols = c("dims", "inc")
		] # TODO: gerer le cas ou il n'y a pas de dims, juste une table normale
	if (anyDuplicated(melt_maintien$key)) warning("!! duplicated key")

	melt_maintien[
		order(dims, inc)
		, lxp1 := cumprod(px)
		, by = .(dims)
		]
	melt_maintien[, inc_p1 := inc+1]
	melt_maintien[
		, key_xp1 := do.call(paste, c(.SD, sep = separator))
		, .SDcols = c("dims", "inc_p1")
		]

	get_x_infos <- melt_maintien[, .(key = key_xp1, lx = lxp1)]
	melt_maintien_lx <- merge(
		melt_maintien
		, get_x_infos
		, by = c("key")
		, all.x = T
	)[order(dims, inc)]
	melt_maintien_lx[inc==min(inc), lx := 1]

	return(melt_maintien_lx[order(retain_order)]$lx)

}
