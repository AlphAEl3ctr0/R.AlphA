#' @title fonction pour importer plusieurs fichiers et les concatener dans une meme table
#' @description This function imports all files selected, concatenates them in a single table and adds an fName variable
#' @param path argument passed to list.fles
#' @param pattern argument passed to list.fles
#' @param ignore.case argument passed to list.fles
#' @param importFunction if you know which function you want to use
#' @param fill passed to rbind
#' @param fileList instead of a Pattern
#' @return the concatenated table
#' @importFrom openxlsx read.xlsx
#' @export

importAll <- function(
	path = "."
	, pattern = ""
	, ignore.case = FALSE
	, importFunction = NULL
	, fill = F
	, fileList = NULL
){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		getLibsR.AlphA()
		message("! parameters manually defined inside function for tests. Do not use results !")
		root <- dirname(rstudioapi::getSourceEditorContext()$path)
		workr_root <- sub("WorkR.*", "WorkR", root)
		setwd(root)
		path = file.path(workr_root, "Tests_xlsx")
		pattern = "impall"
		ignore.case = TRUE
		importFunction = openxlsx::read.xlsx
		# lire des rds ?
		path <- workr_root %>%
			file.path(
				"Datacamp"
				, "Competitions"
				, "Abalone"
				, "Results"
				, "datas"
			# ) %>% list.files()
			) %>% print
		pattern <- "seed_99(1)*\\.rds"
		importFunction <- NULL
		fileList <- c(
			NULL
			, "/Users/Raphael/Google Drive/WorkR/Datacamp/Competitions/Abalone/Results/datas/compareModels_seed_99.rds"
			, "/Users/Raphael/Google Drive/WorkR/Datacamp/Competitions/Abalone/Results/datas/compareModels_seed_991.rds"
		)
	}

	if (missing(fileList)) {
		# with a pattern
		filePaths <- data.table(
			NULL
			, locPath = list.files(path = path, pattern = pattern, ignore.case = ignore.case, full.names = F)
			, fulPath = list.files(path = path, pattern = pattern, ignore.case = ignore.case, full.names = T)
		)
	} else {
		# with a file list
		filePaths <- data.table(fulPath = fileList) %>%
			mutate(
				locPath = fulPath %>%
					stringr::str_remove(dirname(fulPath)) %>%
					stringr::str_remove("^/")
			) %>%
			as.data.table
	}
	# choosing import function depending on extensions
	if (missing(importFunction)) {
		if(manualrun) print ("importFunction missing")
		filePaths[, ext := gsub(".*\\.", "", locPath)]
		importFunsList <- do.call(rbind,list(NULL
			, data.table(ext = "xlsx"	, fun = function(x) as.data.table(openxlsx::read.xlsx(x)))
			, data.table(ext = "csv"	, fun = fread)
			, data.table(ext = "rds"	, fun = readRDS)
		))
		filePaths <- merge(
			filePaths, importFunsList
			, by = "ext"
		)
	} else {
		if(manualrun) print ("importFunction provided")
		testnames <- names(filePaths)
		# filePaths[, .(fun = importFunction), by = .(locPath, fulPath)]
		filePaths[, cst := T]
		importFunsList <- do.call(rbind,list(NULL
			, data.table(cst = T, fun = importFunction)
		))
		filePaths <- merge(
			filePaths, importFunsList
			, by = "cst"
		)[, cst := NULL]
	}
	if (length(unique(filePaths$fun))>1) message("more than 1 type of file : it is very casse gueule (might face problems with column types)")
	importsList <- mapply(
		# FUN = function(ful_path, loc_path) importFuntion(ful_path)[, test := loc_path]
		FUN = function(ful_path, loc_path, importFunction){
			import <- importFunction(ful_path) %>% as.data.table
			import[, fName := loc_path]
		}
		, ful_path = filePaths$fulPath
		, loc_path = filePaths$locPath
		, importFunction = filePaths$fun
		, SIMPLIFY = F
	)
	concatenation <- do.call(
		function(...) rbind(..., fill = fill)
		, importsList
	)
}

# root <- dirname(rstudioapi::getSourceEditorContext()$path)
# workr_root <- sub("WorkR.*", "WorkR", root)
# list.files(file.path(workr_root, "Tests_xlsx"))
# setwd(root)
# a <- importAll(
# 	path = file.path(workr_root, "Tests_xlsx")
# 	# , pattern = "impall.*csv"
# 	, pattern = "impall"
# 	, ignore.case = T
# )
#
# testfun<- as.matrix(fread)
# getwd()
# # for (fSuffix in c(1,10,5,20,30,200)){
# # 	# fSuffix <- 10
# # 	write.csv(
# # 		data.table(
# # 			suffix = fSuffix
# # 			, b = "slfj"
# # 			, c = ISOdate(2020,1,1)
# # 			, d = runif(n = 10, min = 1*fSuffix, max = 10*fSuffix)
# # 		)
# # 		, file = file.path(workr_root, "Tests_xlsx", paste0("testImpAll_", fSuffix, ".csv"))
# # 		, row.names = FALSE
# # 	)
# # }
