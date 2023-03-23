#' @title fonction de sauvegarde dans un dossier old
#' @description This function saves the project with date in its name in an "old" dir
#' @return nothing, only saves
#' @export
#' @import ggplot2

save_in_old <- function(sav_filepath = NULL, sav_fileNote = NULL){
	requireNamespace("rstudioapi")
	requireNamespace("stringr")
	requireNamespace("tools")
	if (is.null(sav_filepath)) {
		sav_filepath <- rstudioapi::getSourceEditorContext()$path #chemin complet fichier
	}
	sav_filename <- sav_filepath %>% basename #uniquement le nom de fichier
	# sav_filename <- stringr::str_remove(sav_filepath, ".*/") #uniquement le nom de fichier
	sav_dirname <- dirname(sav_filepath) # nom du dossier
	sav_olddirname <- file.path(sav_dirname, "old")
	if(!dir.exists(sav_olddirname)) {
		print(paste0("creating old dir : ", sav_olddirname))
		dir.create(sav_olddirname)
	}

	# ajout note apres nom de fichier
	if (!is.null(sav_fileNote)) {
		sav_fileNameNoExt <- sav_filename %>% file_path_sans_ext # flexion
		sav_fileExt <- sav_filename %>% file_ext # extension
		sav_filename <- paste0(
			sav_fileNameNoExt, " (", sav_fileNote, ")", ".", sav_fileExt
		)
	}


	sav_savename <- paste0(Sys.Date(), " ", sav_filename)
	sav_savepath <- file.path(sav_olddirname, sav_savename)
	if (!file.exists(sav_savepath))  {#si n'existe pas déjà : ok on sauvegarde
		file.copy(from = sav_filepath, to = sav_savepath)
		print(paste0("file saved under : ", sav_savepath))
	} else {
		print(paste0("file already exists (remove first) : ", sav_savepath))
	}
}
# library(R.AlphA)
# list.files(root())
# save_in_old(file.choose())
# save_in_old(sav_fileNote = "test")
#
# filePath <- list.files("R", pattern = "R$", full.names = T)[1]
# file.info("R/addDim.R")
# file.access("R/addDim.R")
# file.choose()
# exFile <- "add.Di.m.R"
# exFile <- "/Users/Raphael/Google Drive/WorkR/R.AlphA/R/add.Di.m.R"
# library(tools)
# exFile %>%
# 	basename %>%
# 	file_path_sans_ext %>%
# 	identity
# exFile
#
# exDir <- dirname(exFile)
# exName <- exFile %>% basename
#
# exNameNoExt <- exName %>% file_path_sans_ext
# exExt <- exFile %>% file_ext
# paste0(
# 	exNameNoExt, " (", "note", ")", ".", exExt
# )
# exOldName <- paste0(Sys.Date(), " ", exName)
# oldDir <- file.path(exDir, "old")
# dir.exists(oldDir)
# exOldPath <- file.path(exDir, "old", exOldName)
# list.files(exDir)
#
# library(stringr)
# exFile %>% str_remove(".*/") %>% str_extract("\\..*")
#
# file.path()
#
# file.info()
# file.access()
# test <- rstudioapi::getSourceEditorContext()
# test$selection
