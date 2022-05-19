#' @title fonction de sauvegarde dans un dossier old
#' @description This function saves the project with date in its name in an "old" dir
#' @return nothing, only saves
#' @export

save_in_old <- function(){
	requireNamespace("rstudioapi")
	requireNamespace("stringr")
	sav_filepath <- rstudioapi::getSourceEditorContext()$path #chemin complet fichier
	sav_filename <- stringr::str_remove(sav_filepath, ".*/") #uniquement le nom de fichier
	sav_dirname <- dirname(sav_filepath) # nom du dossier
	sav_olddirname <- file.path(sav_dirname, "old")
	if(!dir.exists(sav_olddirname)) {
		print(paste0("creating old dir : ", sav_olddirname))
		dir.create(sav_olddirname)
	}
	# sav_savename <- paste0(Sys.time(), " ", sav_filename)
	sav_savename <- paste0(Sys.Date(), " ", sav_filename)
	sav_savepath <- file.path(sav_olddirname, sav_savename)
	if (!file.exists(sav_savepath))  {#si n'existe pas déjà : ok on sauvegarde
		file.copy(from = sav_filepath, to = sav_savepath)
		print(paste0("file saved under : ", sav_savepath))
	} else {
		print(paste0("file already exists (remove first) : ", sav_savepath))
	}
}
