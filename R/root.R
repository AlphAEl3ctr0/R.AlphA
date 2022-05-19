# rm(list = ls())
# fonction de base pour avoir le dossier ds lequel se trouve le code --------
#' @title root directory of current source
#' @description root : dirname of the source's location
# #' @importFrom stats runif
#' @export

root <- function(){
	sourceLoc <- rstudioapi::getSourceEditorContext()$path
	rootDirname <- dirname(sourceLoc)
	return(rootDirname)
}
