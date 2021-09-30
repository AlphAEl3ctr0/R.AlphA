#' regler la luminosité de la fenetre graphique
#' @export
#' @param lum luminosity from 0 to 100
#' @importFrom grDevices rgb
#' @importFrom graphics par plot
#'

lum_0_100 <- function(lum){
	lum_pc <- lum/100; par(bg = rgb(lum_pc, lum_pc, lum_pc)) ; plot(1)
}
