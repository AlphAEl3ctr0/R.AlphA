#' regler la luminosité de la fenetre graphique
#' @export
#' @param lum luminosity from 0 to 100
#' @importFrom grDevices rgb
#' @importFrom graphics par plot
#' @import ggplot2

lum_0_100 <- function(lum){
	# lum <- 40
	# for the plot() function
	lum_pc <- lum/100; par(bg = rgb(lum_pc, lum_pc, lum_pc)) ; plot(1)

	# for ggplot()
	lum_pc_leg <- lum/100
	hex_leg <- rgb(lum_pc_leg,lum_pc_leg,lum_pc_leg)
	dark_adjustments <- theme(
		plot.background = element_rect(fill = ret_lum(hex_leg,0.7))
		, legend.background = element_rect(fill = hex_leg)
		, panel.background = element_rect(fill = hex_leg)
		, panel.grid.major = element_line(size = rel(0.5), color = ret_lum(hex_leg, 0.7))
		, panel.grid.minor = element_line(size = rel(0.25), color = ret_lum(hex_leg, 0.7))
		, axis.text = element_text(color = ret_lum(hex_leg,0.4))
		, legend.key = element_rect(fill = ret_lum(hex_leg,0.8))
	)
	theme_set(theme_get()+dark_adjustments)
	ggplot()
}


