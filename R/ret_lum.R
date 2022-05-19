#' change luminosity of a hex color by multiplying rgb components by a factor
#' @export
#' @param hexCol color to retreat, in hexaDecimal format
#' @param rgbFact factor to multiply rgb colors with. if > 1, check final value
#' @importFrom grDevices rgb
#' @importFrom grDevices col2rgb
#'


ret_lum <- function(hexCol, rgbFact){
	# hexCol <- rgb(1,1,1)
	# rgbFact <- 0.7
	back2rgb <- grDevices::col2rgb(hexCol) / 255
	retLum <- back2rgb*rgbFact
	back2Hex <- grDevices::rgb(retLum[1], retLum[2], retLum[3])
	return(back2Hex)
}
