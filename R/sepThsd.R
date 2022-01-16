#' Quick formatting
#' @description wrapper of format function with custom defaults
#' @param big.mark separator for thousands
#' @param digits number of digits
#' @param scientific boolean
#' @export

sepThsd <- function(x, big.mark = " ", digits = 0, scientific = FALSE){
	formatted <- format(
		x
		, big.mark = big.mark
		, digits = digits
		, scientific = scientific
	)
	return(formatted)
}
