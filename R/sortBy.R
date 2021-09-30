#' @title Sort a Data Frame
#' @description This function accepts a df and col number to sort the df by that column
#' @param df The df to be sorted
#' @param col The column name (string) by which to sort the df
#' @param desc self explained, default FALSE
#' @return The sorted df
#' @source https://towardsdatascience.com/creating-a-custom-r-package-9a2e303d3332
#' @examples
#' subject <- c(1:10)
#' height <- round(rnorm(10, mean = 66, sd = 3), 1)
#' d <- data.frame(subject, height)
#'
#' d2 <- sortby(d, "height")
#' d2
#'
#' d3 <- sortby(d, "height", desc = TRUE)
#' d3
#'
#'
#' @export

sortby <- function(df, col, desc = FALSE) {
	#### df is the dataframe to be sorted
	#### col is the variable by which it should be sorted
	#### desc indicates whether the sort order should be descending
	#### str2expression() allows us to dynamically create a df$col
	####     variable which gets evaluated by eval()
	df[order(
		eval(
			str2expression(paste0("df$", col))
		),
		decreasing = desc
	),]
}
