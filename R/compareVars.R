#' @title Compare tables variables
#' @description reads names matching a given pattern in two tables, and gives
#' information about which columns are in both tables, only in x/y and so on
#' @param x first table
#' @param y second table
#' @param pattern to compare only a subset of all the varibles
#' @return an object containing :
#' all : all the column names
#' common : names found in both tables
#' onlyX : only in the first table
#' onlyY : obvious
#' exclusive : columns found in only one of the two tables
#' @export
#'
compareVars <- function(x, y, pattern = ""){
	result <- list()
	xVars <- grep(names(x), pattern = pattern, value = T)
	yVars <- grep(names(y), pattern = pattern, value = T)
	result$xVars <- xVars
	result$yVars <- yVars
	result$all <- union(xVars, yVars)
	result$common <- intersect(xVars, yVars)
	result$onlyX <- setdiff(xVars, yVars)
	result$onlyY <- setdiff(yVars, xVars)
	result$exclusive <- union(result$onlyX, result$onlyY)
	return(result)
}



