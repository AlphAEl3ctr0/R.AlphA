Complete_commut_vprec <- function(lxTable, i){
	tableToFill <- as.data.table(copy(lxTable))
	if(missing(i)) {message("i is missing : set to 0 by default"); i <- 0}
	tableToFill[is.na(lx), lx := 0] # retraitement
	ages <- tableToFill$age
	ages_but_last <- min(ages):max(ages)-1
	for(i_age in ages_but_last){
		tableToFill[age == i_age, px := tableToFill[age == i_age+1]$lx / lx]	# px
	}
	
	v <- 1/(1+i)
	tableToFill[, qx := 1 - px]
	tableToFill[, Dx := lx * v^age]
	
	for (i_age in ages) {
		tableToFill[age == i_age, Nx := sum(tableToFill[age >= i_age, Dx])]		# Nx
	}
	# for (i_age in ages_but_last) {
	# 	tableToFill[age == i_age, ax := tableToFill[age == i_age+1, Nx] / Dx]	# ax
	# }
	tableToFill[, a_pp_x := Nx/Dx] # trouver une vraie notation				# äx
	tableToFill[, ax := a_pp_x - 1] # plus simple qu'une boucle				# ax
}

