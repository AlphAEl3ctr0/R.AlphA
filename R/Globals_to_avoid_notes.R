# This is only to avoid "no visible binding" notes in package check. Source solution :
	# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887/3
	# "hideous hack" according to https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
	"."
	, "lx"
	, "age"
	, "px"
	, "qx"
	, "Dx"
	, "Nx"
	, "a_pp_x"
	, "ax"
	, "sexe"
	, "age_souscription"
	, "dteNais_dec"
	, "dteSouscription_dec"
	, "dteNais"
	, "dteSouscription"
	, "..var_pop"
	, "..variables_table_commut"
	, "dteVision"
	, "random"
	, "age_vis"
	, "dteNais"
	, "excess_of_age"
	, "canceled"
	, "..popvars"
	, "dtemax"
	, "dtemin"
	, "int_get_commut_key"
	, "type"
	, "inc"
	, "dims"
	, "key_names"
	, "inc_p1"
	, "key_xp1"
	, "lxp1"
	, "inc"
	, "qx_pres"
	, "qx.x"
	, "qx.y"
	, "lx_pres"
	, "dim_age_dep"
	, "cst"
	, "tx"
	, "p_survie"
	, "lx_age_vis"
	, "VAP_dep"
	, "p_dep"
	, "dteVision_dec"
	, "age_vision"
	, ".variables_table_pop"
	, "..commutVars"
	, "result"
	, "ext"
	, "locPath"
	, "cst"
	, "fName"
	, "retain_order"
	, "dims"
	, "inc"
	, "lxp1"
	, "inc_p1"
	, "key_xp1"
	))

#
# Complete_commut:   no visible binding for global variable ‘ax’
# generate_pop:      no visible binding for global variable ‘sexe’
# generate_pop:      no visible binding for global variable ‘age_souscription’
# generate_pop:      no visible binding for global variable ‘dteNais_dec’
# generate_pop:      no visible binding for global variable ‘dteSouscription_dec’
# generate_pop:      no visible binding for global variable ‘dteNais’
# generate_pop:      no visible binding for global variable ‘dteSouscription’
# generate_pop:      no visible binding for global variable ‘..var_pop’
# get_commut_vars:   no visible binding for global variable ‘..variables_table_commut’
# inc_pop:           no visible binding for global variable ‘dteVision’
# inc_pop:           no visible binding for global variable ‘random’
# inc_pop:           no visible binding for global variable ‘age_vis’
# inc_pop:           no visible binding for global variable ‘dteNais’
# inc_pop:           no visible binding for global variable ‘excess_of_age’
# inc_pop:           no visible binding for global variable ‘canceled’
# inc_pop:           no visible binding for global variable ‘..popvars’
# rdate:             no visible binding for global variable ‘dtemax’
# rdate:             no visible binding for global variable ‘dtemin’
# inc_pop:           no visible global function definition for ‘str_c’# celle la reste a voir




# 2021.11.02 : ajout nouvelles vars
# Complete_commut:     no visible binding for global variable ‘type’
# Complete_commut:     no visible binding for global variable ‘inc’
# Complete_commut:     no visible binding for global variable ‘dims’
# Complete_commut:     no visible binding for global variable ‘key_names’
# Complete_commut:     no visible binding for global variable ‘inc_p1’
# Complete_commut:     no visible binding for global variable ‘key_xp1’
# Complete_commut:     no visible binding for global variable ‘lxp1’
# dep_table:           no visible binding for global variable ‘inc’
# dep_table:           no visible binding for global variable ‘qx_pres’
# dep_table:           no visible binding for global variable ‘qx.x’
# dep_table:           no visible binding for global variable ‘qx.y’
# dep_table:           no visible binding for global variable ‘lx_pres’
# dep_table:           no visible binding for global variable ‘dim_age_dep’
# dep_table:           no visible binding for global variable ‘cst’
# dep_table:           no visible binding for global variable ‘tx’
# dep_table:           no visible binding for global variable ‘p_survie’
# dep_table:           no visible binding for global variable ‘lx_age_vis’
# dep_table:           no visible binding for global variable ‘VAP_dep’
# dep_table:           no visible binding for global variable ‘p_dep’
# generate_pop:        no visible binding for global variable ‘dteVision_dec’
# generate_pop:        no visible binding for global variable ‘age_vision’
# get_commut_vars:     no visible binding for global variable‘..variables_table_pop’
# get_commut_vars:     no visible binding for global variable ‘..commutVars’
# get_commut_vars:     no visible binding for global variable ‘result’
# importAll:           no visible binding for global variable ‘ext’
# importAll:           no visible binding for global variable ‘locPath’
# importAll:           no visible binding for global variable ‘cst’
# importAll :     	:  no visible binding for global variable ‘fName’
# qx_to_lx:            no visible binding for global variable ‘retain_order’
# qx_to_lx:            no visible binding for global variable ‘dims’
# qx_to_lx:            no visible binding for global variable ‘inc’
# qx_to_lx:            no visible binding for global variable ‘lxp1’
# qx_to_lx:            no visible binding for global variable ‘inc_p1’
# qx_to_lx:            no visible binding for global variable ‘key_xp1’
# importAll :          <anonymous>: no visible binding for global variable ‘fName’



# Undefined global functions or variables:..commutVars ..variables_table_pop VAP_dep age_vision cst dim_age_dep
# dims dteVision_dec ext fName inc inc_p1 key_names key_xp1 locPath
# lx_age_vis lx_pres lxp1 p_dep p_survie qx.x qx.y qx_pres result
# retain_order tx type

