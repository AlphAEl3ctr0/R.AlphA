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
