#===============================================================
# Function returns true/false based on whether a field has
# information characteristics that are optimal for matching.
# In general, optimal fields will have:
#
#   - non-zero and (even better) high entropy
#   - low missing (null) rate
#   - average frequency greater than 1
#
# Input:
#   df - data frame created by the "field_metrics" 
#        function
#   row - specific field to eval
#
# Output:
#   Records from the data frame that meet the heuristics
#===============================================================

is_good_match_field <- function(df, row) {

	if (df$NRt[[row]] < .90 & df$H[[row]] > 0.95 & df$Favg[[row]] > 1.01 & df$prs > 20)

		return(T)

	else
	
		return(F)

}

