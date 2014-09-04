#===============================================================
# Function returns fields that have information characteristics 
# optimal for matching. In general, optimal fields will have:
#
#   - non-zero and (even better) high entropy
#   - low missing (null) rate
#   - average frequency greater than 1
#
# Input:
#   df - data frame created by the "field_metrics" 
#        function
#
# Output:
#   Records from the data frame that meet the heuristics
#===============================================================

good_match_fields <- function(df) {

	subset(df, (NRt < .90 & H > 1.0 & Favg > 1.01 ) ) 

}

