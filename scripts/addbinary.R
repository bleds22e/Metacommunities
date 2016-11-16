## R function for converting an abundance table(x) into its additive binary form.
## by David Nipperess, Macquarie University, Australia (dnippere@bio.mq.edu.au)
## Last edited 24th July 2009.

##  Copyright (C) 2008  David Nipperess

##  This program is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  any later version.

##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License <http://www.gnu.org/licenses/> for more details.

## -----------------

addbinary <- function(x) {
	
# where x is a community data table (as in the vegan package) with species/OTUs as columns and samples/sites as rows. Columns are labelled with the names of the species/OTUs. Rows are labelled with the names of the samples/sites.

add_bin_matrix = NULL

for(i in 1:length(x[1,])) {
	max_abund <- max(x[,i])
	submatrix <- matrix(data=0, nrow=length(x[,1]), ncol=max_abund)
	for(j in 1:length(x[,1])) {
		abund <- x[j,i]
		if(abund>0) {
			for(k in 1:abund) {
				submatrix[j,k]=1
				}
			}
		}
	add_bin_matrix <- cbind(add_bin_matrix,submatrix)
	}
rownames(add_bin_matrix) <- rownames(x)
return(add_bin_matrix)
}