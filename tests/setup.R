if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)

nhis_cat <-
	get_catalog( "nhis" ,
		output_dir = file.path( getwd() ) )

# sample 40% of the records
which_records <- sample( seq( nrow( nhis_cat ) ) , round( nrow( nhis_cat ) * 0.40 ) )

# always sample year == 2016
nhis_cat <- unique( rbind( nhis_cat[ which_records , ] , subset( nhis_cat , year == 2016 ) ) )

lodown( "nhis" , nhis_cat )
