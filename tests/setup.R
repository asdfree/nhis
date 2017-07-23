if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)

nhis_cat <-
	get_catalog( "nhis" ,
		output_dir = file.path( getwd() ) )

# sample 50% of the records
which_records <- sample( seq( nrow( nhis_cat ) ) , round( nrow( nhis_cat ) * 0.25 ) )

# always sample year == 2015
nhis_cat <- unique( rbind( nhis_cat[ which_records , ] , subset( nhis_cat , year == 2015 ) ) )

lodown( "nhis" , nhis_cat )
