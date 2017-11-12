if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

nhis_cat <-
	get_catalog( "nhis" ,
		output_dir = file.path( getwd() ) )

record_categories <- ceiling( seq( nrow( nhis_cat ) ) / ceiling( nrow( nhis_cat ) / 4 ) )

nhis_cat <- unique( rbind( nhis_cat[ record_categories == this_sample_break , ] , nhis_cat[ nhis_cat$year == 2016 , ] ) )

lodown( "nhis" , nhis_cat )
