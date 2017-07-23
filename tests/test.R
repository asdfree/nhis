if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
# examine all available NHIS microdata files
nhis_cat <-
	get_catalog( "nhis" ,
		output_dir = file.path( getwd() ) )

# 2015 only
nhis_cat <- subset( nhis_cat , year == 2015 )
# download the microdata to your local computer
stopifnot( nrow( nhis_cat ) > 0 )

options( survey.lonely.psu = "adjust" )

library(survey)
library(mitools)

nhis_personsx_df <- 
	readRDS( file.path( getwd() , "2015/personsx.rds" ) )

nhis_income_list <- 
	readRDS( file.path( getwd() , "2015/incmimp.rds" ) )

merge_variables <- c( "hhx" , "fmx" , "fpx" )

nhis_personsx_df[ merge_variables ] <- 
	sapply( nhis_personsx_df[ merge_variables ] , as.numeric )

inc_vars_to_keep <- 
	c( 
		merge_variables , 
		setdiff( 
			names( nhis_income_list[[ 1 ]] ) , 
			names( nhis_personsx_df )
		)
	)

# personsx variables to keep
vars_to_keep <- 
	c( merge_variables , "psu_p" , "strat_p" , "wtfa" ,
		'phstat' , 'sex' , 'hospno' , 'age_p' , 'hinotmyr' , 'notcov' )

nhis_personsx_df <- nhis_personsx_df[ vars_to_keep ]
	
nhis_personsx_list <-
	lapply( nhis_income_list ,
		function( w ){
			w <- w[ inc_vars_to_keep ]
			w[ merge_variables ] <- sapply( w[ merge_variables ] , as.numeric )
			result <- merge( nhis_personsx_df , w )
			stopifnot( nrow( result ) == nrow( nhis_personsx_df ) )
			result
		} )

# personsx design		
nhis_design <- 
	svydesign( 
		id = ~psu_p , 
		strata = ~strat_p ,
		nest = TRUE ,
		weights = ~wtfa ,
		data = imputationList( nhis_personsx_list )
	)

rm( nhis_personsx_list ) ; gc()

nhis_samadult_df <- 
	readRDS( file.path( getwd() , "2015/samadult.rds" ) )

nhis_samadult_df[ merge_variables ] <- 
	sapply( nhis_samadult_df[ merge_variables ] , as.numeric )

samadult_vars_to_keep <- 
	c( 
		merge_variables , 
		setdiff( 
			names( nhis_samadult_df ) , 
			names( nhis_personsx_df ) 
		) 
	)

nhis_personsx_samadult_df <-
	merge( nhis_personsx_df , nhis_samadult_df[ samadult_vars_to_keep ] )

stopifnot( nrow( nhis_personsx_samadult_df ) == nrow( nhis_samadult_df ) )

rm( nhis_personsx_df , nhis_samadult_df ) ; gc()

nhis_samadult_list <-
	lapply( nhis_income_list ,
		function( w ){
			w <- w[ inc_vars_to_keep ]
			w[ merge_variables ] <- sapply( w[ merge_variables ] , as.numeric )
			result <- merge( nhis_personsx_samadult_df , w )
			stopifnot( nrow( result ) == nrow( nhis_personsx_samadult_df ) )
			result
		} )

rm( nhis_income_list , nhis_personsx_samadult_df ) ; gc()

# sample adult design (commented out)
# nhis_samadult_design <- 
	# svydesign( 
		# id = ~psu_p , 
		# strata = ~strat_p ,
		# nest = TRUE ,
		# weights = ~wtfa_sa ,
		# data = imputationList( nhis_samadult_list )
	# )
	
rm( nhis_samadult_list ) ; gc()
nhis_design <- 
	update( 
		nhis_design , 
		
		one = 1 ,
		
		poverty_category =
			factor( 
				findInterval( povrati3 , 1:4 ) ,
				labels = 
					c( "below poverty" , "100-199%" , "200-299%" , "300-399%" , "400%+" )
			) ,
			
		fair_or_poor_reported_health = 
			ifelse( phstat %in% 1:5 , as.numeric( phstat >= 4 ) , NA ) ,
			
		sex = factor( sex , labels = c( "male" , "female" ) ) ,
		
		hospno = ifelse( hospno > 366 , NA , hospno )

	)
MIcombine( with( nhis_design , svyby( ~ one , ~ one , unwtd.count ) ) )

MIcombine( with( nhis_design , svyby( ~ one , ~ poverty_category , unwtd.count ) ) )
MIcombine( with( nhis_design , svytotal( ~ one ) ) )

MIcombine( with( nhis_design ,
	svyby( ~ one , ~ poverty_category , svytotal )
) )
MIcombine( with( nhis_design , svymean( ~ age_p ) ) )

MIcombine( with( nhis_design ,
	svyby( ~ age_p , ~ poverty_category , svymean )
) )
MIcombine( with( nhis_design , svymean( ~ sex ) ) )

MIcombine( with( nhis_design ,
	svyby( ~ sex , ~ poverty_category , svymean )
) )
MIcombine( with( nhis_design , svytotal( ~ age_p ) ) )

MIcombine( with( nhis_design ,
	svyby( ~ age_p , ~ poverty_category , svytotal )
) )
MIcombine( with( nhis_design , svytotal( ~ sex ) ) )

MIcombine( with( nhis_design ,
	svyby( ~ sex , ~ poverty_category , svytotal )
) )
MIcombine( with( nhis_design , svyquantile( ~ age_p , 0.5 , se = TRUE ) ) )

MIcombine( with( nhis_design ,
	svyby( 
		~ age_p , ~ poverty_category , svyquantile , 0.5 ,
		se = TRUE , keep.var = TRUE , ci = TRUE 
) ) )
MIcombine( with( nhis_design ,
	svyratio( numerator = ~ hinotmyr , denominator = ~ hospno , na.rm = TRUE )
) )
sub_nhis_design <- subset( nhis_design , notcov == 1 )
MIcombine( with( sub_nhis_design , svymean( ~ age_p ) ) )
this_result <-
	MIcombine( with( nhis_design ,
		svymean( ~ age_p )
	) )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	MIcombine( with( nhis_design ,
		svyby( ~ age_p , ~ poverty_category , svymean )
	) )

coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nhis_design$designs[[1]] )
MIcombine( with( nhis_design , svyvar( ~ age_p ) ) )
# SRS without replacement
MIcombine( with( nhis_design ,
	svymean( ~ age_p , deff = TRUE )
) )

# SRS with replacement
MIcombine( with( nhis_design ,
	svymean( ~ age_p , deff = "replace" )
) )
lodown:::MIsvyciprop( ~ fair_or_poor_reported_health , nhis_design ,
	method = "likelihood" , na.rm = TRUE )
lodown:::MIsvyttest( age_p ~ fair_or_poor_reported_health , nhis_design )
lodown:::MIsvychisq( ~ fair_or_poor_reported_health + sex , nhis_design )
glm_result <- 
	MIcombine( with( nhis_design ,
		svyglm( age_p ~ fair_or_poor_reported_health + sex )
	) )
	
summary( glm_result )

