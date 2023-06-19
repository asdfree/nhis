# excellent health poor
# wealth. "sup, doc?" bugs, daft bills, free
# laughs best medicine
nhis_csv_import <-
	function( this_url ){
		
		this_tf <- tempfile()
		
		download.file( this_url , this_tf , mode = 'wb' )
		
		unzipped_files <- unzip( this_tf , exdir = tempdir() )
		
		this_csv <- grep( '\\.csv$' , unzipped_files , value = TRUE )
		
		this_df <- read.csv( this_csv )
		
		file.remove( c( this_tf , unzipped_files ) )
		
		names( this_df ) <- tolower( names( this_df ) )
		
		this_df
	}

nhis_df <-
	nhis_csv_import( 
		"https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2021/adult21csv.zip" 
	)

imputed_income_df <- 
	nhis_csv_import( 
		"https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2021/adultinc21csv.zip" 
	)
# nhis_fn <- file.path( path.expand( "~" ) , "NHIS" , "this_file.rds" )
# saveRDS( nhis_df , file = nhis_fn , compress = FALSE )
# nhis_df <- readRDS( nhis_fn )
imputed_income_list <- split( imputed_income_df , imputed_income_df[ , 'impnum_a' ] )
variables_to_remove <-
	setdiff( intersect( names( nhis_df ) , names( imputed_income_df ) ) , 'hhx' )

nhis_df <- nhis_df[ , !( names( nhis_df ) %in% variables_to_remove ) ]
nhis_list <-
	lapply( imputed_income_list ,
		function( w ){
			this_df <- merge( nhis_df , w )
			stopifnot( nrow( this_df ) == nrow( nhis_df ) )
			this_df
		} )
library(survey)
library(mitools)

nhis_design <- 
	svydesign( 
		id = ~ ppsu , 
		strata = ~ pstrat ,
		nest = TRUE ,
		weights = ~ wtfa_a ,
		data = imputationList( nhis_list )
	)
nhis_design <- 
	update( 
		nhis_design , 
		
		one = 1 ,
		
		poverty_category =
			factor( 
				findInterval( povrattc_a , c( 1 , 2 , 4 ) ) ,
				labels = 
					c( "below poverty" , "100-199%" , "200-399%" , "400%+" )
			) ,
			
		fair_or_poor_reported_health = 
			ifelse( phstat_a %in% 1:5 , as.numeric( phstat_a >= 4 ) , NA ) ,
			
		sex_a = factor( sex_a , levels = 1:2 , labels = c( "male" , "female" ) ) ,
		
		annual_premium_first_plan = ifelse( hicostr1_a > 40000 , NA , hicostr1_a )

	)
MIcombine( with( nhis_design , svyby( ~ one , ~ one , unwtd.count ) ) )

MIcombine( with( nhis_design , svyby( ~ one , ~ poverty_category , unwtd.count ) ) )
MIcombine( with( nhis_design , svytotal( ~ one ) ) )

MIcombine( with( nhis_design ,
	svyby( ~ one , ~ poverty_category , svytotal )
) )
MIcombine( with( nhis_design , svymean( ~ agep_a ) ) )

MIcombine( with( nhis_design ,
	svyby( ~ agep_a , ~ poverty_category , svymean )
) )
MIcombine( with( nhis_design , svymean( ~ sex_a ) ) )

MIcombine( with( nhis_design ,
	svyby( ~ sex_a , ~ poverty_category , svymean )
) )
MIcombine( with( nhis_design , svytotal( ~ agep_a ) ) )

MIcombine( with( nhis_design ,
	svyby( ~ agep_a , ~ poverty_category , svytotal )
) )
MIcombine( with( nhis_design , svytotal( ~ sex_a ) ) )

MIcombine( with( nhis_design ,
	svyby( ~ sex_a , ~ poverty_category , svytotal )
) )
MIcombine( with( nhis_design ,
	svyquantile(
		~ agep_a ,
		0.5 , se = TRUE 
) ) )

MIcombine( with( nhis_design ,
	svyby(
		~ agep_a , ~ poverty_category , svyquantile ,
		0.5 , se = TRUE ,
		ci = TRUE 
) ) )
MIcombine( with( nhis_design ,
	svyratio( numerator = ~ annual_premium_first_plan , denominator = ~ agep_a , na.rm = TRUE )
) )
sub_nhis_design <- subset( nhis_design , notcov_a == 1 )
MIcombine( with( sub_nhis_design , svymean( ~ agep_a ) ) )
this_result <-
	MIcombine( with( nhis_design ,
		svymean( ~ agep_a )
	) )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	MIcombine( with( nhis_design ,
		svyby( ~ agep_a , ~ poverty_category , svymean )
	) )

coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nhis_design$designs[[1]] )
MIcombine( with( nhis_design , svyvar( ~ agep_a ) ) )
# SRS without replacement
MIcombine( with( nhis_design ,
	svymean( ~ agep_a , deff = TRUE )
) )

# SRS with replacement
MIcombine( with( nhis_design ,
	svymean( ~ agep_a , deff = "replace" )
) )
# MIsvyciprop( ~ fair_or_poor_reported_health , nhis_design ,
# 	method = "likelihood" , na.rm = TRUE )
# MIsvyttest( agep_a ~ fair_or_poor_reported_health , nhis_design )
# MIsvychisq( ~ fair_or_poor_reported_health + sex_a , nhis_design )
glm_result <- 
	MIcombine( with( nhis_design ,
		svyglm( agep_a ~ fair_or_poor_reported_health + sex_a )
	) )
	
summary( glm_result )
results <-
	MIcombine( 
		with(
			subset( nhis_design , agep_a < 65 ) , 
			svyby(
				~ as.numeric( rxsk12m_a == 1 | rxls12m_a == 1 | rxdl12m_a == 1 ) , 
				~ poverty_category , 
				svymean , 
				na.rm = TRUE 
			) 
		)
	)

stopifnot(
	all(
		as.numeric( round( coef( results ) , 3 ) ) == c( 0.145 , 0.138 , 0.099 , 0.039 )
	) 
)

stopifnot(
	all( 
		as.numeric( round( SE( results ) , 5 ) ) - c( 0.0126 , 0.0098 , 0.0062 , 0.0031 ) < 0.0001
	) 
)

