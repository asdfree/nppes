if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
lodown( "nppes" , output_dir = file.path( getwd() ) )
npi_filepath <-
	grep(
		"npidata_pfile_20050523-([0-9]+)\\.csv" ,
		list.files(
			file.path( getwd() ) ,
			full.names = TRUE
		) ,
		value = TRUE
	)
	
column_names <-
	names( 
		read.csv( 
			npi_filepath , 
			nrow = 1 )[ FALSE , , ] 
	)

column_names <- gsub( "\\." , "_" , tolower( column_names ) )

column_types <-
	ifelse( 
		grepl( "code" , column_names ) & 
		!grepl( "country|state|gender|taxonomy|postal" , column_names ) , 
		'n' , 'c' 
	)

columns_to_import <-
	c( "entity_type_code" , "provider_gender_code" , "provider_enumeration_date" ,
	"is_sole_proprietor" , "provider_business_practice_location_address_state_name" )

stopifnot( all( columns_to_import %in% column_names ) )

# readr::read_csv() columns must match their order in the csv file
columns_to_import <-
	columns_to_import[ order( match( columns_to_import , column_names ) ) ]

nppes_df <- 
	data.frame( 
		readr::read_csv( 
			npi_filepath , 
			col_names = columns_to_import , 
			col_types = 
				paste0( 
					ifelse( column_names %in% columns_to_import , column_types , '_' ) , 
					collapse = "" 
				) ,
			skip = 1
		) 
	)

nppes_df <- 
	transform( 
		nppes_df , 
		
		individual = as.numeric( entity_type_code ) ,
		
		provider_enumeration_year = as.numeric( substr( provider_enumeration_date , 7 , 10 ) )
		
	)
nrow( nppes_df )

table( nppes_df[ , "provider_gender_code" ] , useNA = "always" )
mean( nppes_df[ , "provider_enumeration_year" ] , na.rm = TRUE )

tapply(
	nppes_df[ , "provider_enumeration_year" ] ,
	nppes_df[ , "provider_gender_code" ] ,
	mean ,
	na.rm = TRUE 
)
prop.table( table( nppes_df[ , "is_sole_proprietor" ] ) )

prop.table(
	table( nppes_df[ , c( "is_sole_proprietor" , "provider_gender_code" ) ] ) ,
	margin = 2
)
sum( nppes_df[ , "provider_enumeration_year" ] , na.rm = TRUE )

tapply(
	nppes_df[ , "provider_enumeration_year" ] ,
	nppes_df[ , "provider_gender_code" ] ,
	sum ,
	na.rm = TRUE 
)
quantile( nppes_df[ , "provider_enumeration_year" ] , 0.5 , na.rm = TRUE )

tapply(
	nppes_df[ , "provider_enumeration_year" ] ,
	nppes_df[ , "provider_gender_code" ] ,
	quantile ,
	0.5 ,
	na.rm = TRUE 
)
sub_nppes_df <- subset( nppes_df , provider_business_practice_location_address_state_name = 'CA' )
mean( sub_nppes_df[ , "provider_enumeration_year" ] , na.rm = TRUE )
var( nppes_df[ , "provider_enumeration_year" ] , na.rm = TRUE )

tapply(
	nppes_df[ , "provider_enumeration_year" ] ,
	nppes_df[ , "provider_gender_code" ] ,
	var ,
	na.rm = TRUE 
)
t.test( provider_enumeration_year ~ individual , nppes_df )
this_table <- table( nppes_df[ , c( "individual" , "is_sole_proprietor" ) ] )

chisq.test( this_table )
glm_result <- 
	glm( 
		provider_enumeration_year ~ individual + is_sole_proprietor , 
		data = nppes_df
	)

summary( glm_result )
library(dplyr)
nppes_tbl <- tbl_df( nppes_df )
nppes_tbl %>%
	summarize( mean = mean( provider_enumeration_year , na.rm = TRUE ) )

nppes_tbl %>%
	group_by( provider_gender_code ) %>%
	summarize( mean = mean( provider_enumeration_year , na.rm = TRUE ) )
