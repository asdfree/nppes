if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
lodown( "nppes" , output_dir = file.path( getwd() ) )
library(DBI)
dbdir <- file.path( getwd() , "SQLite.db" )
db <- dbConnect( RSQLite::SQLite() , dbdir )

dbSendQuery( db , "ALTER TABLE npi ADD COLUMN individual INTEGER" )

dbSendQuery( db , 
	"UPDATE npi 
	SET individual = 
		CASE WHEN entity_type_code = 1 THEN 1 ELSE 0 END" 
)

dbSendQuery( db , "ALTER TABLE npi ADD COLUMN provider_enumeration_year INTEGER" )

dbSendQuery( db , 
	"UPDATE npi 
	SET provider_enumeration_year = 
		CAST( SUBSTRING( provider_enumeration_date , 7 , 10 ) AS INTEGER )" 
)
dbGetQuery( db , "SELECT COUNT(*) FROM npi" )

dbGetQuery( db ,
	"SELECT
		provider_gender_code ,
		COUNT(*) 
	FROM npi
	GROUP BY provider_gender_code"
)
dbGetQuery( db , "SELECT AVG( provider_enumeration_year ) FROM npi" )

dbGetQuery( db , 
	"SELECT 
		provider_gender_code , 
		AVG( provider_enumeration_year ) AS mean_provider_enumeration_year
	FROM npi 
	GROUP BY provider_gender_code" 
)
dbGetQuery( db , 
	"SELECT 
		is_sole_proprietor , 
		COUNT(*) / ( SELECT COUNT(*) FROM npi ) 
			AS share_is_sole_proprietor
	FROM npi 
	GROUP BY is_sole_proprietor" 
)
dbGetQuery( db , "SELECT SUM( provider_enumeration_year ) FROM npi" )

dbGetQuery( db , 
	"SELECT 
		provider_gender_code , 
		SUM( provider_enumeration_year ) AS sum_provider_enumeration_year 
	FROM npi 
	GROUP BY provider_gender_code" 
)
RSQLite::initExtension( db )

dbGetQuery( db , 
	"SELECT 
		LOWER_QUARTILE( provider_enumeration_year ) , 
		MEDIAN( provider_enumeration_year ) , 
		UPPER_QUARTILE( provider_enumeration_year ) 
	FROM npi" 
)

dbGetQuery( db , 
	"SELECT 
		provider_gender_code , 
		LOWER_QUARTILE( provider_enumeration_year ) AS lower_quartile_provider_enumeration_year , 
		MEDIAN( provider_enumeration_year ) AS median_provider_enumeration_year , 
		UPPER_QUARTILE( provider_enumeration_year ) AS upper_quartile_provider_enumeration_year
	FROM npi 
	GROUP BY provider_gender_code" 
)
dbGetQuery( db ,
	"SELECT
		AVG( provider_enumeration_year )
	FROM npi
	WHERE provider_business_practice_location_address_state_name = 'CA'"
)
RSQLite::initExtension( db )

dbGetQuery( db , 
	"SELECT 
		VARIANCE( provider_enumeration_year ) , 
		STDEV( provider_enumeration_year ) 
	FROM npi" 
)

dbGetQuery( db , 
	"SELECT 
		provider_gender_code , 
		VARIANCE( provider_enumeration_year ) AS var_provider_enumeration_year ,
		STDEV( provider_enumeration_year ) AS stddev_provider_enumeration_year
	FROM npi 
	GROUP BY provider_gender_code" 
)
library(dplyr)
library(dbplyr)
dplyr_db <- dplyr::src_sqlite( dbdir )
nppes_tbl <- tbl( dplyr_db , 'npi' )
nppes_tbl %>%
	summarize( mean = mean( provider_enumeration_year ) )

nppes_tbl %>%
	group_by( provider_gender_code ) %>%
	summarize( mean = mean( provider_enumeration_year ) )
dbGetQuery( db , "SELECT COUNT(*) FROM npi" )
