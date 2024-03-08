################################################################################
### RMySQL package and DBI
################################################################################
# Character vector of installed packages
installed.packages()[, "Package"]
pkg <- installed.packages()[, "Package"]

# Check if DBI package is installed
'DBI' %in% pkg

# If package is not installed --> install!
if(!('DBI' %in% pkg)) { install.packages("DBI") }

# Check if RMySQL package is installed
if(!('RMySQL' %in% pkg)) {
  install.packages("RMySQL")
}
## In case of error in Linux: 
## sudo dnf install mariadb-devel mariadb-connector-c-devel

# Load RMySQL package
library(RMySQL)
# Loading required package: DBI

################################################################################

# Testing online database connection
dbUCSClive <- dbConnect(MySQL(), 
                        user = 'genome',
                        dbname = 'hg19', 
                        host = 'genome-euro-mysql.soe.ucsc.edu',
                        port = 3306)

# List of all tables in the specified database (hg19)
dbUCSClive_tables <- dbListTables(dbUCSClive, hg19)
# Show first table names
dbUCSClive_tables[1:50]
# Look for table name in list
grep("refGene",dbUCSClive_tables, perl=TRUE, value=TRUE)

# Get NR_data from table using MySQL query
rsUCSClive = dbSendQuery(dbUCSClive, "SELECT * FROM refGene
                              WHERE name LIKE 'NR_%' AND chrom = 'chr1'")

# Save results as data frame object using fetch function
dataRefGene <- fetch(rsUCSClive, n=-1)
colnames(dataRefGene)
# [1] "bin"          "name"         "chrom"        "strand"       "txStart"     
# [6] "txEnd"        "cdsStart"     "cdsEnd"       "exonCount"    "exonStarts"  
# [11] "exonEnds"     "score"        "name2"        "cdsStartStat" "cdsEndStat"  
# [16] "exonFrames" 
dim(dataRefGene)
# [1] 1945   16

# Get NR_ acccession no.
dataRefGene[1:5,c('name','name2')]
dataRefGene[,'name']
accNo <- dataRefGene[,'name']
class(accNo)

# Get names
dataRefGene[,'name2']
name2 <- dataRefGene[,'name2']

# Get LINC only
grep("LINC",name2, perl=TRUE, value=TRUE)
chr1LINC <- grep("LINC",name2, perl=TRUE, value=TRUE)
chr1LINC

################################################################################

# Ensembl database connection
# dbnames MySQL via subfolders in https://ftp.ensembl.org/pub/
dbEnsembl <- dbConnect(MySQL(), 
                        user = 'anonymous',
                        dbname = 'homo_sapiens_core_110_38', 
                        host = 'ensembldb.ensembl.org')
# List of all tables in the specified database (hg19)
dbEnsembl_tables <- dbListTables(dbEnsembl, homo_sapiens_core_110_38)
dbEnsembl_tables[1:50]
# Look for table name in list
grep("gene",dbEnsembl_tables, perl=TRUE, value=TRUE)
# Get data from table using MySQL query
rsEnsembl = dbSendQuery(dbEnsembl, "SELECT * FROM gene")

# Save results as data frame object using fetch function
dataEnsGene <- fetch(rsEnsembl, n=-1)
colnames(dataEnsGene)
# [1] "gene_id"           "biotype"           "analysis_id"            
# [4] "seq_region_id"     "seq_region_start"  "seq_region_end"         
# [7] "seq_region_strand" "display_xref_id"   "source"                 
# [10] "description"      "is_current"        "canonical_transcript_id"
# [13] "stable_id"        "version"           "created_date"           
# [16] "modified_date"
dim(dataEnsGene)
# [1] 71440    16
dataEnsGene[1:50,c('gene_id',"stable_id")]


################################################################################
# Next part is optional!!! Try if you follow Linux (BIT01) and Databases (BIT04)
################################################################################
# Make sure your database is running (systemctl start mariadb)
#  and you have the ucsc_hg38 database and refGene table
# Do not use root login in scripts!
# Connect with ucsc_hg38 database
dbUCSC <- dbConnect(MySQL(), 
                   user = 'bit', 
                   password = 'rstudio', 
                   dbname = 'ucsc_hg38', 
                   host = 'localhost')

# List tables in ucsc_hg38 database
dbListTables(dbUCSC, ucsc_hg38)
# [1] "refGene"

# Run query and retrieve data by making query and save in results set object 
rsUCSC = dbSendQuery(dbUCSC, "SELECT * FROM refGene
                              WHERE name LIKE 'NR_%' AND chrom = 'chr1'")

# Save results as data frame object using fetch function
dataRefGene <- fetch(rsUCSC, n=-1)
colnames(dataRefGene)
# [1] "bin"          "name"         "chrom"        "strand"       "txStart"     
# [6] "txEnd"        "cdsStart"     "cdsEnd"       "exonCount"    "exonStarts"  
# [11] "exonEnds"     "score"        "name2"        "cdsStartStat" "cdsEndStat"  
# [16] "exonFrames" 
dim(dataRefGene)
# [1] 1753   16


################################################################################
### RMariaDB package and DBI (informational only!)
################################################################################
# RMySQL old implementation --> being phased out in favor of new MariaDB
# RMariaDB is a database interface and MariaDB driver for R
#  --> complies with database interface definition implemented in package DBI 
# REMARK: in december 2018 on Fedora 29 --> error
# More info:
#  https://github.com/r-dbi/RMariaDB
#  https://github.com/r-dbi/DBI
################################################################################
pkg <- installed.packages()[, "Package"]
if(!('devtools' %in% pkg)) { install.packages("devtools") }
library(devtools)
# Error with usethis 
#  --> sudo dnf install libcurl-devel (Fedora, CentOS, RHEL)
#  --> retry installing devtools
if(!('RMariaDB' %in% pkg)) { install.packages("RMariaDB") }
library(RMariaDB)

# Connect to database
dbCon <- dbConnect(RMariaDB::MariaDB(), 
                 host = "localhost",
                 user = "bit", 
                 password = "rstudio",
                 dbname = "ucsc_hg38")

dbGetQuery(dbCon, "SHOW TABLES")
dbListTables(dbCon)
# Error in result_fetch(res@ptr, n = n) : Error fetching buffer
# --> caused by problem with Connector/C

# Disconnect from the database
dbDisconnect(dbCon)



################################################################################
### RSQLite package and DBI
################################################################################
pkg <- installed.packages()[, "Package"]
if(!('RSQLite' %in% pkg)) { install.packages("RSQLite") }
library(RSQLite)
# SQLite only needs a path to the database
# Create an in-memory RSQLite database
dbConLite <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")

dbListTables(dbConLite)
dbWriteTable(dbConLite, "mtcars", mtcars)
dbListTables(dbConLite)

dbListFields(dbConLite, "mtcars")
dbReadTable(dbConLite, "mtcars")

# You can fetch all results:
res <- dbSendQuery(dbConLite, "SELECT * FROM mtcars WHERE cyl = 4")
dbFetch(res)
dbClearResult(res)

dbDisconnect(dbConLite)



################################################################################
################################################################################
################################################################################

