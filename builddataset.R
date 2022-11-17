# Makeing synthetic  C2M2 data

# This tool creates a valid C2M2 instance. 
###################################################################################
###################################################################################
# --------------------------------User Settable Options-------------------------------
## Customize your synthetic DCC

## To make a given package easily identifiable when browsing the portal be sure to change these 5 options for each run:

filenameprefix <- 'nov17'    
biosampleprefix <- 'nov17-'
subjectprefix <- 'nov17-'
collections <- c("muchwow"="a collection of files with a randomly chosen format", "doge"="a collection of things with a randomly chosen datatype")
outputfoldername <- "test_2022_11_17"

# collections uses comma separated keypairs: "title"="description" 
### be sure to add or subtract entire pairs at once

### The rest of these parameters can be left the same, or changed however you like
### Every settable option must have at least one value

c2m2id <- "cfde_registry_dcc:test1" # must be a valid ID starting with `cfde_registry_dcc:`, currently: "test1", "hmp", "gtex", "motrpac", "kidsfirst", "metabolomics", "lincs", "4dn", "idg", "exrna", "sparc", "test2"
dccabbrev <- "procca"
website <- "https://www.raynamharris.com/"
email <- "rmharris@ucdavis.edu"
submitter <- "Rayna Harris"
fileprefix <- 'nov17-'

## Arrays: add/subtract values from lists to change complexity of data
### Arrays must be key value pairs: "title"="description"
### be sure to add or subtract entire pairs at once

namespace <- c("tag:raynamharris.com,2022-11-17:"="the best namespace") #currently only a single namespace is supported
mainproject <- c("queso"="the main project")
projects <- c("taco"="a project with a hard shell", "burrito"="a rolled project", "nachos"="a spread out project")
dcc <- c("procrastinomics"="the best dcc") # name=description

## Point values: change to any other point value to increase/decrease size of data
### Number of files in file table
numfile <- 1000
### Number of biosamples in biosample table
numbio <- 50
### Number of subjects in subject table
numsub <- 5
### Maximum number of controlled vocabulary terms to include
anatomys <- 10
assays <- 10
analyses <- 4
bioassays <- 4
compressionformats <- c("format:3987", "format:3615")
datatypes <- 5
dbgap_permissions <- 3
diseases <- 8
fileformats <- 4
genes <- 34  #max is 5000
phenotypes <- 7 #max is 5000
proteins <- 4
species <- 2
subjectgranularitys <- 2 #must be a number between 0-6
subjectroles <- 2 # must be a number between 0-8
substances <- 10  #max is 5000

## Do you want to include demographic data?
subjectethnicity <- "yes"  #"yes" or "no"
subjectrace <-  "yes"  #"yes" or "no"
subjectsex <-  "yes"  #"yes" or "no"
### Ages of subjects 
averageage <- 30  # number or NA to not include age
standarddev <- 30 # number or NA to not include age

## How randomized should metadata be on a scale from 1-100? 1= fewest possible combinations, 100= every unique combination
metadata_random <- 10
### Should the metadata appear roughly equally? (yes/no)
metadata_even <- "yes"

## Allow for missing non-required metadata values as a percentage of total values
### 15 = 15% missingness. Must be value between 0 and 100
### filesmissing, biosamplesmissing, and subjectsmissing create missingness in those tables.
### associationsmissing creates missingness in association tables and collection associations
filesmissing <- 10
biosamplesmissing <- 12
subjectsmissing <- 5
associationsmissing <- 0 
### Date range. For generating creation dates
startdate <- '1999/01/01'
enddate <- '2000/01/01'


#---------------------------End of Settable Options--------------------------------
#        Click "Source" at the top left of this screen to run the code            #
###################################################################################
###################################################################################
###################################################################################

## Load libraries

library(stringr)
library(stringi)
library(data.table)
library(dplyr)
library(numbers)


## Files with input data
## Each is randomly sampled to only pull in as many terms as are asked for above
### If you want to add/change/delete assays, file_formats, anatomy, disease, or data_types directly edit the files in the CVtables directory

anatomy_table <- sample_n(fread("CVtables/anatomy.tsv", sep = "\t"), anatomys, replace = T) %>% unique() %>% droplevels()
assaytype_table <- fread("CVtables/assay_type.tsv", sep = "\t") %>% filter(str_detect(id, "OBI")) %>% sample_n(assays, replace = T) %>% unique() %>% droplevels()
analysistype_table <- fread("CVtables/assay_type.tsv", sep = "\t") %>% filter(str_detect(id, "OBI")) %>% sample_n(analyses, replace = T) %>% unique() %>% droplevels()
bioassaytype_table <- fread("CVtables/assay_type.tsv", sep = "\t") %>% filter(str_detect(id, "OBI")) %>% sample_n(bioassays, replace = T) %>% unique() %>% droplevels()
datatype_table <- sample_n(fread("CVtables/data_type.tsv", sep = "\t"), datatypes, replace = T) %>% unique() %>% droplevels()
disease_table <- sample_n(fread("CVtables/disease.tsv", sep = "\t"), diseases, replace = T) %>% unique() %>% droplevels()
fileformat_table <- sample_n(fread("CVtables/file_format.tsv", sep = "\t"), fileformats, replace = T) 
fileformat_table <- rbind(fileformat_table, filter(fread("CVtables/file_format.tsv", sep = "\t"), id %in% compressionformats))%>% unique() %>% droplevels()
gene_table <- sample_n(fread("CVtables/gene_tiny.tsv", sep = "\t"), genes, replace = T) %>% unique() %>% droplevels()
granularity_table <- sample_n(fread("CVtables/subject_granularity.tsv", sep = "\t"), subjectgranularitys, replace = T) %>% unique() %>% droplevels()
phenotype_table <- sample_n(fread("CVtables/phenotype_tiny.tsv", sep = "\t"), phenotypes, replace = T) %>% unique() %>% droplevels()
protein_table <- sample_n(fread("CVtables/protein_tiny.tsv", sep = "\t"), proteins, replace = T) %>% unique() %>% droplevels()

ras_list <- c("000228", "001110", "001436", "002162", "001785", "000467")
ras_table <- paste("phs", sample(ras_list, dbgap_permissions, replace = T), sep="")

role_table <- sample_n(fread("CVtables/subject_role.tsv", sep = "\t"), subjectroles, replace = T) %>% unique() %>% droplevels()
subjectethnicity_table <- fread("CVtables/subject_ethnicity.tsv", sep = "\t")
subjectrace_table <- fread("CVtables/subject_race.tsv", sep = "\t")
subjectsex_table <- fread("CVtables/subject_sex.tsv", sep = "\t")
taxon_table <- sample_n(fread("CVtables/ncbi_taxonomy_tiny.tsv", sep= "\t"), species, replace = T)
taxon_table <- rbind(taxon_table, filter(fread("CVtables/ncbi_taxonomy_tiny.tsv", sep= "\t"), id %in% gene_table$organism | id %in% protein_table$organism)) %>% unique() %>% droplevels()
substance_table <-sample_n(fread("CVtables/substance.records_for_first_5000_CIDs.max_100_synonyms_per_term.tsv", sep = "\t"), substances, replace = T) %>% unique() %>% droplevels()
compound_table <-fread("CVtables/compound.first_5000_records.max_100_synonyms_per_term.tsv", sep = "\t")  %>% filter(id %in% substance_table$compound)

## Real data has correlations, this introduces correlations between several of the CV terms to make the data look more real

multiplier <- numbers::mLCM(c(nrow(anatomy_table), nrow(assaytype_table), nrow(datatype_table), nrow(disease_table), nrow(fileformat_table), length(names(projects)), nrow(granularity_table), nrow(role_table), nrow(analysistype_table)))
metadatasets <- data.frame( matrix(ncol = 9, nrow = multiplier))
colnames(metadatasets) <- c("anatomy", "assay", "bioassay", "data", "file", "project", "granularity", "role","analysis")
metadatasets$anatomy <- anatomy_table$id
metadatasets$analysis <- analysistype_table$id
metadatasets$assay <- assaytype_table$id
metadatasets$bioassay <- bioassaytype_table$id
metadatasets$data <- datatype_table$id
metadatasets$file <- fileformat_table$id
metadatasets$granularity <- granularity_table$id
metadatasets$project <- names(projects)
metadatasets$role  <- role_table$id
metadatasets <- metadatasets[sample(c(1:multiplier), size = 1 + round(multiplier*(metadata_random/100), digits = 0), replace = F),]
if (metadata_even == 'no') {
  metadatasets <- metadatasets[sample(1:nrow(metadatasets), size = nrow(metadatasets), replace = T),]
}

## In real data, not all associations are filled. These simple weights give us a way to 
## introduce similar variability


anatomyweights <- cbind(c(associationsmissing, rep(50/length(anatomy_table$id), length(anatomy_table$id))), c(NA, anatomy_table$id))
substanceweights <- cbind(c(associationsmissing, rep(50/length(substance_table$id), length(substance_table$id))), c(NA, substance_table$id))
compoundweights <- cbind(c(associationsmissing, rep(50/length(compound_table$id), length(compound_table$id))), c(NA, compound_table$id))
geneweights <- cbind(c(associationsmissing, rep(50/length(gene_table$id), length(gene_table$id))), c(NA, gene_table$id))
diseaseweights <- cbind(c(associationsmissing, rep(50/length(disease_table$id), length(disease_table$id))), c(NA, disease_table$id))
phenotypeweights <- cbind(c(associationsmissing, rep(50/length(phenotype_table$id), length(phenotype_table$id))), c(NA, phenotype_table$id))
taxonweights <- cbind(c(associationsmissing, rep(50/length(taxon_table$id), length(taxon_table$id))), c(NA, taxon_table$id))
proteinweights <- cbind(c(associationsmissing, rep(50/length(protein_table$id), length(protein_table$id))), c(NA, protein_table$id))






## Core tables

### file.tsv


file_tsv <- data.frame(matrix(nrow=numfile, ncol = 20))
colnames(file_tsv) <- c("id_namespace", "local_id", "project_id_namespace", 
                         "project_local_id", "persistent_id", "creation_time", 
                         "size_in_bytes", "uncompressed_size_in_bytes", 
                         "sha256", "md5", "filename", "file_format", "compression_format",
                         "data_type","assay_type", "analysis_type", "mime_type", "bundle_collection_id_namespace",
                         "bundle_collection_local_id", "dbgap_study_id")
file_tsv$id_namespace <- names(namespace)
file_tsv$local_id <- paste(fileprefix, stri_rand_strings(numfile, 3, pattern = "[0-9]"), str_pad(c(1:numfile),6, pad="0"), sep="")
file_tsv$project_id_namespace <- names(namespace)
file_tsv$persistent_id <- NA
file_tsv$creation_time <- sample(seq(as.Date(startdate), as.Date(enddate), by="day"), numfile, replace = T)
file_tsv$size_in_bytes <- abs(sample(.Random.seed, numfile, replace = T))*2
file_tsv$sha256 <- stri_rand_strings(numfile, 64, pattern = "[a-fA-F0-9]")
file_tsv$md5 <- ""
file_tsv$filename <- paste(stri_rand_strings(numfile, 3, pattern = "[A-Z]"), "_",stri_rand_strings(numfile, 6, pattern = "[0-9]"), sep="") 
file_tsv[,c(4, 12, 14, 15, 16)] <- metadatasets[sample(1:nrow(metadatasets), numfile, replace = T),c(6,5,4,2,9)]
file_tsv$mime_type <- NA
file_tsv$bundle_collection_id_namespace <- NA
file_tsv$bundle_collection_local_id <- NA
file_tsv$compression_format <- sample(compressionformats, size = numfile, replace = T)
file_tsv$dbgap_study_id <- sample(ras_table, size = numfile, replace = T)
file_tsv$uncompressed_size_in_bytes <- file_tsv$size_in_bytes + abs(sample(.Random.seed, size = numfile, replace = T))


if (filesmissing > 0) {
  remove <- round(numfile*(filesmissing/100))
  file_tsv$creation_time[c(sample(1:numfile, remove, replace = F))] <- NA
  file_tsv$size_in_bytes[c(sample(1:numfile, remove, replace = F))] <- NA
  file_tsv$sha256[c(sample(1:numfile, remove, replace = F))] <- NA
  file_tsv$file_format[c(sample(1:numfile, remove, replace = F))] <- NA
  file_tsv$data_type[c(sample(1:numfile, remove, replace = F))] <- NA
  file_tsv$assay_type[c(sample(1:numfile, remove, replace = F))] <- NA
  file_tsv$analysis_type[c(sample(1:numfile, remove, replace = F))] <- NA
  file_tsv$dbgap_study_id[c(sample(1:numfile, remove, replace = F))] <- NA
  file_tsv$compression_format[c(sample(1:numfile, remove, replace = F))] <- NA
  file_tsv$uncompressed_size_in_bytes[is.na(file_tsv$compression_format) == T] <- NA
}

### biosample.tsv
biosample_tsv <- data.frame(matrix(nrow=numbio, ncol = 8))
colnames(biosample_tsv) <- c("id_namespace", "local_id", "project_id_namespace", 
                              "project_local_id", "persistent_id", "creation_time",
                              "assay_type", "anatomy")
biosample_tsv$id_namespace <- names(namespace)
biosample_tsv$local_id <- paste(biosampleprefix, stri_rand_strings(numbio, 3, pattern = "[0-9]"), str_pad(c(1:numbio),6, pad="0"), sep="")
biosample_tsv$project_id_namespace <- names(namespace)
biosample_tsv$persistent_id <- ""
biosample_tsv$creation_time <- sample(seq(as.Date(startdate), as.Date(enddate), by="day"), numbio, replace = T)
biosample_tsv[,c(4, 7, 8)] <- metadatasets[sample(1:nrow(metadatasets), numbio, replace = T),c(6,3,1)]
if (biosamplesmissing > 0) {
  remove <- round(numbio*(biosamplesmissing/100))
  biosample_tsv$creation_time[c(sample(1:numbio, remove, replace = T))] <- NA
  biosample_tsv$assay_type[c(sample(1:numbio, remove, replace = T))] <- NA
  biosample_tsv$anatomy[c(sample(1:numbio, remove, replace = T))] <- NA
}  

### subject.tsv
subject_tsv <- data.frame(matrix(nrow=numsub, ncol = 10))
colnames(subject_tsv) <- c("id_namespace", "local_id", "project_id_namespace", 
                            "project_local_id", "persistent_id", "creation_time",
                            "granularity", "sex", "ethnicity", "age_at_enrollment")
subject_tsv$id_namespace <- names(namespace)
subject_tsv$local_id <- paste(subjectprefix, stri_rand_strings(numsub, 3, pattern = "[0-9]"), str_pad(c(1:numsub),6, pad="0"), sep="")
subject_tsv$project_id_namespace <- names(namespace)
subject_tsv$project_local_id <- sample(names(projects), numsub, replace = T)
subject_tsv$persistent_id <- ""
subject_tsv$creation_time <- sample(seq(as.Date(startdate), as.Date(enddate), by="day"), numsub, replace = T)
subject_tsv$granularity <- sample(metadatasets$granularity, numsub, replace = T)
if (subjectsex == 'yes') {
  subject_tsv$sex <- sample(c(NA, subjectsex_table$id), numsub, replace = T, prob = c(associationsmissing, 10, 50, 50, 10, 10, 10))
}
if (subjectethnicity == 'yes') {
  subject_tsv$ethnicity <- sample(c(NA, subjectethnicity_table$id), numsub, replace = T, prob = c(associationsmissing, 50, 50))
}
if (!is.na(averageage)){
subject_tsv$age_at_enrollment <- rnorm(n = numsub, mean = averageage, sd = standarddev) %>% round(digits = 2) %>% abs()
}

if (subjectsmissing > 0) {
  remove <- round(numsub*(subjectsmissing/100))
  subject_tsv$creation_time[c(sample(1:numsub, remove, replace = T))] <- NA
  subject_tsv$age_at_enrollment[c(sample(1:numsub, remove, replace = T))] <- NA
  subject_tsv$sex[c(sample(1:numsub, remove, replace = T))] <- NA
  subject_tsv$ethnicity[c(sample(1:numsub, remove, replace = T))] <- NA
}  

### collection.tsv
collection_tsv <- data.frame(matrix(nrow=length(collections), ncol = 8))
colnames(collection_tsv) <- c("id_namespace", "local_id", "persistent_id", 
                               "creation_time", "abbreviation", "name", "description",
                               "has_time_series_data")

collection_tsv$id_namespace <- names(namespace)
collection_tsv$local_id <- paste('coll-', stri_rand_strings(length(collections), 3, pattern = "[0-9]"), str_pad(c(1:length(collections)),6, pad="0"), sep="")
collection_tsv$persistent_id <-  ""
collection_tsv$creation_time <- sample(seq(as.Date(startdate), as.Date(enddate), by="day"), length(collections), replace = T)
collection_tsv$abbreviation <- names(collections)
collection_tsv$name <- names(collections)
collection_tsv$description <- collections
collection_tsv$has_time_series_data <- sample(c(NA, "true", "false"), length(collections), prob = c(associationsmissing,30,30))
#### go back and fill in collection information for files that were labeled as bundles
bundles <- sample(file_tsv$local_id[is.na(file_tsv$compression_format) == F], size = length(collections))
for (bundle in length(bundles)){
  file_tsv$bundle_collection_local_id[file_tsv$local_id==bundles[bundle]] <- collection_tsv$local_id[bundle]
  file_tsv$bundle_collection_id_namespace[file_tsv$local_id==bundles[bundle]] <- collection_tsv$id_namespace[bundle]
  }

### namespace.tsv
namespace_tsv <- as.data.frame(t(as.matrix( c(names(namespace), dccabbrev, names(dcc), dcc)) ))
colnames(namespace_tsv) <- c("id", "abbreviation", "name", "description")

### dcc.tsv
dcc_tsv <- as.data.frame(t(as.matrix(c(c2m2id, names(dcc), dccabbrev, dcc, email, submitter, website, names(namespace), names(mainproject)))))
colnames(dcc_tsv) <- c("id", "dcc_name", "dcc_abbreviation", "dcc_description", "contact_email", "contact_name", "dcc_url", "project_id_namespace", "project_local_id")

### project.tsv
allprojects <- c(mainproject, projects)
project_tsv <- data.frame(matrix(nrow=length(allprojects), ncol = 7))
colnames(project_tsv) <- c("id_namespace", "local_id", "persistent_id", 
                            "creation_time", "abbreviation", "name", "description")

project_tsv$id_namespace <- names(namespace)
project_tsv$local_id <- names(allprojects)
project_tsv$persistent_id <-  ""
project_tsv$creation_time <- sample(seq(as.Date(startdate), as.Date(enddate), by="day"), length(allprojects))
project_tsv$abbreviation <- names(allprojects)
project_tsv$name <- names(allprojects)
project_tsv$description <- allprojects


## Association Tables

### biosample_from_subject.tsv
biosample_from_subject_tsv <- right_join(biosample_tsv, subject_tsv, by = c("id_namespace", "project_local_id")) %>% 
  group_by(local_id.x) %>% slice(1) %>% 
  mutate(age_at_sampling= abs(round(age_at_enrollment + (rnorm(n = 1, mean = 2, sd = .5)), digits = 2))) %>%
  select(project_id_namespace.x, local_id.x, project_id_namespace.y, local_id.y, age_at_sampling)

colnames(biosample_from_subject_tsv) <- c( "biosample_id_namespace", "biosample_local_id",
                                     "subject_id_namespace", "subject_local_id", "age_at_sampling")


if (subjectsmissing > 0 & biosamplesmissing > 0) {
  remove <- round(numsub*(subjectsmissing/100)*(biosamplesmissing/100))
  biosample_from_subject_tsv$biosample_local_id[c(sample(1:numbio, remove, replace = T))] <- NA
  biosample_from_subject_tsv <- filter(biosample_from_subject_tsv, !is.na(biosample_local_id)) %>% unique() %>% droplevels()
} 

### file_describes_biosample.tsv
file_describes_biosample_tsv <- right_join(file_tsv, biosample_tsv, by = c("id_namespace", "project_local_id")) %>% 
  group_by(local_id.x) %>% slice(1) %>% 
  select(project_id_namespace.x, local_id.x, project_id_namespace.y, local_id.y)

colnames(file_describes_biosample_tsv) <- c( "file_id_namespace", "file_local_id",
                                       "biosample_id_namespace", "biosample_local_id")

if (filesmissing > 0 & biosamplesmissing > 0) {
  remove <- round(numfile*(filesmissing/100)*(biosamplesmissing/100))
  file_describes_biosample_tsv$file_local_id[c(sample(1:numfile, remove, replace = T))] <- NA
  file_describes_biosample_tsv <- filter(file_describes_biosample_tsv, !is.na(file_local_id), !is.na(biosample_local_id)) %>% unique() %>% droplevels()
}   


### file_describes_subject.tsv

file_describes_subject_tsv <- left_join(file_describes_biosample_tsv, biosample_from_subject_tsv) %>%
  select("file_id_namespace", "file_local_id","subject_id_namespace", "subject_local_id")

if (filesmissing > 0 & subjectsmissing > 0) {
  remove <- round(numfile*(filesmissing/100)*(subjectsmissing/100))
  file_describes_subject_tsv$file_local_id[c(sample(1:length(file_describes_subject_tsv$file_local_id), remove, replace = T))] <- NA
  file_describes_subject_tsv <- filter(file_describes_subject_tsv, !is.na(file_local_id), !is.na(subject_local_id)) %>% unique() %>% droplevels()
} 

### subject_substance.tsv

subject_substance_tsv <- data.frame(matrix(nrow=numsub, ncol = 3))
colnames(subject_substance_tsv) <- c( "subject_id_namespace", "subject_local_id",
                                        "substance")

subject_substance_tsv$subject_id_namespace <- names(namespace)
subject_substance_tsv$subject_local_id <- sample(subject_tsv$local_id, replace = F, size = numsub)
subject_substance_tsv$substance <- sample(substance_table$id, numsub, replace = T)
subject_substance_tsv$substance <- sample(substanceweights[,2], size = numsub, replace = T, prob = substanceweights[,1])
subject_substance_tsv <- filter(subject_substance_tsv, !is.na(substance)) %>% unique() %>% droplevels()

### subject_phenotype.tsv

subject_phenotype_tsv <- data.frame(matrix(nrow=numsub, ncol = 4))
colnames(subject_phenotype_tsv) <- c( "subject_id_namespace", "subject_local_id",
                                      "association_type", "phenotype")

subject_phenotype_tsv$subject_id_namespace <- names(namespace)
subject_phenotype_tsv$subject_local_id <- sample(subject_tsv$local_id, replace = F, size = numsub)
subject_phenotype_tsv$association_type <- sample(c(NA, "cfde_phenotype_association_type:0","cfde_phenotype_association_type:1"), 
                                                 replace = T, prob = c(associationsmissing,10,40), size = length(subject_tsv$local_id))
subject_phenotype_tsv$phenotype <- sample(phenotype_table$id, numsub, replace = T)
subject_phenotype_tsv$phenotype <- sample(phenotypeweights[,2], size = numsub, replace = T, prob = phenotypeweights[,1])
subject_phenotype_tsv <- filter(subject_phenotype_tsv, !is.na(phenotype), !is.na(association_type)) %>% unique() %>% droplevels()


### biosample_substance.tsv

biosample_substance_tsv <- data.frame(matrix(nrow=numbio, ncol = 3))
colnames(biosample_substance_tsv) <- c( "biosample_id_namespace", "biosample_local_id",
                                 "substance")
biosample_substance_tsv$biosample_id_namespace <- names(namespace)
biosample_substance_tsv$biosample_local_id <- sample(biosample_tsv$local_id, replace = F, size = numbio)
biosample_substance_tsv$substance <- sample(substance_table$id, numbio, replace = T)
biosample_substance_tsv$substance <- sample(substanceweights[,2], size = numbio, replace = T, prob = substanceweights[,1])
biosample_substance_tsv <- filter(biosample_substance_tsv, !is.na(substance)) %>% unique() %>% droplevels()

### biosample_gene.tsv

biosample_gene_tsv <- data.frame(matrix(nrow=numbio, ncol = 3))
colnames(biosample_gene_tsv) <- c( "biosample_id_namespace", "biosample_local_id",
                                   "gene")
biosample_gene_tsv$biosample_id_namespace <- names(namespace)
biosample_gene_tsv$biosample_local_id <- sample(biosample_tsv$local_id, replace = F, size = numbio)
biosample_gene_tsv$gene <- sample(geneweights[,2], size = numbio, replace = T, prob = geneweights[,1])
biosample_gene_tsv <- filter(biosample_gene_tsv, !is.na(gene)) %>% unique() %>% droplevels()

### subject_race.tsv

subject_race_tsv <- data.frame(matrix(nrow=numsub, ncol = 3))
colnames(subject_race_tsv) <- c( "subject_id_namespace", "subject_local_id", "race")
if (subjectrace == 'yes') {
subject_race_tsv$subject_id_namespace <- names(namespace)
subject_race_tsv$subject_local_id <- subject_tsv$local_id
subject_race_tsv$race <- sample(c(NA, subjectrace_table$id), size = numsub, replace = T, prob = c(associationsmissing,10,10,30,20,20 ))
subject_race_tsv <- filter(subject_race_tsv, !is.na(race))  %>% unique() %>% droplevels()
}

### file_in_collection.tsv

randformat <- filter(file_tsv, file_format==sample(fileformat_table$id, 1), !local_id %in% bundles)
randatatype <- filter(file_tsv, data_type==sample(datatype_table$id,1), !local_id %in% bundles)
file_in_collection_tsv <- data.frame(matrix(nrow=length(randformat$local_id) + length(randatatype$local_id), ncol = 4))
colnames(file_in_collection_tsv) <- c( "file_id_namespace", "file_local_id",
                                 "collection_id_namespace", "collection_local_id")

file_in_collection_tsv$file_id_namespace <- names(namespace)
file_in_collection_tsv$file_local_id <- c(randformat$local_id, randatatype$local_id)
file_in_collection_tsv$collection_id_namespace <- names(namespace)
file_in_collection_tsv$collection_local_id <- c(rep(collection_tsv$local_id[1],length(randformat$local_id)), rep(collection_tsv$local_id[2],length(randatatype$local_id))) 
file_in_collection_tsv <- file_in_collection_tsv %>% unique() %>% droplevels()

### file_describes_collection.tsv

file_describes_collection_tsv <- data.frame(matrix(nrow=0, ncol = 4))
colnames(file_describes_collection_tsv) <- c( "file_id_namespace", "file_local_id",
                                        "collection_id_namespace", "collection_local_id")

### biosample_in_collection.tsv

biosample_in_collection_tsv <- left_join(file_in_collection_tsv, file_describes_biosample_tsv) %>%
  select(biosample_id_namespace, biosample_local_id, collection_id_namespace, collection_local_id) %>% 
  filter(!is.na(biosample_id_namespace))  %>% unique() %>% droplevels()

### subject_in_collection.tsv

subject_in_collection_tsv <- left_join(file_in_collection_tsv, file_describes_subject_tsv) %>%
  select(subject_id_namespace, subject_local_id, collection_id_namespace, collection_local_id) %>%
  filter(!is.na(subject_id_namespace))  %>% unique() %>% droplevels()

### collection_in_collection.tsv
collection_in_collection_tsv <- data.frame(matrix(nrow=0, ncol = 4))
colnames(collection_in_collection_tsv) <- c( "superset_collection_id_namespace", "superset_collection_local_id",
                                       "subset_collection_id_namespace", "subset_collection_local_id")


### collection_anatomy.tsv
collection_anatomy_tsv <- data.frame(matrix(nrow=nrow(collection_tsv), ncol = 3))
colnames(collection_anatomy_tsv) <- c( "collection_id_namespace", "collection_local_id",
                                       "anatomy")
collection_anatomy_tsv$collection_id_namespace <- collection_tsv$id_namespace
collection_anatomy_tsv$collection_local_id <- collection_tsv$local_id
collection_anatomy_tsv$anatomy <- sample(anatomyweights[,2], nrow(collection_tsv), replace = T, prob = anatomyweights[,1])
collection_anatomy_tsv <- filter(collection_anatomy_tsv, !is.na(anatomy)) %>% unique() %>% droplevels()


### collection_disease.tsv
collection_disease_tsv <- data.frame(matrix(nrow=nrow(collection_tsv), ncol = 3))
colnames(collection_disease_tsv) <- c( "collection_id_namespace", "collection_local_id",
                                       "disease")
collection_disease_tsv$collection_id_namespace <- collection_tsv$id_namespace
collection_disease_tsv$collection_local_id <- collection_tsv$local_id
collection_disease_tsv$disease <- sample(diseaseweights[,2], nrow(collection_tsv), replace = T, prob = diseaseweights[,1])
collection_disease_tsv <- filter(collection_disease_tsv, !is.na(disease)) %>% unique() %>% droplevels()

### collection_protein.tsv

collection_protein_tsv <- data.frame(matrix(nrow=nrow(collection_tsv), ncol = 3))
colnames(collection_protein_tsv) <- c( "collection_id_namespace", "collection_local_id",
                                  "protein")
collection_protein_tsv$collection_id_namespace <- collection_tsv$id_namespace
collection_protein_tsv$collection_local_id <- collection_tsv$local_id
collection_protein_tsv$protein <- sample(proteinweights[,2], nrow(collection_tsv), replace = T, prob = proteinweights[,1])
collection_protein_tsv <- filter(collection_protein_tsv, !is.na(protein)) %>% unique() %>% droplevels()

### collection_phenotype.tsv

collection_phenotype_tsv <- data.frame(matrix(nrow=nrow(collection_tsv), ncol = 3))
colnames(collection_phenotype_tsv) <- c( "collection_id_namespace", "collection_local_id",
                                         "phenotype")
collection_phenotype_tsv$collection_id_namespace <- collection_tsv$id_namespace
collection_phenotype_tsv$collection_local_id <- collection_tsv$local_id
collection_phenotype_tsv$phenotype <- sample(phenotypeweights[,2], nrow(collection_tsv), replace = T, prob = phenotypeweights[,1])
collection_phenotype_tsv <- filter(collection_phenotype_tsv, !is.na(phenotype)) %>% unique() %>% droplevels()


### collection_gene.tsv

collection_gene_tsv <- data.frame(matrix(nrow=nrow(collection_tsv), ncol = 3))
colnames(collection_gene_tsv) <- c( "collection_id_namespace", "collection_local_id",
                                    "gene")
collection_gene_tsv$collection_id_namespace <- collection_tsv$id_namespace
collection_gene_tsv$collection_local_id <- collection_tsv$local_id
collection_gene_tsv$gene <- sample(geneweights[,2], nrow(collection_tsv), replace = T, prob = geneweights[,1])
collection_gene_tsv <- filter(collection_gene_tsv, !is.na(gene)) %>% unique() %>% droplevels()

### collection_compound.tsv

collection_compound_tsv <- data.frame(matrix(nrow=nrow(collection_tsv), ncol = 3))
colnames(collection_compound_tsv) <- c( "collection_id_namespace", "collection_local_id",
                               "compound")
collection_compound_tsv$collection_id_namespace <- collection_tsv$id_namespace
collection_compound_tsv$collection_local_id <- collection_tsv$local_id
collection_compound_tsv$compound <- sample(compoundweights[,2], nrow(collection_tsv), replace = T, prob = compoundweights[,1])
collection_compound_tsv <- filter(collection_compound_tsv, !is.na(compound)) %>% unique() %>% droplevels()

### collection_substance.tsv

collection_substance_tsv <- data.frame(matrix(nrow=nrow(collection_tsv), ncol = 3))
colnames(collection_substance_tsv) <- c( "collection_id_namespace", "collection_local_id",
                               "substance")
collection_substance_tsv$collection_id_namespace <- collection_tsv$id_namespace
collection_substance_tsv$collection_local_id <- collection_tsv$local_id
collection_substance_tsv$substance <- sample(substanceweights[,2], nrow(collection_tsv), replace = T, prob = substanceweights[,1])
collection_substance_tsv <- filter(collection_substance_tsv, !is.na(substance)) %>% unique() %>% droplevels()

### collection_taxonomy.tsv

collection_taxonomy_tsv <- data.frame(matrix(nrow=nrow(collection_tsv), ncol = 3))
colnames(collection_taxonomy_tsv) <- c( "collection_id_namespace", "collection_local_id",
                                    "taxon")
collection_taxonomy_tsv$collection_id_namespace <- collection_tsv$id_namespace
collection_taxonomy_tsv$collection_local_id <- collection_tsv$local_id
collection_taxonomy_tsv$taxon <- sample(taxonweights[,2], nrow(collection_tsv), replace = T, prob = taxonweights[,1])
collection_taxonomy_tsv <- filter(collection_taxonomy_tsv, !is.na(taxon))  %>% unique() %>% droplevels()


### subject_disease.tsv

subject_disease_tsv <- data.frame(matrix(nrow=numsub, ncol = 4))
colnames(subject_disease_tsv) <- c("subject_id_namespace", "subject_local_id", "association_type",
                              "disease")
subject_disease_tsv$subject_id_namespace <- names(namespace)
subject_disease_tsv$subject_local_id <- subject_tsv$local_id
subject_disease_tsv$association_type <- sample(c(NA, "cfde_disease_association_type:0","cfde_disease_association_type:1"), 
                                          replace = T, prob = c(associationsmissing,10,40), size = length(subject_tsv$local_id))
subject_disease_tsv$disease <- sample(disease_table$id, numsub, replace = T)
subject_disease_tsv <- filter(subject_disease_tsv, !is.na(disease), !is.na(association_type))  %>% unique() %>% droplevels()



### biosample_disease.tsv

biosample_disease_tsv <- full_join(subject_disease_tsv, biosample_from_subject_tsv) %>% 
  select(biosample_id_namespace, biosample_local_id, association_type, disease) %>%
  filter(!is.na(biosample_local_id), !is.na(disease),!is.na(association_type))  %>% unique() %>% droplevels()

### subject_role_taxonomy.tsv

subject_role_taxonomy_tsv <- data.frame(matrix(nrow=numsub, ncol = 4))
colnames(subject_role_taxonomy_tsv) <- c("subject_id_namespace", "subject_local_id", "role_id", "taxonomy_id")

subject_role_taxonomy_tsv$subject_id_namespace <- names(namespace)
subject_role_taxonomy_tsv$subject_local_id <- subject_tsv$local_id
subject_role_taxonomy_tsv$role_id <- sample(metadatasets$role, numsub, replace = T)
subject_role_taxonomy_tsv$taxonomy_id <- sample(taxonweights[,2], numsub, replace = T, prob =taxonweights[,1])
subject_role_taxonomy_tsv <- filter(subject_role_taxonomy_tsv, !is.na(taxonomy_id)) %>% unique() %>% droplevels()

### protein_gene.tsv

protein_gene_tsv <- data.frame(matrix(nrow=proteins, ncol = 2))
colnames(protein_gene_tsv) <- c("protein", "gene")
protein_gene_tsv$protein <- sample(proteinweights[,2], proteins, replace = TRUE, prob = proteinweights[,1])
protein_gene_tsv$gene <- sample(geneweights[,2], proteins, replace = TRUE, prob = geneweights[,1])
protein_gene_tsv <- filter(protein_gene_tsv, !is.na(protein), !is.na(gene)) %>% unique() %>% droplevels()

### phenotype_gene.tsv

phenotype_gene_tsv <- data.frame(matrix(nrow=phenotypes, ncol = 2))
colnames(phenotype_gene_tsv) <- c("phenotype", "gene")
phenotype_gene_tsv$phenotype <- sample(phenotypeweights[,2], phenotypes, replace = TRUE, prob = phenotypeweights[,1])
phenotype_gene_tsv$gene <- sample(geneweights[,2], phenotypes, replace = TRUE, prob = geneweights[,1])
phenotype_gene_tsv <- filter(phenotype_gene_tsv, !is.na(phenotype), !is.na(gene))  %>% unique() %>% droplevels()

### phenotype_disease.tsv

phenotype_disease_tsv <- data.frame(matrix(nrow=phenotypes, ncol = 2))
colnames(phenotype_disease_tsv) <- c("phenotype", "disease")
phenotype_disease_tsv$phenotype <- sample(phenotypeweights[,2], phenotypes, replace = TRUE, prob = phenotypeweights[,1])
phenotype_disease_tsv$disease <- sample(diseaseweights[,2], phenotypes, replace = TRUE, prob = diseaseweights[,1])
phenotype_disease_tsv <- filter(phenotype_disease_tsv, !is.na(phenotype), !is.na(disease)) %>% unique() %>% droplevels()



## Projects

### project_in_project.tsv

project_in_project_tsv <- data.frame(matrix(nrow=length(projects), ncol = 4))
colnames(project_in_project_tsv) <- c("parent_project_id_namespace", "parent_project_local_id",
                                "child_project_id_namespace", "child_project_local_id")
project_in_project_tsv$parent_project_id_namespace <- names(namespace)
project_in_project_tsv$parent_project_local_id <- names(mainproject)
project_in_project_tsv$child_project_id_namespace <- names(namespace)
project_in_project_tsv$child_project_local_id <- names(projects)

### collection_defined_by_project.tsv

collection_defined_by_project_tsv <- data.frame(matrix(nrow=0, ncol = 4))
colnames(collection_defined_by_project_tsv) <- c( "collection_id_namespace", "collection_local_id",
                                       "project_id_namespace", "project_local_id")


## Write out

dir.create(outputfoldername, showWarnings = F)
write.table(biosample_disease_tsv, paste(outputfoldername,"/biosample_disease.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(biosample_from_subject_tsv, paste(outputfoldername,"/biosample_from_subject.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(biosample_gene_tsv, paste(outputfoldername, "/biosample_gene.tsv", sep = ""), sep = "\t", row.names = F, col.names = T, quote = F, na = "")
write.table(biosample_in_collection_tsv, paste(outputfoldername,"/biosample_in_collection.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(biosample_substance_tsv, paste(outputfoldername, "/biosample_substance.tsv", sep = ""), sep = "\t", row.names = F, col.names = T, quote = F, na = "")
write.table(biosample_tsv, paste(outputfoldername,"/biosample.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(collection_compound_tsv, paste(outputfoldername,"/collection_compound.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(collection_defined_by_project_tsv, paste(outputfoldername,"/collection_defined_by_project.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(collection_anatomy_tsv, paste(outputfoldername,"/collection_anatomy.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(collection_disease_tsv, paste(outputfoldername,"/collection_disease.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(collection_gene_tsv, paste(outputfoldername,"/collection_gene.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(collection_in_collection_tsv, paste(outputfoldername,"/collection_in_collection.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(collection_protein_tsv, paste(outputfoldername,"/collection_protein.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(collection_phenotype_tsv, paste(outputfoldername,"/collection_phenotype.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(collection_substance_tsv, paste(outputfoldername,"/collection_substance.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(collection_taxonomy_tsv, paste(outputfoldername,"/collection_taxonomy.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(collection_tsv, paste(outputfoldername,"/collection.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(dcc_tsv, paste(outputfoldername,"/dcc.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(file_describes_biosample_tsv, paste(outputfoldername,"/file_describes_biosample.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(file_describes_collection_tsv, paste(outputfoldername,"/file_describes_collection.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(file_describes_subject_tsv, paste(outputfoldername,"/file_describes_subject.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(file_in_collection_tsv, paste(outputfoldername,"/file_in_collection.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(file_tsv, paste(outputfoldername,"/file.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(namespace_tsv, paste(outputfoldername,"/id_namespace.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(project_in_project_tsv, paste(outputfoldername,"/project_in_project.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(project_tsv, paste(outputfoldername,"/project.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(protein_gene_tsv, paste(outputfoldername,"/protein_gene.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(phenotype_disease_tsv, paste(outputfoldername,"/phenotype_disease.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(phenotype_gene_tsv, paste(outputfoldername,"/phenotype_gene.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(subject_disease_tsv, paste(outputfoldername,"/subject_disease.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "") 
write.table(subject_phenotype_tsv, paste(outputfoldername,"/subject_phenotype.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "") 
write.table(subject_in_collection_tsv, paste(outputfoldername,"/subject_in_collection.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(subject_race_tsv, paste(outputfoldername, "/subject_race.tsv", sep = ""), sep = "\t", row.names = F, col.names = T, quote = F, na = "")
write.table(subject_role_taxonomy_tsv, paste(outputfoldername,"/subject_role_taxonomy.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(subject_substance_tsv, paste(outputfoldername, "/subject_substance.tsv", sep = ""), sep = "\t", row.names = F, col.names = T, quote = F, na = "")
write.table(subject_tsv, paste(outputfoldername,"/subject.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")

## temp make my own CV tables
anatomy_table %>% unique() %>% droplevels() %>% write.table(paste(outputfoldername,"/anatomy.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
analysistype_table %>% unique() %>% droplevels() %>% write.table( paste(outputfoldername,"/analysis_type.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
rbind( assaytype_table, bioassaytype_table) %>% unique() %>% droplevels() %>% write.table( paste(outputfoldername,"/assay_type.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
compound_table %>% unique() %>% droplevels() %>% write.table( paste(outputfoldername,"/compound.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
datatype_table %>% unique() %>% droplevels() %>% write.table( paste(outputfoldername,"/data_type.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
disease_table %>% unique() %>% droplevels() %>% write.table( paste(outputfoldername,"/disease.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
fileformat_table %>% unique() %>% droplevels() %>% write.table(paste(outputfoldername,"/file_format.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
gene_table %>% unique() %>% droplevels() %>% write.table( paste(outputfoldername,"/gene.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
phenotype_table %>% unique() %>% droplevels() %>% write.table( paste(outputfoldername,"/phenotype.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
protein_table %>% unique() %>% droplevels() %>% write.table( paste(outputfoldername,"/protein.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
substance_table %>% unique() %>% droplevels() %>% write.table( paste(outputfoldername,"/substance.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
taxon_table %>% unique() %>% droplevels() %>% write.table( paste(outputfoldername,"/ncbi_taxonomy.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")



# system(paste("python3 CCfiles/build_term_tables.py ", outputfoldername, "/", sep = ""))
system(paste("cp C2M2_datapackage.json ", outputfoldername, sep = "" ))





