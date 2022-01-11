# Makeing synthetic  C2M2 data

# This tool creates a valid C2M2 instance. 
# Every settable option must have at least one value


###################################################################################
###################################################################################
# --------------------------------User Settable Options-------------------------------
## Customize your synthetic DCC
## If you run multiple times, be sure to change output folder to avoid overwrites

dcc <- c("procrastinomics"="the best dcc") # name=description
c2m2id <- "cfde_registry_dcc:test1" # must be a valid ID starting with `cfde_registry_dcc:`, currently: "test1", "hmp", "gtex", "motrpac", "kidsfirst", "metabolomics", "lincs", "4dn", "idg", "exrna", "sparc", "test2"
outputfoldername <- "newnovelvocabulary"
dccabbrev <- "procca"
website <- "http://acharbonneau.github.io/"
email <- "achar@ucdavis.edu"
submitter <- "Amanda Charbonneau"
# For localID
subjectprefix <- 'octo-'
fileprefix <- 'asdf-'
biosampleprefix <- 'jklh-'

## Point values: change to any other point value to increase/decrease size of data
### Number of files in file table
numfile <- 1192
### Number of biosamples in biosample table
numbio <- 63
### Number of subjects in subject table
numsub <- 5
### Ages of subjects
averageage <- 25
standarddev <- 10
### Maximum number of terms
anatomys <- 60
assays <- 6
bioassays <- 3
datatypes <- 4
diseases <- 8
fileformats <- 4
species <- c("NCBI:txid9606")
compressionformats <- c("format:3987", "format:3615")
subjectgranularitys <- 1 #must be a number between 0-6
subjectroles <- 1 # must be a number between 0-7
subjectethnicitys <- 2   #must be a number between 0-2
subjectsexes <-  3  #must be a number between 0-5
subjectraces <-  3  #must be a number between 0-5
### How randomized should metadata be on a scale from 1-100? 1= fewest possible combinations, 100= every unique combination
metadata_random <- 10
### Should the metadata appear roughly equally? (yes/no)
metadata_even <- "yes"

### Allow for missing non-required metadata values as a percentage of total values
### 15 = 15% missingness. Must be value between 0 and 100
filesmissing <- 10
biosamplesmissing <- 12
subjectsmissing <- 5
diseasemissing <- 40
### Date range. For generating creation dates
startdate <- '1999/01/01'
enddate <- '2000/01/01'
### Set subject granularity and subject role
### Can accept comma seperated lists. Script will randomly combine them

### Arrays: add/subtract values from lists to change complexity of data
### Arrays must be key value pairs: "title"="description"
namespace <- c("tag:procrastinomics.com,2021-07-23:"="the best namespace") #currently only a single namespace is supported
mainproject <- c("queso"="the main project")
projects <- c("taco"="a project with a hard shell", "burrito"="a rolled project", "nachos"="a spread out project")
collections <- c("arbitraryone"="a collection of files with a randomly chosen format", "arbitrarytwo"="a collection of things with a randomly chosen datatype")

#---------------------------End of Settable Options--------------------------------
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
### If you want to add/change/delete assays, file_formats, anatomy, disease, or data_types directly edit these files

system("wget https://osf.io/25qd6/download")

anatomy <- sample_n(read.csv("full_term_tables/anatomy.tsv", sep = "\t"), anatomys, replace = T) %>% unique() %>% droplevels()
assaytype <- read.csv("full_term_tables/assay_type.tsv", sep = "\t") %>% filter(str_detect(id, "OBI")) %>% sample_n(assays, replace = T) %>% unique() %>% droplevels()
bioassaytype <- read.csv("full_term_tables/assay_type.tsv", sep = "\t") %>% filter(str_detect(id, "OBI")) %>% sample_n(bioassays, replace = T) %>% unique() %>% droplevels()
datatype <- sample_n(read.csv("full_term_tables/data_type.tsv", sep = "\t"), datatypes, replace = T) %>% unique() %>% droplevels()
disease <- sample_n(read.csv("full_term_tables/disease.tsv", sep = "\t"), diseases, replace = T) %>% unique() %>% droplevels()
fileformat <- sample_n(read.csv("full_term_tables/file_format.tsv", sep = "\t"), fileformats, replace = T) %>% unique() %>% droplevels()
granularity <- sample_n(read.csv("CCfiles/subject_granularity.tsv", sep = "\t"), subjectgranularitys, replace = T) %>% unique() %>% droplevels()
role <- sample_n(read.csv("CCfiles/subject_role.tsv", sep = "\t"), subjectroles, replace = T) %>% unique() %>% droplevels()
multiplier <- numbers::mLCM(c(anatomys, assays, datatypes, diseases, fileformats, length(names(projects)), subjectgranularitys, subjectroles))
metadatasets <- data.frame( matrix(ncol = 8, nrow = multiplier))
colnames(metadatasets) <- c("anatomy", "assay", "bioassay", "data", "file", "project", "granularity", "role")
metadatasets$anatomy <- anatomy$id
metadatasets$assay <- assaytype$id
metadatasets$bioassay <- bioassaytype$id
metadatasets$data <- datatype$id
metadatasets$file <- fileformat$id
metadatasets$project <- names(projects)
metadatasets$granularity <- granularity$id
metadatasets$role  <- role$id
metadatasets <- metadatasets[sample(c(1:multiplier), size = 1 + round(multiplier*(metadata_random/100), digits = 0), replace = F),]
if (metadata_even == 'no') {
  metadatasets <- metadatasets[sample(1:nrow(metadatasets), size = nrow(metadatasets), replace = T),]
}

subjectethnicitytype <- sample_n(read.csv("CCfiles/subject_ethnicity.tsv", sep = "\t"), subjectethnicitys, replace = T) %>% unique() %>% droplevels()
subjectracetype <- sample_n(read.csv("CCfiles/subject_race.tsv", sep = "\t"), subjectraces, replace = T) %>% unique() %>% droplevels()
subjectsextype <- read.csv("CCfiles/subject_sex.tsv", sep = "\t", nrows = subjectsexes)
substancetype <- c("SID:5381226", "SID:49854366")
genetype <- c("ENSG00000010404", "ENSG00000265301")


## Base tables

filetable <- data.frame(matrix(nrow=numfile, ncol = 18))
colnames(filetable) <- c("id_namespace", "local_id", "project_id_namespace", 
                         "project_local_id", "persistent_id", "creation_time", 
                         "size_in_bytes", "uncompressed_size_in_bytes", 
                         "sha256", "md5", "filename", "file_format", "compression_format",
                         "data_type","assay_type", "mime_type", "bundle_collection_id_namespace",
                         "bundle_collection_local_id")
filetable$id_namespace <- names(namespace)
filetable$local_id <- paste(fileprefix, stri_rand_strings(numfile, 3, pattern = "[0-9]"), str_pad(c(1:numfile),6, pad="0"), sep="")
filetable$project_id_namespace <- names(namespace)
filetable$persistent_id <- NA
filetable$creation_time <- sample(seq(as.Date(startdate), as.Date(enddate), by="day"), numfile, replace = T)
filetable$size_in_bytes <- abs(sample(.Random.seed, numfile, replace = T))*2
filetable$sha256 <- stri_rand_strings(numfile, 64, pattern = "[a-fA-F0-9]")
filetable$md5 <- ""
filetable$filename <- paste(stri_rand_strings(numfile, 3, pattern = "[A-Z]"), "_",stri_rand_strings(numfile, 6, pattern = "[0-9]"), sep="") 
filetable[,c(4, 12, 14, 15)] <- metadatasets[sample(1:nrow(metadatasets), numfile, replace = T),c(6,5,4,2)]
filetable$mime_type <- NA
compformatweights <- rep(10/length(compressionformats), length(compressionformats))
filetable$compression_format <- sample(c(NA, compressionformats), size = numfile, replace = T, prob = c(90, compformatweights))
filetable$uncompressed_size_in_bytes[is.na(filetable$compression_format) == F] <- filetable$size_in_bytes[is.na(filetable$compression_format) == F] + abs(sample(.Random.seed))
filetable$bundle_collection_id_namespace <- NA
filetable$bundle_collection_local_id <- NA
  
if (filesmissing > 0) {
  remove <- round(numfile*(filesmissing/100))
  filetable$creation_time[c(sample(1:numfile, remove, replace = F))] <- NA
  filetable$size_in_bytes[c(sample(1:numfile, remove, replace = F))] <- NA
  filetable$sha256[c(sample(1:numfile, remove, replace = F))] <- NA
  filetable$file_format[c(sample(1:numfile, remove, replace = F))] <- NA
  filetable$data_type[c(sample(1:numfile, remove, replace = F))] <- NA
  filetable$assay_type[c(sample(1:numfile, remove, replace = F))] <- NA
}


biosampletable <- data.frame(matrix(nrow=numbio, ncol = 8))
colnames(biosampletable) <- c("id_namespace", "local_id", "project_id_namespace", 
                              "project_local_id", "persistent_id", "creation_time",
                              "assay_type", "anatomy")
biosampletable$id_namespace <- names(namespace)
biosampletable$local_id <- paste(biosampleprefix, stri_rand_strings(numbio, 3, pattern = "[0-9]"), str_pad(c(1:numbio),6, pad="0"), sep="")
biosampletable$project_id_namespace <- names(namespace)
biosampletable$persistent_id <- ""
biosampletable$creation_time <- sample(seq(as.Date(startdate), as.Date(enddate), by="day"), numbio, replace = T)
biosampletable[,c(4, 7, 8)] <- metadatasets[sample(1:nrow(metadatasets), numbio, replace = T),c(6,3,1)]
if (biosamplesmissing > 0) {
  remove <- round(numbio*(biosamplesmissing/100))
  biosampletable$creation_time[c(sample(1:numbio, remove, replace = T))] <- NA
  biosampletable$assay_type[c(sample(1:numbio, remove, replace = T))] <- NA
  biosampletable$anatomy[c(sample(1:numbio, remove, replace = T))] <- NA
}  


subjecttable <- data.frame(matrix(nrow=numsub, ncol = 10))
colnames(subjecttable) <- c("id_namespace", "local_id", "project_id_namespace", 
                            "project_local_id", "persistent_id", "creation_time",
                            "granularity", "sex", "ethnicity", "age_at_enrollment")
subjecttable$id_namespace <- names(namespace)
subjecttable$local_id <- paste(subjectprefix, stri_rand_strings(numsub, 3, pattern = "[0-9]"), str_pad(c(1:numsub),6, pad="0"), sep="")
subjecttable$project_id_namespace <- names(namespace)
subjecttable$project_local_id <- sample(names(projects), numsub, replace = T)
subjecttable$persistent_id <- ""
subjecttable$creation_time <- sample(seq(as.Date(startdate), as.Date(enddate), by="day"), numsub, replace = T)
subjecttable$granularity <- sample(metadatasets$granularity, numsub, replace = T)
subjecttable$sex <- sample(subjectsextype$id, numsub, replace = T)
subjecttable$ethnicity <- sample(subjectethnicitytype$id, numsub, replace = T)
subjecttable$age_at_enrollment <- rnorm(n = numsub, mean = averageage, sd = standarddev) %>% round(digits = 2) %>% abs()

if (subjectsmissing > 0) {
  remove <- round(numsub*(subjectsmissing/100))
  subjecttable$creation_time[c(sample(1:numsub, remove, replace = T))] <- NA
  subjecttable$age_at_enrollment[c(sample(1:numsub, remove, replace = T))] <- NA
  subjecttable$sex[c(sample(1:numsub, remove, replace = T))] <- NA
  subjecttable$ethnicity[c(sample(1:numsub, remove, replace = T))] <- NA
}  

collectiontable <- data.frame(matrix(nrow=length(collections), ncol = 7))
colnames(collectiontable) <- c("id_namespace", "local_id", "persistent_id", 
                               "creation_time", "abbreviation", "name", "description")

collectiontable$id_namespace <- names(namespace)
collectiontable$local_id <- paste('coll-', stri_rand_strings(length(collections), 3, pattern = "[0-9]"), str_pad(c(1:length(collections)),6, pad="0"), sep="")
collectiontable$persistent_id <-  ""
collectiontable$creation_time <- sample(seq(as.Date(startdate), as.Date(enddate), by="day"), length(collections), replace = T)
collectiontable$abbreviation <- names(collections)
collectiontable$name <- names(collections)
collectiontable$description <- collections
bundles <- sample(filetable$local_id[is.na(filetable$compression_format) == F], size = length(collections))
for (bundle in length(bundles)){
  filetable$bundle_collection_local_id[filetable$local_id==bundles[bundle]] <- collectiontable$local_id[bundle]
  filetable$bundle_collection_id_namespace[filetable$local_id==bundles[bundle]] <- collectiontable$id_namespace[bundle]
  }


namespacetable <- as.data.frame(t(as.matrix( c(names(namespace), dccabbrev, names(dcc), dcc)) ))
colnames(namespacetable) <- c("id", "abbreviation", "name", "description")

contacttable <- as.data.frame(t(as.matrix(c(c2m2id, names(dcc), dccabbrev, dcc, email, submitter, website, names(namespace), names(mainproject)))))
colnames(contacttable) <- c("id", "dcc_name", "dcc_abbreviation", "dcc_description", "contact_email", "contact_name", "dcc_url", "project_id_namespace", "project_local_id")

allprojects <- c(mainproject, projects)
projecttable <- data.frame(matrix(nrow=length(allprojects), ncol = 7))
colnames(projecttable) <- c("id_namespace", "local_id", "persistent_id", 
                            "creation_time", "abbreviation", "name", "description")

projecttable$id_namespace <- names(namespace)
projecttable$local_id <- names(allprojects)
projecttable$persistent_id <-  ""
projecttable$creation_time <- sample(seq(as.Date(startdate), as.Date(enddate), by="day"), length(allprojects))
projecttable$abbreviation <- names(allprojects)
projecttable$name <- names(allprojects)
projecttable$description <- allprojects


## Association Tables

biosamplefromsubject <- right_join(biosampletable, subjecttable, by = c("id_namespace", "project_local_id")) %>% 
  group_by(local_id.x) %>% slice(1) %>% 
  mutate(age_at_sampling= abs(round(age_at_enrollment + (rnorm(n = 1, mean = 2, sd = .5)), digits = 2))) %>%
  select(project_id_namespace.x, local_id.x, project_id_namespace.y, local_id.y, age_at_sampling)

colnames(biosamplefromsubject) <- c( "biosample_id_namespace", "biosample_local_id",
                                     "subject_id_namespace", "subject_local_id", "age_at_sampling")


if (subjectsmissing > 0 & biosamplesmissing > 0) {
  remove <- round(numsub*(subjectsmissing/100)*(biosamplesmissing/100))
  biosamplefromsubject$biosample_local_id[c(sample(1:numbio, remove, replace = T))] <- NA
  biosamplefromsubject <- filter(biosamplefromsubject, !is.na(biosample_local_id))
} 


filedescribesbiosample <- right_join(filetable, biosampletable, by = c("id_namespace", "project_local_id")) %>% 
  group_by(local_id.x) %>% slice(1) %>% 
  select(project_id_namespace.x, local_id.x, project_id_namespace.y, local_id.y)

colnames(filedescribesbiosample) <- c( "file_id_namespace", "file_local_id",
                                       "biosample_id_namespace", "biosample_local_id")

if (filesmissing > 0 & biosamplesmissing > 0) {
  remove <- round(numfile*(filesmissing/100)*(biosamplesmissing/100))
  filedescribesbiosample$file_local_id[c(sample(1:numfile, remove, replace = T))] <- NA
  filedescribesbiosample <- filter(filedescribesbiosample, !is.na(file_local_id))
}   

filedescribessubject <- left_join(filedescribesbiosample, biosamplefromsubject, by=c()) %>%
  select("file_id_namespace", "file_local_id","subject_id_namespace", "subject_local_id")

if (filesmissing > 0 & subjectsmissing > 0) {
  remove <- round(numfile*(filesmissing/100)*(subjectsmissing/100))
  filedescribessubject$file_local_id[c(sample(1:length(filedescribessubject$file_local_id), remove, replace = T))] <- NA
  filedescribessubject <- filter(filedescribessubject, !is.na(file_local_id))
} 

subjectsubstance <- data.frame(matrix(nrow=numsub/3, ncol = 3))
colnames(subjectsubstance) <- c( "subject_id_namespace", "subject_local_id",
                                        "substance")

subjectsubstance$subject_id_namespace <- names(namespace)
subjectsubstance$subject_local_id <- sample(subjecttable$local_id, replace = F, size = numsub/3)
subjectsubstance$substance <- sample(substancetype, numsub/3, replace = T)

biosamplesubstance <- data.frame(matrix(nrow=numbio/3, ncol = 3))
colnames(biosamplesubstance) <- c( "biosample_id_namespace", "biosample_local_id",
                                 "substance")
biosamplesubstance$biosample_id_namespace <- names(namespace)
biosamplesubstance$biosample_local_id <- sample(biosampletable$local_id, replace = F, size = numbio/3)
biosamplesubstance$substance <- sample(substancetype, numbio/3, replace = T)


biosamplegene <- data.frame(matrix(nrow=numbio/3, ncol = 3))
colnames(biosamplegene) <- c( "biosample_id_namespace", "biosample_local_id",
                                   "gene")
biosamplegene$biosample_id_namespace <- names(namespace)
biosamplegene$biosample_local_id <- sample(biosampletable$local_id, replace = F, size = numbio/3)
biosamplegene$gene <- sample(genetype, numbio/3, replace = T)


subjectrace <- data.frame(matrix(nrow=numsub, ncol = 3))
colnames(subjectrace) <- c( "subject_id_namespace", "subject_local_id", "race")
subjectrace$subject_id_namespace <- names(namespace)
subjectrace$subject_local_id <- subjecttable$local_id
subjectrace$race <- sample(subjectracetype$id, numsub, replace = T)

### Collections

randformat <- filter(filetable, file_format==sample(fileformat$id,1), local_id!=bundles)
randatatype <- filter(filetable, data_type==sample(datatype$id,1), local_id!=bundles)
fileincollection <- data.frame(matrix(nrow=length(randformat$local_id) + length(randatatype$local_id), ncol = 4))
colnames(fileincollection) <- c( "file_id_namespace", "file_local_id",
                                 "collection_id_namespace", "collection_local_id")

fileincollection$file_id_namespace <- names(namespace)
fileincollection$file_local_id <- c(randformat$local_id, randatatype$local_id)
fileincollection$collection_id_namespace <- names(namespace)
fileincollection$collection_local_id <- c(rep(collectiontable$local_id[1],length(randformat$local_id)), rep(collectiontable$local_id[2],length(randatatype$local_id)))

filedescribescollection <- data.frame(matrix(nrow=0, ncol = 4))
colnames(filedescribescollection) <- c( "file_id_namespace", "file_local_id",
                                        "collection_id_namespace", "collection_local_id")

biosampleincollection <- left_join(fileincollection, filedescribesbiosample) %>%
  select(biosample_id_namespace, biosample_local_id, collection_id_namespace, collection_local_id) %>% 
  filter(!is.na(biosample_id_namespace)) %>% unique()

subjectincollection <- left_join(fileincollection, filedescribessubject) %>%
  select(subject_id_namespace, subject_local_id, collection_id_namespace, collection_local_id) %>%
  filter(!is.na(subject_id_namespace)) %>% unique()

collectionincollection <- data.frame(matrix(nrow=0, ncol = 4))
colnames(collectionincollection) <- c( "superset_collection_id_namespace", "superset_collection_local_id",
                                       "subset_collection_id_namespace", "subset_collection_local_id")


### Projects

projectinproject <- data.frame(matrix(nrow=length(projects), ncol = 4))
colnames(projectinproject) <- c("parent_project_id_namespace", "parent_project_local_id",
                                "child_project_id_namespace", "child_project_local_id")
projectinproject$parent_project_id_namespace <- names(namespace)
projectinproject$parent_project_local_id <- names(mainproject)
projectinproject$child_project_id_namespace <- names(namespace)
projectinproject$child_project_local_id <- names(projects)

collectiondefbyproject <- data.frame(matrix(nrow=0, ncol = 4))
colnames(collectiondefbyproject) <- c( "collection_id_namespace", "collection_local_id",
                                       "project_id_namespace", "project_local_id")

### Disease

subjectdisease <- data.frame(matrix(nrow=length(subjecttable$local_id), ncol = 3))
colnames(subjectdisease) <- c("subject_id_namespace", "subject_local_id",
                              "disease")
subjectdisease$subject_id_namespace <- names(namespace)
subjectdisease$subject_local_id <- subjecttable$local_id
subjectdisease$disease <- sample(disease$id, length(subjecttable$local_id), replace = T)

if (diseasemissing > 0) {
  remove <- round(length(subjectdisease$subject_local_id)*(diseasemissing/100))
  subjectdisease$disease[c(sample(1:length(subjecttable$local_id), remove, replace = T))] <- NA
  subjectdisease <- filter(subjectdisease, !is.na(disease))
} 

biosampledisease <- full_join(subjectdisease, biosamplefromsubject) %>% 
  select(biosample_id_namespace, biosample_local_id, disease) %>%
  filter(!is.na(biosample_local_id)) %>% filter(!is.na(disease))

### Other

subjectroletaxonomy <- data.frame(matrix(nrow=numsub, ncol = 4))
colnames(subjectroletaxonomy) <- c("subject_id_namespace", "subject_local_id", "role_id", "taxonomy_id")

subjectroletaxonomy$subject_id_namespace <- names(namespace)
subjectroletaxonomy$subject_local_id <- subjecttable$local_id
subjectroletaxonomy$role_id <- sample(metadatasets$role, numsub, replace = T)
subjectroletaxonomy$taxonomy_id <- sample(species, numsub, replace = T)

## Reference tables


substance <- data.frame(matrix(nrow=length(substancetype), ncol = 5))
colnames(substance) <- c( "id", "name", "description", "synonyms", "compound")
substance$id <- substancetype
substance$name <- c("(1-(Furan-2-ylmethylamino)-3-[3-(trifluoromethyl)pyrazol-1-yl]propan-2-ol)", "2-acetyloxybenzoic acid")
substance$description <- c("description words", "description words")
substance$synonyms <- c(NA, '["asprin"]')
substance$compound <- c("CID:2809225", "CID:2244")

compound <- data.frame(matrix(nrow=length(unique(substance$compound)), ncol = 4))
colnames(compound) <- c( "id", "name", "description", "synonyms")
compound$id <- unique(substance$compound)
compound$name <-  c("(1-(Furan-2-ylmethylamino)-3-[3-(trifluoromethyl)pyrazol-1-yl]propan-2-ol)", "2-acetyloxybenzoic acid")
compound$description <- c("description words", "description words")
compound$synonyms <- c(NA, '["asprin"]')

gene <- data.frame(matrix(nrow=length(genetype), ncol = 5))
colnames(gene) <- c( "id", "name", "description", "synonyms", "organism")
gene$id <- genetype
gene$name <- c("IDS", "MIR548AD")
gene$description <- c("iduronate 2-sulfatase", "microRNA 548ad")
gene$synonyms <- c('["ID2S", "SIDS"]', '["hsa-mir-548ad"]')
gene$organism <- c("NCBI:txid9606", "NCBI:txid9606")


## Write out

dir.create(outputfoldername, showWarnings = F)
write.table(biosamplefromsubject, paste(outputfoldername,"/biosample_from_subject.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(biosampleincollection, paste(outputfoldername,"/biosample_in_collection.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(collectiontable, paste(outputfoldername,"/collection.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(filetable, paste(outputfoldername,"/file.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(filedescribesbiosample, paste(outputfoldername,"/file_describes_biosample.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(filedescribessubject, paste(outputfoldername,"/file_describes_subject.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(fileincollection, paste(outputfoldername,"/file_in_collection.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(namespacetable, paste(outputfoldername,"/id_namespace.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(contacttable, paste(outputfoldername,"/dcc.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(projecttable, paste(outputfoldername,"/project.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(projectinproject, paste(outputfoldername,"/project_in_project.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(subjecttable, paste(outputfoldername,"/subject.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(subjectincollection, paste(outputfoldername,"/subject_in_collection.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(subjectroletaxonomy, paste(outputfoldername,"/subject_role_taxonomy.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(collectiondefbyproject, paste(outputfoldername,"/collection_defined_by_project.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(collectionincollection, paste(outputfoldername,"/collection_in_collection.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(filedescribescollection, paste(outputfoldername,"/file_describes_collection.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(subjectdisease, paste(outputfoldername,"/subject_disease.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "") 
write.table(biosampledisease, paste(outputfoldername,"/biosample_disease.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(biosampletable, paste(outputfoldername,"/biosample.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(subjectsubstance, paste(outputfoldername, "/subject_substance.tsv", sep = ""), sep = "\t", row.names = F, col.names = T, quote = F, na = "")
write.table(biosamplesubstance, paste(outputfoldername, "/biosample_substance.tsv", sep = ""), sep = "\t", row.names = F, col.names = T, quote = F, na = "")
write.table(biosamplegene, paste(outputfoldername, "/biosample_gene.tsv", sep = ""), sep = "\t", row.names = F, col.names = T, quote = F, na = "")
write.table(subjectrace, paste(outputfoldername, "/subject_race.tsv", sep = ""), sep = "\t", row.names = F, col.names = T, quote = F, na = "")
write.table(compound, paste(outputfoldername, "/compound.tsv", sep = ""), sep = "\t", row.names = F, col.names = T, quote = F, na = "")
write.table(gene, paste(outputfoldername, "/gene.tsv", sep = ""), sep = "\t", row.names = F, col.names = T, quote = F, na = "")
write.table(substance, paste(outputfoldername, "/substance.tsv", sep = ""), sep = "\t", row.names = F, col.names = T, quote = F, na = "")



system(paste("python3 CCfiles/build_term_tables.py ", outputfoldername, "/", sep = ""))
system(paste("cp CCfiles/C2M2_datapackage.json ", outputfoldername, sep = "" ))





