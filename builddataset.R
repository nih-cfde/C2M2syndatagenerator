# Makeing synthetic  C2M2 data

# This tool creates a valid C2M2 instance. 
# Every settable option must have at least one value


###################################################################################
###################################################################################
# --------------------------------User Settable Options-------------------------------
## Customize your synthetic DCC
## If you run multiple times, be sure to change output folder to avoid overwrites

dcc <- c("procrastinomics"="the best dcc") # name=description
outputfoldername <- "disease1000"
dccabbrev <- "procca"
website <- "http://acharbonneau.github.io/"
email <- "achar@ucdavis.edu"
submitter <- "Amanda Charbonneau"
subjectprefix <- 'iuyt-'
fileprefix <- 'asdf-'
biosampleprefix <- 'jklh-'
  
## The early 2021 model does not have disease, post-July 2021 does. 
## 'yes' for disease support, 'no' for the older model
supportfordisease <- 'yes'
  
## Point values: change to any other point value to increase/decrease size of data
### Number of files in file table
numfile <- 1000
### Number of biosamples in biosample table
numbio <- 57
### Number of subjects in subject table
numsub <- 30
### Maximum number of assays, file_formats, anatomy, disease, and data_types
### Generates a random number of each, up to the max
maxtypes <- 15
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
subjectgranularity <- c("cfde_subject_granularity:0")
subjectrole <- c("NCBI:txid9606")

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


## Files with input data
### If you want to add/change/delete assays, file_formats, anatomy, disease, or data_types directly edit these files
types <- sample(1:maxtypes, 5, replace = T)

anatomy <- sample_n(read.csv("CVtables/anatomy.tsv", sep = "\t"), types[1], replace = T) %>% unique() %>% droplevels()
assaytype <- read.csv("CVtables/assay_type.tsv", sep = "\t") %>% filter(str_detect(id, "OBI")) %>% sample_n(types[2]*2, replace = T) %>% unique() %>% droplevels()
datatype <- sample_n(read.csv("CVtables/data_type.tsv", sep = "\t"), types[3], replace = T) %>% unique() %>% droplevels()
disease <- sample_n(read.csv("CVtables/disease.tsv", sep = "\t"), types[4], replace = T) %>% unique() %>% droplevels()
fileformat <- sample_n(read.csv("CVtables/file_format.tsv", sep = "\t"), types[5], replace = T) %>% unique() %>% droplevels()

## Base tables

filetable <- data.frame(matrix(nrow=numfile, ncol = 15))
colnames(filetable) <- c("id_namespace", "local_id", "project_id_namespace", 
                         "project_local_id", "persistent_id", "creation_time", 
                         "size_in_bytes", "uncompressed_size_in_bytes", 
                         "sha256", "md5", "filename", "file_format", "data_type",
                         "assay_type", "mime_type")
filetable$id_namespace <- names(namespace)
filetable$local_id <- paste(fileprefix, stri_rand_strings(numfile, 3, pattern = "[0-9]"), str_pad(c(1:numfile),6, pad="0"), sep="")
filetable$project_id_namespace <- names(namespace)
filetable$project_local_id <- sample(names(projects), numfile, replace = T)
filetable$persistent_id <- ""
filetable$creation_time <- sample(seq(as.Date(startdate), as.Date(enddate), by="day"), numfile, replace = T)
filetable$size_in_bytes <- abs(sample(.Random.seed, numfile, replace = T))*2
filetable$uncompressed_size_in_bytes <- ""
filetable$sha256 <- stri_rand_strings(numfile, 64, pattern = "[a-fA-F0-9]")
filetable$md5 <- ""
filetable$filename <- paste(stri_rand_strings(numfile, 3, pattern = "[A-Z]"), "_",stri_rand_strings(numfile, 6, pattern = "[0-9]"), sep="") 
filetable$file_format <- sample(fileformat$id, numfile, replace = T)
filetable$data_type <- sample(datatype$id, numfile, replace = T)
filetable$assay_type <-  sample(assaytype$id[1:types[2]], numfile, replace = T)
filetable$mime_type <-""

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
biosampletable$project_local_id <- sample(names(projects), numbio, replace = T)
biosampletable$persistent_id <- ""
biosampletable$creation_time <- sample(seq(as.Date(startdate), as.Date(enddate), by="day"), numbio, replace = T)
biosampletable$assay_type <- sample(assaytype$id[], numbio, replace = T)
biosampletable$anatomy <- sample(anatomy$id, numbio, replace = T)
if (biosamplesmissing > 0) {
  remove <- round(numbio*(biosamplesmissing/100))
  biosampletable$creation_time[c(sample(1:numbio, remove, replace = T))] <- NA
  biosampletable$assay_type[c(sample(1:numbio, remove, replace = T))] <- NA
  biosampletable$anatomy[c(sample(1:numbio, remove, replace = T))] <- NA
}  
                         
                         
subjecttable <- data.frame(matrix(nrow=numsub, ncol = 7))
colnames(subjecttable) <- c("id_namespace", "local_id", "project_id_namespace", 
                            "project_local_id", "persistent_id", "creation_time",
                            "granularity")
subjecttable$id_namespace <- names(namespace)
subjecttable$local_id <- paste(subjectprefix, stri_rand_strings(numsub, 3, pattern = "[0-9]"), str_pad(c(1:numsub),6, pad="0"), sep="")
subjecttable$project_id_namespace <- names(namespace)
subjecttable$project_local_id <- sample(names(projects), numsub, replace = T)
subjecttable$persistent_id <- ""
subjecttable$creation_time <- sample(seq(as.Date(startdate), as.Date(enddate), by="day"), numsub, replace = T)
subjecttable$granularity <- sample(subjectgranularity, numsub, replace = T)

if (subjectsmissing > 0) {
  remove <- round(numsub*(subjectsmissing/100))
  subjecttable$creation_time[c(sample(1:numsub, remove, replace = T))] <- NA
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

namespacetable <- as.data.frame(t(as.matrix( c(namespace, dccabbrev, names(dcc), dcc)) ))
colnames(namespacetable) <- c("id", "abbreviation", "name", "description")

contacttable <- as.data.frame(t(as.matrix(c(email, submitter, names(namespace), names(mainproject), dccabbrev, names(dcc), dcc, website))))
colnames(contacttable) <- c("contact_email", "contact_name", "project_id_namespace", "project_local_id", "dcc_abbreviation", "dcc_name", "dcc_description", "dcc_url")

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

biosamplefromsubject <- data.frame(matrix(nrow=numbio, ncol = 4))
colnames(biosamplefromsubject) <- c( "biosample_id_namespace", "biosample_local_id",
                                     "subject_id_namespace", "subject_local_id")

biosamplefromsubject$biosample_id_namespace <- names(namespace)
biosamplefromsubject$biosample_local_id <- biosampletable$local_id
biosamplefromsubject$subject_id_namespace <- names(namespace)
biosamplefromsubject$subject_local_id <- sample(subjecttable$local_id, numbio, replace = T)

if (subjectsmissing > 0 & biosamplesmissing > 0) {
  remove <- round(numsub*(subjectsmissing/100)*(biosamplesmissing/100))
  biosamplefromsubject$biosample_local_id[c(sample(1:numbio, remove, replace = T))] <- NA
  biosamplefromsubject <- filter(biosamplefromsubject, !is.na(biosample_local_id))
} 

filedescribesbiosample <- data.frame(matrix(nrow=numfile, ncol = 4))
colnames(filedescribesbiosample) <- c( "file_id_namespace", "file_local_id",
                                       "biosample_id_namespace", "biosample_local_id")

filedescribesbiosample$file_id_namespace <- names(namespace)
filedescribesbiosample$file_local_id <- filetable$local_id
filedescribesbiosample$biosample_id_namespace <- names(namespace)
filedescribesbiosample$biosample_local_id <- sample(biosampletable$local_id, numfile, replace = T) 

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

### Collections

randformat <- filter(filetable, file_format==sample(fileformat$id,1))
randatatype <- filter(filetable, data_type==sample(datatype$id,1))
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
subjectroletaxonomy$role_id <- "cfde_subject_role:0"
subjectroletaxonomy$taxonomy_id <- sample(subjectrole, numsub, replace = T)

## Reference tables

ncbi <- data.frame(matrix(nrow=1, ncol = 4))
colnames(ncbi) <- c( "id", "clade", "name", "description")
ncbi[1,] <- c("NCBI:txid9606", "species", "Homo sapiens", "human")


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
write.table(contacttable, paste(outputfoldername,"/primary_dcc_contact.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(projecttable, paste(outputfoldername,"/project.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(projectinproject, paste(outputfoldername,"/project_in_project.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(subjecttable, paste(outputfoldername,"/subject.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(subjectincollection, paste(outputfoldername,"/subject_in_collection.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(subjectroletaxonomy, paste(outputfoldername,"/subject_role_taxonomy.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(collectiondefbyproject, paste(outputfoldername,"/collection_defined_by_project.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(collectionincollection, paste(outputfoldername,"/collection_in_collection.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(ncbi, paste(outputfoldername,"/ncbi_taxonomy.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
write.table(filedescribescollection, paste(outputfoldername,"/file_describes_collection.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
system(paste("python3 build_term_tables.py ", outputfoldername, "/", sep = ""))

if (supportfordisease=='yes') {
  write.table(subjectdisease, paste(outputfoldername,"/subject_disease.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
  write.table(biosampledisease, paste(outputfoldername,"/biosample_disease.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
  write.table(biosampletable, paste(outputfoldername,"/biosample.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
  system(paste("cp ./C2M2_July_datapackage.json ", outputfoldername, sep = "" ))
  } 
else {
  write.table(select(biosampletable, -assay_type), paste(outputfoldername,"/biosample.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
  download.file("https://osf.io/e5tc2/download", paste(outputfoldername,"/C2M2_datapackage.json", sep = ""))
  system(paste("rm ", outputfoldername, "/disease.tsv", sep = ""))
  read.csv(paste(outputfoldername, "/assay_type.tsv", sep = ""), sep = "\t") %>% select(-synonyms) %>% write.table(paste(outputfoldername, "/assay_type.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
  read.csv(paste(outputfoldername, "/anatomy.tsv", sep = ""), sep = "\t") %>% select(-synonyms) %>% write.table(paste(outputfoldername, "/anatomy.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
  read.csv(paste(outputfoldername, "/data_type.tsv", sep = ""), sep = "\t") %>% select(-synonyms) %>% write.table(paste(outputfoldername, "/data_type.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
  read.csv(paste(outputfoldername, "/file_format.tsv", sep = ""), sep = "\t") %>% select(-synonyms) %>% write.table(paste(outputfoldername, "/file_format.tsv", sep = ""), sep ="\t", row.names = F, col.names = T, quote = F, na = "")
}
