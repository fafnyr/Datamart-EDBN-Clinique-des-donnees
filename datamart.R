############################################################################################################################
############################################# Script to modify the datamart ################################################
############################################################################################################################
# Warning : all text-mining is done in FRENCH

#set your directory
setwd(dir = "Y:/CIC/EC-CIC/STAGES/StageM1_LCM2019/Package edbn_mart/")

install.packages("EDBNmart_0.0.0.9000.tar.gz", type = "source", repos = NULL)

#open the variable containing the datamart (opened here with an .RData file)
library(readr)
datamart_exemple <- read_csv("~/datamart_exemple.csv")
library(stringi)
datamart_exemple[["TEXTE_AFFICHAGE"]] <- stri_trans_general(datamart_exemple[["TEXTE_AFFICHAGE"]], "latin-ascii")
datamart_exemple[["TITRE"]] <- stri_trans_general(datamart_exemple[["TITRE"]], "latin-ascii")

#datamart_exemple <- readRDS(file = "datamart_exemple.rds")

# If datamart variable has another name, changing it for the rest of the script
datamart <- datamart_exemple

##########################################################
##################### Functions #########################
##########################################################
library(EDBNmart)

##########################################################
############## Creation of Better dataframe ##############
##########################################################
library(data.table)
datamart <- as.data.table(datamart)

library(textreadr)
library(dplyr)
library(boilerpipeR)

#suppression of XML and HTML tags
datamart <- datamart[,TEXTE_AFFICHAGE := lapply(TEXTE_AFFICHAGE, function(row) html_xmltotext(rowdoc = row))]
datamart[["TEXTE_AFFICHAGE"]] <- as.character(datamart[["TEXTE_AFFICHAGE"]])

#################################################################################################################################
################# Deidentification (optional, you can skip to the the next part if you do not want to identify) #################
#################################################################################################################################

library(openssl)
library(stringr)

#transform a "date of birth" variable into an "age" variable
datamart[["AGE"]] <- datamart[["DATENAIS"]]
datamart <- datamart[,DATENAIS := lapply(X = DATENAIS,
                                       function(row) gsub(pattern = ".*(\\d{4})-(\\d{2})-(\\d{2}).*",
                                                          replacement = "\\3/\\2/\\1",
                                                          x = row,
                                                          ignore.case = TRUE))]
datamart[["DATENAIS"]] <- as.character(datamart[["DATENAIS"]])
datamart <- datamart[,AGE := lapply(X = AGE,
                                  function(row) round(as.numeric(Sys.Date() - as.Date(row, format='%d/%m/%Y'))/365.25,0))]
datamart[["AGE"]] <- as.integer(datamart[["AGE"]])

#convert dates into a recognised format
datamart <- datamart[,TEXTE_AFFICHAGE := lapply(TEXTE_AFFICHAGE,
                                              function(row) better_calendar(totranslate = row))]
datamart[["TEXTE_AFFICHAGE"]] <- as.character(datamart[["TEXTE_AFFICHAGE"]])

#remove dates of birth from the text
datamart <- datamart[,TEXTE_AFFICHAGE := lapply(TEXTE_AFFICHAGE,
                                              function(row) gsub(
                                                pattern = DATENAIS,
                                                replacement = paste("[",AGE,"]",sep = ""),
                                                x = row,
                                                ignore.case = TRUE))]
datamart[["TEXTE_AFFICHAGE"]] <- as.character(datamart[["TEXTE_AFFICHAGE"]])

#offsets preparation
offsetdates <- cbind.data.frame(datamart[["IPP"]],datamart[["IPP"]])
colnames(offsetdates) <- c("IPP", "WEEK_OFFSET")
offsetdates <- distinct(offsetdates, IPP, .keep_all = TRUE)
offsetdates <- as.data.table(offsetdates)
offsetdates <- offsetdates[,WEEK_OFFSET := lapply(WEEK_OFFSET, function(row) round(x = runif(n = 1, min = 4, max = 14), digits = 0))]
offsetdates[["IPP"]] <- as.character(offsetdates[["IPP"]])
offsetdates[["WEEK_OFFSET"]] <- as.integer(offsetdates[["WEEK_OFFSET"]])
datamart <- full_join(x = datamart, y = offsetdates, by = "IPP")
datamart <- as.data.table(datamart)
rm(offsetdates)


#offset dates in the text
datamart[["TEXTE_AFFICHAGE"]] <- apply(X = datamart, MARGIN = 1,
                                      function(row_dt) offset_date_text(textpurified = row_dt[["TEXTE_AFFICHAGE"]],
                                                                        offset_date = row_dt[["WEEK_OFFSET"]]))
#offset other dates (present in structured format)
datamart[["DATE_DECES"]] <- as.Date(datamart[["DATE_DECES"]], "%Y/%m/%d")
datamart[["DATE_DECES"]] <- datamart[["DATE_DECES"]]+as.integer(datamart[["WEEK_OFFSET"]])*7
datamart[["DATE_MAJ"]] <- as.Date(datamart[["DATE_MAJ"]], "%Y/%m/%d")
datamart[["DATE_MAJ"]] <- datamart[["DATE_MAJ"]]+as.integer(datamart[["WEEK_OFFSET"]])*7
datamart[["DATESIGNATURE"]] <- as.Date(datamart[["DATESIGNATURE"]], "%Y/%m/%d")
datamart[["DATESIGNATURE"]] <- datamart[["DATESIGNATURE"]]+as.integer(datamart[["WEEK_OFFSET"]])*7

#deidentification

returnblind <- blindeye(df_to_anon = datamart, algo = "sha512", size_key = 64)
#returnblind is for the CdD or the person responsible of the identifying data

##########################################################
####################### Save list #######################
##########################################################
saveRDS(returnblind, file = "returnblind.rds")
#returnblind <- readRDS(file = "returnblind.rds")

datamart <- returnblind$df
rm(returnblind)
#datamart is for the user outside of the CdD


##########################################################
################# Reidentification #######################
##########################################################

#df_test2 <- getting_sight_back(df_to_anon = df_test1, algo = "sha512", liste_anon = listeanon, hashkey = saltkey)


##########################################################################
################# Separation of datamart into 2 variable ################
##########################################################################
.rs.restartR()

datamart.exam_bio <- datamart[grep("Compte rendu examens biologiques", datamart[["TEXTE_AFFICHAGE"]], ignore.case = TRUE),]
datamart.anti_exam_bio <- datamart[grep("Compte rendu examens biologiques", datamart[["TEXTE_AFFICHAGE"]], ignore.case = TRUE, invert = TRUE),]


##########################################################
################# Creation of Blood tables ################
##########################################################
datamart.exam_bio[["TEXTE_AFFICHAGE"]] <- as.character(datamart.exam_bio[["TEXTE_AFFICHAGE"]])

datamart.exam_bio.result <- do.call("rbind", apply(datamart.exam_bio, 1, bio_report_to_table))
rm(datamart.exam_bio)
library(dplyr)
library(data.table)

# remove a particular word used to repeat the name of the variable and its value with a different units of measure
for (rowD in 1:nrow(datamart.exam_bio.result)) {
  if(grepl(pattern = "soit:", x = datamart.exam_bio.result[["Nom"]][rowD])){
    datamart.exam_bio.result[["Nom"]][rowD] <- datamart.exam_bio.result[["Nom"]][sum(rowD-1)]
  }
}


##########################################################
########### Manipulate units in Blood tables ###########
##########################################################

#separate the value in "Valeur_numeriaque" into four variables :
exam_bio.results_units <- datamart.exam_bio.result[!is.na(datamart.exam_bio.result[["Valeur_numerique"]]),]
exam_bio.results_units <- exam_bio.results_units[!is.null(exam_bio.results_units[["Valeur_numerique"]]),]

#1st variable "value_vn": contain the digits
exam_bio.results_units[["value_vn"]] <- exam_bio.results_units[["Valeur_numerique"]]
exam_bio.results_units[["value_vn"]] <- apply(X = exam_bio.results_units, MARGIN = 1,
                                              function(row_df) sub(pattern = "^(-?)(\\d+\\.*\\d*)(.*)",
                                                                   replacement = "\\2",
                                                                   x = row_df[["value_vn"]]))
#2nd variable "negative_vn": contain the sign(negative or positive)
exam_bio.results_units[["negative_vn"]] <- exam_bio.results_units[["Valeur_numerique"]]
exam_bio.results_units[["negative_vn"]] <- apply(X = exam_bio.results_units, MARGIN = 1,
                                                 function(row_df) sub(pattern = "^(-?)(\\d+\\.*\\d*)(.*)",
                                                                      replacement = "\\1",
                                                                      x = row_df[["negative_vn"]]))
#3rd variable "unit_vn": units in the numerator
exam_bio.results_units[["unit_vn"]] <- exam_bio.results_units[["Valeur_numerique"]]
exam_bio.results_units[["unit_vn"]] <- apply(X = exam_bio.results_units, MARGIN = 1,
                                             function(row_df) sub(pattern = "\\d+\\.?\\d*",
                                                                  replacement = "",
                                                                  x = row_df[["unit_vn"]]))
exam_bio.results_units[["unit_vn"]] <- apply(X = exam_bio.results_units, MARGIN = 1,
                                             function(row_df) gsub(pattern = " ",
                                                                   replacement = "",
                                                                   x = row_df[["unit_vn"]]))
#4th variable "unit2_vn": units in the denominator
exam_bio.results_units[["unit2_vn"]] <- exam_bio.results_units[["unit_vn"]]
exam_bio.results_units[["unit2_vn"]] <- apply(X = exam_bio.results_units, MARGIN = 1,
                                              function(row_df) ifelse(test = grepl(pattern = "\\/", row_df[["unit2_vn"]]),
                                                                      yes = sub(pattern = "^(.*\\/)(.*)$",
                                                                                replacement = "\\2",
                                                                                x = row_df[["unit2_vn"]]),
                                                                      no = ""
                                              )
)
exam_bio.results_units[["unit_vn"]] <- apply(X = exam_bio.results_units, MARGIN = 1,
                                             function(row_df) sub(pattern = "^(.*)(\\/.*)$",
                                                                  replacement = "\\1",
                                                                  x = row_df[["unit_vn"]]))
exam_bio.results_units[["value_vn"]] <- as.numeric(exam_bio.results_units[["value_vn"]])
exam_bio.results_units[["unit2_vn"]] <- apply(X = exam_bio.results_units, MARGIN = 1,
                                              function(row_df) sub(pattern = "L",
                                                                   replacement = "l",
                                                                   x = row_df[["unit2_vn"]]))

##########################################################
################ Example of conversion ################
##########################################################


library(measurements)#use of conv_unit()

exam_bio.results_units[["value_vn"]][3]
exam_bio.results_units[["unit_vn"]][3]
exam_bio.results_units[["unit2_vn"]][3]

#change value_vn according to unit_vn
conv_unit(x = exam_bio.results_units[["value_vn"]][3],from = exam_bio.results_units[["unit_vn"]][3], to = "mol")
#change value_vn according to unit2_vn
exam_bio.results_units[["value_vn"]][3] * exam_bio.results_units[["value_vn"]][3]/conv_unit(x = exam_bio.results_units[["value_vn"]][3],from = exam_bio.results_units[["unit2_vn"]][3], to = "m3")




