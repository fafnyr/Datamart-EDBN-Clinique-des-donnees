setwd(dir = "Y:/CIC/EC-CIC/STAGES/StageM1_LCM2019")

load("Y:/CIC/EC-CIC/STAGES/StageM1_LCM2019/debutStage_20190326.RData") #Rdata de départ de mon stage
rm(ehop)
rm(entrepot.lcm)
rm(lcm.stage)
rm(panier)
rm(sum.panier)
#only res.lcm stays

#si warning inutiles :
#assign("last.warning", NULL, envir = baseenv())

##### Functions #####
source("Y:/CIC/EC-CIC/STAGES/StageM1_LCM2019/fonction_edbn_lyma.R")

##### Creation of Better dataframe #####
library(data.table)
res.lcm <- as.data.table(res.lcm)

library(textreadr)
library(dplyr)
library(boilerpipeR)

res.lcm <- res.lcm[,TEXTE_AFFICHAGE := lapply(TEXTE_AFFICHAGE, function(row) html_xmltotext(rowdoc = row))]
res.lcm[["TEXTE_AFFICHAGE"]] <- as.character(res.lcm[["TEXTE_AFFICHAGE"]])

##### Deidentification #####
library(stringi)
library(openssl)
library(stringr)


#transformer date naissance en age
res.lcm[["AGE"]] <- res.lcm[["DATENAIS"]]
res.lcm <- res.lcm[,DATENAIS := lapply(X = DATENAIS, 
                                  function(row) gsub(pattern = ".*(\\d{4})-(\\d{2})-(\\d{2}).*", 
                                                     replacement = "\\3/\\2/\\1", 
                                                     x = row, 
                                                     ignore.case = TRUE))]
res.lcm[["DATENAIS"]] <- as.character(res.lcm[["DATENAIS"]])
res.lcm <- res.lcm[,AGE := lapply(X = AGE, 
                                  function(row) round(as.numeric(Sys.Date() - as.Date(row, format='%d/%m/%Y'))/365.25,0))]
res.lcm[["AGE"]] <- as.integer(res.lcm[["AGE"]])

#conversion dates en format reconnu
res.lcm <- res.lcm[,TEXTE_AFFICHAGE := lapply(TEXTE_AFFICHAGE, 
                                              function(row) better_calendar(totranslate = row))]
res.lcm[["TEXTE_AFFICHAGE"]] <- as.character(res.lcm[["TEXTE_AFFICHAGE"]])

#enlever date de naissance des textes
res.lcm <- res.lcm[,TEXTE_AFFICHAGE := lapply(TEXTE_AFFICHAGE, 
                                              function(row) gsub(
                                                pattern = DATENAIS,
                                                replacement = paste("[",AGE,"]",sep = ""),
                                                x = row,
                                                ignore.case = TRUE))]
res.lcm[["TEXTE_AFFICHAGE"]] <- as.character(res.lcm[["TEXTE_AFFICHAGE"]])

#préparation des offsets
offsetdates <- cbind.data.frame(res.lcm[["IPP"]],res.lcm[["IPP"]])
colnames(offsetdates) <- c("IPP", "WEEK_OFFSET")
offsetdates <- distinct(offsetdates, IPP, .keep_all = TRUE)
offsetdates <- as.data.table(offsetdates)
offsetdates <- offsetdates[,WEEK_OFFSET := lapply(WEEK_OFFSET, function(row) round(x = runif(n = 1, min = 4, max = 14), digits = 0))]
offsetdates[["IPP"]] <- as.character(offsetdates[["IPP"]])
offsetdates[["WEEK_OFFSET"]] <- as.integer(offsetdates[["WEEK_OFFSET"]])
res.lcm <- full_join(x = res.lcm, y = offsetdates, by = "IPP")
res.lcm <- as.data.table(res.lcm)
rm(offsetdates)

#---------------------
#load("Y:/CIC/EC-CIC/STAGES/StageM1_LCM2019/05-14.RData")
#library(data.table)
#library(dplyr)
#library(stringr)
#---------------------

#offset les dates du texte
res.lcm[["TEXTE_AFFICHAGE"]] <- apply(X = res.lcm, MARGIN = 1,
                  function(row_dt) offset_date_text(textpurified = row_dt[["TEXTE_AFFICHAGE"]], 
                                                    offset_date = row_dt[["WEEK_OFFSET"]]))
#décalage de chaque date
res.lcm[["DATE_DECES"]] <- as.Date(res.lcm[["DATE_DECES"]], "%Y/%m/%d")
res.lcm[["DATE_DECES"]] <- res.lcm[["DATE_DECES"]]+as.integer(res.lcm[["WEEK_OFFSET"]])*7
res.lcm[["DATE_MAJ"]] <- as.Date(res.lcm[["DATE_MAJ"]], "%Y/%m/%d")
res.lcm[["DATE_MAJ"]] <- res.lcm[["DATE_MAJ"]]+as.integer(res.lcm[["WEEK_OFFSET"]])*7
res.lcm[["DATESIGNATURE"]] <- as.Date(res.lcm[["DATESIGNATURE"]], "%Y/%m/%d")
res.lcm[["DATESIGNATURE"]] <- res.lcm[["DATESIGNATURE"]]+as.integer(res.lcm[["WEEK_OFFSET"]])*7

#deidentification

returnblind <- blindeye(df_to_anon = res.lcm, algo = "sha512", size_key = 64)
#returnblind is for the CdD

##### Save list #####
saveRDS(returnblind, file = "returnblind.rds")
#returnblind <- readRDS(file = "returnblind.rds")

res.lcm <- returnblind$df
rm(returnblind)
#res.lcm is for the user outside of the CdD

##### Reidentification #####

#df_test2 <- getting_sight_back(df_to_anon = df_test1, algo = "sha512", liste_anon = listeanon, hashkey = saltkey)


##### Creation of Blood tables #####
.rs.restartR()
res.lcm.exam_bio <- res.lcm[grep("Compte rendu examens biologiques", res.lcm[["TEXTE_AFFICHAGE"]], ignore.case = TRUE),]
res.lcm.anti_exam_bio <- res.lcm[grep("Compte rendu examens biologiques", res.lcm[["TEXTE_AFFICHAGE"]], ignore.case = TRUE, invert = TRUE),]
res.lcm.exam_bio[["TEXTE_AFFICHAGE"]] <- as.character(res.lcm.exam_bio[["TEXTE_AFFICHAGE"]])

#ne permet pas d'extraire les titres
res.lcm.exam_bio.result <- do.call("rbind", apply(res.lcm.exam_bio, 1, bio_report_to_table))
rm(res.lcm.exam_bio)
library(dplyr)
library(data.table)
#rename_soit doit être modifié si le nom du tableau est changé

for (rowD in 1:nrow(res.lcm.exam_bio.result)) {
  if(grepl(pattern = "soit:", x = res.lcm.exam_bio.result[["Nom"]][rowD])){
    res.lcm.exam_bio.result[["Nom"]][rowD] <- res.lcm.exam_bio.result[["Nom"]][sum(rowD-1)]
  }
}#correction tableau







