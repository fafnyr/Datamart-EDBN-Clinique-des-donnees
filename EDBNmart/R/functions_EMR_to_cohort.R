############################################################################################################################
############################################# repository of all functions ##################################################
############################################################################################################################



#' Function html_xmltotext()
#'
#' This function allows you to remove all html and xml tags from the documents, but keep the structure of the document.
#' @param rowdoc document that needs removal of its tags (class : character)
#' @import dplyr boilerpipeR textreadr
#' @keywords html xml datamart
#' @export
#' @return doctxt : document without html or xml tags, ready for extraction if it is a table (class : character).
#' @examples package needed for the example: library(data.table)
#' @examples res.lcm <- res.lcm[,TEXTE_AFFICHAGE := lapply(TEXTE_AFFICHAGE, function(row) html_xmltotext(rowdoc = row))]
#' html_xmltotext()
html_xmltotext <- function(rowdoc){
  #doctxt <- rowdoc[["TEXTE_AFFICHAGE"]]
  doctxt <- rowdoc
  Encoding(doctxt) <- "UTF-8"

  #test if it is an XML document
  if(grepl("xml", doctxt, ignore.case = TRUE)){
    #part to force KeepEverythingExtractor to keep the structure of the document
    doctxt <- gsub("<value/>", "<value>NA</value>", doctxt)
    doctxt <- gsub("<value> </value>", "<value>NA</value>", doctxt)
    doctxt <- gsub(";", ",", doctxt)
    #conversion :
    doctxt <- boilerpipeR::KeepEverythingExtractor(doctxt, asText = TRUE) #better than DefaultExtractor(part of text is missing) and better than xmlTreeParse (source of errors)
    #format after, most important :
    doctxt <- gsub(":\n", ":", doctxt)
    doctxt <- gsub("\n", ";", doctxt)
    #bonus(in case the xml tags are not removed entirely) :
    doctxt <- gsub("<.*?>", "", doctxt)
    doctxt <- gsub("rum ", "", doctxt)
    doctxt <- gsub("ligne ", "", doctxt)
    doctxt <- gsub("titre ", "", doctxt)
    doctxt <- gsub("text ", "", doctxt)
    doctxt <- gsub("value ", "", doctxt)
    doctxt <- gsub("label ", "", doctxt)
    doctxt <- gsub("fo ", "", doctxt)
    doctxt <- gsub("http://www.w3.org/1999/XSL/Format", "", doctxt)
    doctxt <- gsub("xml", "", doctxt)
  } else {
    #test if it is an HTML document
    if(grepl("html", doctxt, ignore.case = TRUE)){
      #conversion
      doctxt <- doctxt %>% textreadr::read_html() %>% paste(collapse=" ")
      doctxt <- gsub("\U00A0", " ", doctxt) #remove non-breaking space
    }
  }
  return(doctxt)
}



#' Function bio_report_to_table()
#'
#' This function allows you to extract all tables containing biological data.
#' @param df_expand document that will be modified in order to be read like a csv document (class : character)
#' @keywords csv table bio datamart
#' @export
#' @return df_results : table containing data about samples collected from a patient (class : data.frame).
#' @examples res.lcm.exam_bio.result <- do.call("rbind", apply(res.lcm.exam_bio, 1, bio_report_to_table))
#' bio_report_to_table()
bio_report_to_table <- function(df_expand){
  df_expand[["TEXTE_AFFICHAGE"]] <- gsub(x = df_expand[["TEXTE_AFFICHAGE"]], pattern = "soit:", replacement = "soit:;", ignore.case = TRUE)
  df_expand[["TEXTE_AFFICHAGE"]] <- gsub(x = df_expand[["TEXTE_AFFICHAGE"]], pattern = ".*Qualifieur;", replacement = "", ignore.case = TRUE)
  df_results <- read.delim(text = df_expand[["TEXTE_AFFICHAGE"]], header = FALSE, na.strings = "NA", sep = ";", fill = TRUE, col.names = c("Nom","Date","Valeur_numerique","Valeur_textuelle","Borne_inferieure","Borne_superieure","Qualifieur"))
  df_results <- data.frame(Nom = as.character(df_results[["Nom"]]),
                           Date = as.character(df_results[["Date"]]),
                           Valeur_numerique = as.character(df_results[["Valeur_numerique"]]),
                           Valeur_textuelle = as.character(df_results[["Valeur_textuelle"]]),
                           Borne_inferieure = as.character(df_results[["Borne_inferieure"]]),
                           Borne_superieure = as.character(df_results[["Borne_superieure"]]),
                           Qualifieur = as.character(df_results[["Qualifieur"]]),
                           IPP = rep(df_expand[["IPP"]],length(df_results[["Nom"]]))
  )
  return(df_results)
}



#' Function better_calendar()
#'
#' This function allows you to transform all dates that are in letters into a standardized format DD/MM/YYYY.
#' @param totranslate document with dates in letters (class : character)
#' @import stringi
#' @keywords date calendar datamart
#' @export
#' @return totranslate : formated text (class : character)
#' @examples package needed for the example: library(data.table)
#' @examples res.lcm <- res.lcm[,TEXTE_AFFICHAGE := lapply(TEXTE_AFFICHAGE, function(row) better_calendar(totranslate = row))]
#' @examples res.lcm[["TEXTE_AFFICHAGE"]] <- as.character(res.lcm[["TEXTE_AFFICHAGE"]])
#' better_calendar()
better_calendar <- function(totranslate){
  totranslate <- stri_trans_general(totranslate, "latin-ascii")
  #remove abreviation from ordinal numbers
  totranslate <- gsub("([[:digit:]])(er)", "\\1", totranslate, ignore.case = TRUE)
  totranslate <- gsub("([[:digit:]])(eme)", "\\1", totranslate, ignore.case = TRUE)
  #transform month(text) into month(numeric)
  totranslate <- gsub("[[:space:]]*janvier[[:space:]]+", "/01/", totranslate, ignore.case = TRUE)
  totranslate <- gsub("[[:space:]]*fevrier[[:space:]]+", "/02/", totranslate, ignore.case = TRUE)
  totranslate <- gsub("[[:space:]]*mars[[:space:]]+", "/03/", totranslate, ignore.case = TRUE)
  totranslate <- gsub("[[:space:]]*avril[[:space:]]+", "/04/", totranslate, ignore.case = TRUE)
  totranslate <- gsub("[[:space:]]*mai[[:space:]]+", "/05/", totranslate, ignore.case = TRUE)
  totranslate <- gsub("[[:space:]]*juin[[:space:]]+", "/06/", totranslate, ignore.case = TRUE)
  totranslate <- gsub("[[:space:]]*juillet[[:space:]]+", "/07/", totranslate, ignore.case = TRUE)
  totranslate <- gsub("[[:space:]]*ao?t[[:space:]]+", "/08/", totranslate, ignore.case = TRUE)
  totranslate <- gsub("[[:space:]]*septembre[[:space:]]+", "/09/", totranslate, ignore.case = TRUE)
  totranslate <- gsub("[[:space:]]*octobre[[:space:]]+", "/10/", totranslate, ignore.case = TRUE)
  totranslate <- gsub("[[:space:]]*novembre[[:space:]]+", "/11/", totranslate, ignore.case = TRUE)
  totranslate <- gsub("[[:space:]]*decembre[[:space:]]+", "/12/", totranslate, ignore.case = TRUE)
  # in case the day numbers are single digit
  totranslate <- gsub(" (\\d{1}/\\d{2}/\\d{2})", " 0\\1", totranslate, ignore.case = TRUE)
  return(totranslate)
}



#' Function offset_date_text()
#'
#' This function allows you to change(offset) all dates in standardized format DD/MM/YYYY, by a number of weeks.
#' @param textpurified document which dates will be modified, purified from dates in letters (class : character).
#' @param offset_date number of weeks of the offset (class : integer).
#' @import stringr
#' @keywords date offset datamart
#' @export
#' @return textpurified : text offsetted (class : character)
#' @examples res.lcm[["TEXTE_AFFICHAGE"]] <- apply(X = res.lcm, MARGIN = 1, function(row_dt) offset_date_text(textpurified = row_dt[["TEXTE_AFFICHAGE"]], offset_date = row_dt[["WEEK_OFFSET"]]))
#' offset_date_text()
offset_date_text <- function(textpurified, offset_date){
  #creation of a table containing all dates
  datestext <- str_extract_all( string = textpurified, pattern = "\\d{2}/\\d{2}/\\d{4}", simplify = TRUE)
  datestext <- as.vector(datestext)
  datestext <- cbind.data.frame(datestext,datestext)
  colnames(datestext) <- c("pattern", "offset")
  datestext[["pattern"]] <- as.character(datestext[["pattern"]])
  datestext[["offset"]] <- as.character(datestext[["offset"]])

  # verify that DD and MM really seems like day number and month number
  datestext[["offset"]] <- lapply(X = datestext[["offset"]],
                                  function(row) ifelse(test = grepl(pattern = "^.*/([2-9][0-9]|1[3-9])/.*$", row),
                                                       yes = sub(pattern = "(\\d{2})/(\\d{2})/(\\d{4})",
                                                                 replacement = "\\2/\\1/\\3",
                                                                 x = row),
                                                       no = row
                                  )
  )
  datestext[["offset"]] <- as.character(datestext[["offset"]])

  #offset in the table
  datestext[["offset"]] <- as.Date(datestext[["offset"]], "%d/%m/%Y")
  datestext[["offset"]] <- datestext[["offset"]] + as.integer(offset_date)*7
  datestext[["pattern"]] <- as.character(datestext[["pattern"]])
  datestext[["offset"]] <- as.character(datestext[["offset"]])

  # modification of the text with the offsetted dates
  for (rowD in 1:nrow(datestext)) {
    if(!identical(datestext[["pattern"]][rowD], character(0))){
      if(!is.null(datestext[["pattern"]][rowD])){
        if(!is.na(datestext[["pattern"]][rowD])){
          textpurified <- gsub(pattern = datestext[["pattern"]][rowD],
                               replacement = datestext[["offset"]][rowD],
                               x = textpurified)
        }
      }
    }
  }

  rm(datestext)
  return(textpurified)
}



#' Function blindeye()
#'
#' This function allows you to de-identify the patients in the text from Name, surname, date of birth (replaced by age), Id_code in the hospital (IPP). It also keep everything in order to re-identify the patients if needed.
#' @param df_to_anon datamart that will be de-identified (class : data.frame). Columns needed : TEXTE_AFFICHAGE (doc in character class), NOM (name), PRENOM (surname), DATENAIS(birthday date).
#' @param algo name of the hashing algorithm chosen from the "openssl" package, "sha512" or "sha384" are advised because they are more secure (sha-2 family). (Class : character)
#' @param size_key randomly generated hash key, a size of at least 64 is advised (class : integer)
#' @import stringi dplyr openssl
#' @keywords de-identification hash datamart
#' @export
#' @return list_return :
#' @return list_return$df : the de-identified datamart (class : data.frame)
#' @return list_return$liste : liste of all the patients and their identifying data after de-identification of the datamart(class : data.frame)(need to be kept secret).
#' @return list_return$hashkey : the key used to hash the specific hospital code of each patient (IPP)(class : character)(need to be kept secret).
#' @examples returnblind <- blindeye(df_to_anon = res.lcm, algo = "sha512", size_key = 64)
#' blindeye()
blindeye <- function( df_to_anon, algo , size_key){
  #remove all diacritics
  df_to_anon[["TEXTE_AFFICHAGE"]] <- stri_trans_general(df_to_anon[["TEXTE_AFFICHAGE"]], "latin-ascii")
  df_to_anon[["NOM"]] <- stri_trans_general(df_to_anon[["NOM"]], "latin-ascii")
  df_to_anon[["PRENOM"]] <- stri_trans_general(df_to_anon[["PRENOM"]], "latin-ascii")

  #anonymise the patients
  df_to_anon[["TEXTE_AFFICHAGE"]] <- apply(df_to_anon, 1, function(row) gsub( pattern = row[["NOM"]], replacement = " NOM_P ", x = row[["TEXTE_AFFICHAGE"]], ignore.case = TRUE))
  df_to_anon[["TEXTE_AFFICHAGE"]] <- apply(df_to_anon, 1, function(row) gsub( pattern = row[["PRENOM"]], replacement = " PRENOM_P ", x = row[["TEXTE_AFFICHAGE"]], ignore.case = TRUE))

  #creation of hash key
  hashkey <- stri_rand_strings(1, size_key, "[a-zA-Z0-9]")

  #hashing
  if (algo == "sha384") df_to_anon[["ANON"]] <- sha384(df_to_anon[["IPP"]], key = hashkey)
  if (algo == "sha512") df_to_anon[["ANON"]] <- sha512(df_to_anon[["IPP"]], key = hashkey)

  #creation of list destinated to the re-identifyer
  liste_anon <- cbind(df_to_anon[["NOM"]],
                      df_to_anon[["PRENOM"]],
                      df_to_anon[["IPP"]],
                      df_to_anon[["DATENAIS"]],
                      df_to_anon[["WEEK_OFFSET"]])

  colnames(liste_anon) <- c("NOM","PRENOM","IPP", "DATENAIS", "WEEK_OFFSET")
  liste_anon <- as.data.frame(liste_anon)
  liste_anon <- distinct(liste_anon, IPP, .keep_all = TRUE)
  liste_anon[["NOM"]] <- as.character(liste_anon[["NOM"]])
  liste_anon[["PRENOM"]] <- as.character(liste_anon[["PRENOM"]])
  liste_anon[["IPP"]] <- as.character(liste_anon[["IPP"]])
  liste_anon[["DATENAIS"]] <- as.character(liste_anon[["DATENAIS"]])
  liste_anon[["WEEK_OFFSET"]] <- as.integer(liste_anon[["WEEK_OFFSET"]])

  #modification of dataframe
  df_to_anon[["TEXTE_AFFICHAGE"]] <- apply(df_to_anon, 1, function(row) gsub( pattern = row[["IPP"]], replacement = df_to_anon[["ANON"]], x = row[["TEXTE_AFFICHAGE"]], ignore.case = TRUE))
  df_to_anon[["IPP"]] <- df_to_anon[["ANON"]]#replace the IPP by ANON

  df_to_anon <- subset(df_to_anon, select = -c(NOM, PRENOM, ANON, DATENAIS, WEEK_OFFSET))
  list_return <- list("df" = df_to_anon, "liste" = liste_anon, "hashkey" = hashkey)
  return(list_return)
}



#' Function getting_sight_back()
#'
#' This function allows you to re-identify the datamart (in pair with the blindeye() function)
#' @param WORKINPROGRESS WORKINPROGRESS
#' @import dplyr openssl
#' @keywords re-identification datamart
#' @export
#' @return WORKINPROGRESS
#' @examples WORKINPROGRESS
#' getting_sight_back()
getting_sight_back <- function ( df_to_anon, algo, liste_anon , hashkey){
  #hashage
  if (algo == "sha384") liste_anon[["ANON"]] <- sha384(liste_anon[["IPP"]], key = hashkey)
  if (algo == "sha512") liste_anon[["ANON"]] <- sha512(liste_anon[["IPP"]], key = hashkey)
  #return of name and surname
  liste_anon[["ANON"]] <- as.character(liste_anon[["ANON"]])
  df_to_anon[["ANON"]] <- as.character(df_to_anon[["ANON"]])
  df_to_anon <- full_join(liste_anon, df_to_anon, by = "ANON")
  #make the patients retrun in doc
  df_to_anon[["TEXTE_AFFICHAGE"]] <- apply(df_to_anon, 1, function(row) gsub( pattern = " NOM_P ", replacement = row[["NOM"]], x = row[["TEXTE_AFFICHAGE"]], ignore.case = TRUE))
  df_to_anon[["TEXTE_AFFICHAGE"]] <- apply(df_to_anon, 1, function(row) gsub( pattern = " PRENOM_P ", replacement = row[["PRENOM"]], x = row[["TEXTE_AFFICHAGE"]], ignore.case = TRUE))

  return(df_to_anon)
}





#column with names "SEXE", "AGE", "NOM" for both dataframes
cross_cohort_EDBN <- function(COHORTE, EDBN, nomfichier){
  library(dplyr)

  #### EDBN sans les duplicats d? aux donn?es non-structur?es :
  EDBN.unduplicate <- distinct(EDBN, IPP, .keep_all = TRUE)
  #N
  EDBN_N <- length(EDBN.unduplicate[["NOM"]])
  EDBN_M <- length(grep("M", EDBN.unduplicate[["SEXE"]], ignore.case = TRUE))
  EDBN_F <- length(grep("F", EDBN.unduplicate[["SEXE"]], ignore.case = TRUE))
  #pourcentage sex
  EDBN_p_M <- length(grep("M", EDBN.unduplicate[["SEXE"]], ignore.case = TRUE))/(length(grep("M", EDBN.unduplicate[["SEXE"]], ignore.case = TRUE))+length(grep("F", EDBN.unduplicate[["SEXE"]], ignore.case = TRUE)))*100
  EDBN_p_F <- length(grep("F", EDBN.unduplicate[["SEXE"]], ignore.case = TRUE))/(length(grep("M", EDBN.unduplicate[["SEXE"]], ignore.case = TRUE))+length(grep("F", EDBN.unduplicate[["SEXE"]], ignore.case = TRUE)))*100
  #age
  EDBN_mean <- mean(x = EDBN.unduplicate[["AGE"]], na.rm = TRUE)
  EDBN_sd <- sd(x = EDBN.unduplicate[["AGE"]], na.rm = TRUE)
  EDBN_median <- median(x = EDBN.unduplicate[["AGE"]], na.rm = TRUE)

  #### COHORTE :
  #N
  COHORTE_N <- length(COHORTE[["NOM"]])
  COHORTE_M <- length(grep("M", COHORTE[["SEXE"]], ignore.case = TRUE))
  COHORTE_F <- length(grep("F", COHORTE[["SEXE"]], ignore.case = TRUE))
  #pourcentage sex
  COHORTE_p_M <- length(grep("M", COHORTE[["SEXE"]], ignore.case = TRUE))/(length(grep("M", COHORTE[["SEXE"]], ignore.case = TRUE))+length(grep("F", COHORTE[["SEXE"]], ignore.case = TRUE)))*100
  COHORTE_p_F <- length(grep("F", COHORTE[["SEXE"]], ignore.case = TRUE))/(length(grep("M", COHORTE[["SEXE"]], ignore.case = TRUE))+length(grep("F", COHORTE[["SEXE"]], ignore.case = TRUE)))*100
  #age
  COHORTE_mean <- mean(x = COHORTE[["AGE"]], na.rm = TRUE)
  COHORTE_sd <- sd(x = COHORTE[["AGE"]], na.rm = TRUE)
  COHORTE_median <- median(x = COHORTE[["AGE"]], na.rm = TRUE)

  #### COHORTE-EDBN commun :
  C_E.semi <- semi_join(COHORTE, EDBN.unduplicate, by = "IPP")
  #N
  C_E.semi_N <- length(C_E.semi[["NOM"]])
  C_E.semi_M <- length(grep("M", C_E.semi[["SEXE"]], ignore.case = TRUE))
  C_E.semi_F <- length(grep("F", C_E.semi[["SEXE"]], ignore.case = TRUE))
  #pourcentage sex
  C_E.semi_p_M <- length(grep("M", C_E.semi[["SEXE"]], ignore.case = TRUE))/(length(grep("M", C_E.semi[["SEXE"]], ignore.case = TRUE))+length(grep("F", C_E.semi[["SEXE"]], ignore.case = TRUE)))*100
  C_E.semi_p_F <- length(grep("F", C_E.semi[["SEXE"]], ignore.case = TRUE))/(length(grep("M", C_E.semi[["SEXE"]], ignore.case = TRUE))+length(grep("F", C_E.semi[["SEXE"]], ignore.case = TRUE)))*100
  #age
  C_E.semi_mean<- mean(x = C_E.semi[["AGE"]], na.rm = TRUE)
  C_E.semi_sd <- sd(x = C_E.semi[["AGE"]], na.rm = TRUE)
  C_E.semi_median <- median(x = C_E.semi[["AGE"]], na.rm = TRUE)

  #### COHORTE only :
  COHORTE_anti <- anti_join(COHORTE, EDBN.unduplicate, by = "IPP")
  #N
  COHORTE_anti_N <- length(COHORTE_anti[["NOM"]])
  COHORTE_anti_M <- length(grep("M", COHORTE_anti[["SEXE"]], ignore.case = TRUE))
  COHORTE_anti_F <- length(grep("F", COHORTE_anti[["SEXE"]], ignore.case = TRUE))
  #pourcentage sex
  COHORTE_anti_p_M <- length(grep("M", COHORTE_anti[["SEXE"]], ignore.case = TRUE))/(length(grep("M", COHORTE_anti[["SEXE"]], ignore.case = TRUE))+length(grep("F", COHORTE_anti[["SEXE"]], ignore.case = TRUE)))*100
  COHORTE_anti_p_F <- length(grep("F", COHORTE_anti[["SEXE"]], ignore.case = TRUE))/(length(grep("M", COHORTE_anti[["SEXE"]], ignore.case = TRUE))+length(grep("F", COHORTE_anti[["SEXE"]], ignore.case = TRUE)))*100
  #age
  COHORTE_anti_mean<- mean(x = COHORTE_anti[["AGE"]], na.rm = TRUE)
  COHORTE_anti_sd <- sd(x = COHORTE_anti[["AGE"]], na.rm = TRUE)
  COHORTE_anti_median <- median(x = COHORTE_anti[["AGE"]], na.rm = TRUE)

  #### EDBN only :
  EDBN.unduplicate_anti <- anti_join(EDBN.unduplicate, COHORTE, by = "IPP")
  #N
  EDBN_anti_N <- length(EDBN.unduplicate_anti[["NOM"]])
  EDBN_anti_M <- length(grep("M", EDBN.unduplicate_anti[["SEXE"]], ignore.case = TRUE))
  EDBN_anti_F <- length(grep("F", EDBN.unduplicate_anti[["SEXE"]], ignore.case = TRUE))
  #pourcentage sex
  EDBN_anti_p_M <- length(grep("M", EDBN.unduplicate_anti[["SEXE"]], ignore.case = TRUE))/(length(grep("M", EDBN.unduplicate_anti[["SEXE"]], ignore.case = TRUE))+length(grep("F", EDBN.unduplicate_anti[["SEXE"]], ignore.case = TRUE)))*100
  EDBN_anti_p_F <- length(grep("F", EDBN.unduplicate_anti[["SEXE"]], ignore.case = TRUE))/(length(grep("M", EDBN.unduplicate_anti[["SEXE"]], ignore.case = TRUE))+length(grep("F", EDBN.unduplicate_anti[["SEXE"]], ignore.case = TRUE)))*100
  #age
  EDBN_anti_mean<- mean(x = EDBN.unduplicate_anti[["AGE"]], na.rm = TRUE)
  EDBN_anti_sd <- sd(x = EDBN.unduplicate_anti[["AGE"]], na.rm = TRUE)
  EDBN_anti_median <- median(x = EDBN.unduplicate_anti[["AGE"]], na.rm = TRUE)


  #### Creation of a matrix to compare EDBN and cohort :
  df_resum <- rbind(c(EDBN_N, EDBN_M, EDBN_F, EDBN_p_M, EDBN_p_F, EDBN_mean, EDBN_sd, EDBN_median),
                    c(COHORTE_N, COHORTE_M, COHORTE_F, COHORTE_p_M, COHORTE_p_F, COHORTE_mean, COHORTE_sd, COHORTE_median),
                    c(C_E.semi_N, C_E.semi_M, C_E.semi_F, C_E.semi_p_M, C_E.semi_p_F, C_E.semi_mean, C_E.semi_sd, C_E.semi_median),
                    c(COHORTE_anti_N, COHORTE_anti_M, COHORTE_anti_F, COHORTE_anti_p_M, COHORTE_anti_p_F, COHORTE_anti_mean, COHORTE_anti_sd, COHORTE_anti_median),
                    c(EDBN_anti_N, EDBN_anti_M, EDBN_anti_F, EDBN_anti_p_M, EDBN_anti_p_F, EDBN_anti_mean, EDBN_anti_sd, EDBN_anti_median))
  colnames(df_resum) <- c("total (n)", "Male (n)", "Female (n)", "male (%)", "female (%)", "mean (age)", "sd (age)", "median (age)")
  rownames(df_resum) <- c("EDBN", "Whole COHORT", "Common EDBN-COHORT", "COHORT only", "EDBN only")

  write.table(df_resum, file = paste(nomfichier, ".txt", sep = "", collapse = NULL), row.names = T, col.names = T, quote = TRUE, sep = ";")

  #### Creation of a Venn Diagram :
  library(VennDiagram)
  plot.new()
  draw.pairwise.venn(
    area1 = EDBN_N,
    area2 = COHORTE_N,
    cross.area = C_E.semi_N,
    category = c("EDBN", "Cohort"),
    ext.text = TRUE,
    scaled = TRUE,
    cat.pos = c(-20,30),
    fill = c("red","blue")
  )
  return(df_resum)
}




























