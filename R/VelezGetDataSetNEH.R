# libraries
library(tidyverse)

# arguments
# dictDataDir = path to the directory with TSV files exported from Numbers file.
# dictDataDir <- "/Users/lucascdz/FILES/doutorado/corpus/1608_Velez.1744/Velez.1744.indextotiusartis.1.1.0_tsv"

# # # # # # # # # # # # # #
#
# NÃO EXCLUIR OS IDS !!!  #
#
# # # # # # # # # # # # # #



GetDataSet <- function(dictDataDir){

   ## GET SOURCE DATA TABLES

   specialEntriesPath <- paste0(dictDataDir,"/",dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^specialEntries"))])
   specialEntriesDF <- read.csv(specialEntriesPath, sep = "\t")

   LexicalEntriesPath <- paste0(dictDataDir,"/",dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^LexicalEntries"))])
   LexicalEntriesDF <- read.csv(LexicalEntriesPath, sep = "\t")

   FormsPath <- paste0(dictDataDir,"/",dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^Forms"))])
   FormsDF <- read.csv(FormsPath, sep = "\t")

   LexicalSensesPath <- paste0(dictDataDir,"/",dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^LexicalSenses"))])
   LexicalSensesDF <- read.csv(LexicalSensesPath, sep = "\t")

   UsageExamplesPath <- paste0(dictDataDir,"/",dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^UsageExamples"))])
   UsageExamplesDF <- read.csv(UsageExamplesPath, sep = "\t")

   NotesPath <- paste0(dictDataDir,"/",dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^Notes"))])
   NotesDF <- read.csv(NotesPath, sep = "\t")

   #crossRefPath <- paste0(dictDataDir,"/",dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^crossRef"))])
   #crossRefDF <- read.csv(crossRefPath, sep = "\t")


   #### TABLES TO GET: LexicalEntries, canonicalForms, otherForms, LexicalSenses, UsageExamples

   # LexicalEntries TABLE UPDATE
   LexicalEntriesDF$dcterms.identifier <- gsub("^http://lila-erc.eu/data/id/", "", LexicalEntriesDF$dcterms.identifier) %>%
      gsub(" $", "", .)
   LexicalEntriesDF <- left_join(LexicalEntriesDF, NotesDF)
   LexicalEntriesDF[is.na(LexicalEntriesDF)] <- ""

   # keys DF
   keys <- unique(LexicalEntriesDF$dcterms.identifier)


   # allForms TABLE
   FormsDF$rdfs.comment <- gsub("\\[.*\\]", "", FormsDF$rdfs.comment)
   FormsDF$skos.note <- gsub("\\[.*\\]", "", FormsDF$skos.note)
   FormsDF$ontolex.writtenRep <- gsub("\\[(.*)\\]", "\\1", FormsDF$ontolex.writtenRep)
   FormsDF$lexinfo.note <- gsub("\\[.*\\]", "", FormsDF$lexinfo.note)
   #
   FormsDF$allForms <- unlist(lapply(seq_along(FormsDF$ontolex.Form), function(i) str_flatten_comma(FormsDF[i,c(3:6)])))
   FormsDF$allForms <- gsub(", ,", ",", FormsDF$allForms) %>%
      gsub("^, ", "", .) %>%
      gsub(", $", ",", .) %>%
      gsub(",$", "", .)
   FormsDF$dictID <- gsub(".f\\d$", "", FormsDF$ontolex.Form)
   FormsDF <- FormsDF[,c(8,7)]
   #
   allFormsDF <- data.frame(key=LexicalEntriesDF$dcterms.identifier, dictID=LexicalEntriesDF$ontolex.LexicalEntry, forms=unlist(lapply(seq_along(LexicalEntriesDF$ontolex.LexicalEntry), function(i) str_flatten_comma(FormsDF$allForms[FormsDF$dictID==LexicalEntriesDF$ontolex.LexicalEntry[i]]))), stringsAsFactors = F)
   allFormsDF <- data.frame(key=keys, forms=unlist(lapply(seq_along(keys), function(i) str_flatten_comma(allFormsDF$forms[allFormsDF$key==keys[i]], "; "))), stringsAsFactors = F)
   #
   #allFormsDF <- data.frame(ontolex.LexicalEntry=LexicalEntriesDF$ontolex.LexicalEntry, forms=unlist(lapply(seq_along(LexicalEntriesDF$ontolex.LexicalEntry), function(i) str_flatten_comma(FormsDF$allForms[FormsDF$dictID==LexicalEntriesDF$ontolex.LexicalEntry[i]]))), stringsAsFactors = F)
   #
   #
   #
   # allLexinfo TABLE
   allLexinfoDF <- data.frame(key=LexicalEntriesDF$dcterms.identifier, allLexinfo=unlist(lapply(seq_along(LexicalEntriesDF$dcterms.identifier), function(i) str_flatten(LexicalEntriesDF[i,c(8:9)], " "))), stringsAsFactors = F)
   allLexinfoDF$allLexinfo <- gsub("^ ", "", allLexinfoDF$allLexinfo)
   allLexinfoDF$allLexinfo <- gsub(" $", "", allLexinfoDF$allLexinfo)
   allLexinfoDF <- allLexinfoDF[allLexinfoDF$allLexinfo!="",]
   allLexinfoDF <- allLexinfoDF[order(allLexinfoDF$key),]
   # clear repetitions round #1
   for (i in seq_along(allLexinfoDF$key)){
      if(identical(allLexinfoDF$allLexinfo[i], allLexinfoDF$allLexinfo[i+1]) & identical(allLexinfoDF$key[i], allLexinfoDF$key[i+1])){
         allLexinfoDF$allLexinfo[i+1] <- ""
         }
      }
   allLexinfoDF <- allLexinfoDF[allLexinfoDF$allLexinfo!="",]
   # clear repetitions round #2
   for (i in seq_along(allLexinfoDF$key)){
      if(identical(allLexinfoDF$allLexinfo[i], allLexinfoDF$allLexinfo[i+1]) & identical(allLexinfoDF$key[i], allLexinfoDF$key[i+1])){
         allLexinfoDF$allLexinfo[i+1] <- ""
      }
   }
   allLexinfoDF <- allLexinfoDF[allLexinfoDF$allLexinfo!="",]
   # clear repetitions round #3
   for (i in seq_along(allLexinfoDF$key)){
      if(identical(allLexinfoDF$allLexinfo[i], allLexinfoDF$allLexinfo[i+1]) & identical(allLexinfoDF$key[i], allLexinfoDF$key[i+1])){
         allLexinfoDF$allLexinfo[i+1] <- ""
      }
   }
   allLexinfoDF <- allLexinfoDF[allLexinfoDF$allLexinfo!="",]
   LexinfoDF <- data.frame(key=unique(allLexinfoDF$key), stringsAsFactors = F)
   LexinfoDF$lexinfo <- unlist(lapply(seq_along(LexinfoDF$key), function(i) str_flatten(allLexinfoDF$allLexinfo[allLexinfoDF$key==LexinfoDF$key[i]], " ")))
   #
   # ok
   #
   #
   #
   #
   # allSenses TABLE
   LexicalSensesDF$senseGroup <- gsub(".s\\d\\d", "", LexicalSensesDF$ontolex.LexicalSense)
   LexicalSensesDF$definition <- unlist(lapply(seq_along(LexicalSensesDF$ontolex.LexicalSense), function(i) str_flatten(LexicalSensesDF[i,c(2:4)], " ")))
   LexicalSensesDF$definition <- gsub("  ", " ", LexicalSensesDF$definition) %>%
      gsub("  ", " ", .) %>%
      gsub("  ", " ", .) %>%
      gsub("^ ", "", .)
   LexicalSensesDF <- LexicalSensesDF[,c(6,7,5)]

   senseKeysDF <- LexicalEntriesDF[,c(4,3)]
   colnames(senseKeysDF)[1] <- "key"
   senseKeysDF <- senseKeysDF[order(senseKeysDF$key),]
   # clear repetitions round #1
   for (i in seq_along(senseKeysDF$key)){
      if(identical(senseKeysDF$senseGroup[i], senseKeysDF$senseGroup[i+1]) & identical(senseKeysDF$key[i], senseKeysDF$key[i+1])){
         senseKeysDF$senseGroup[i+1] <- ""
         }
      }
   senseKeysDF <- senseKeysDF[senseKeysDF$senseGroup!="",]
   # clear repetitions round #2
   for (i in seq_along(senseKeysDF$key)){
      if(identical(senseKeysDF$senseGroup[i], senseKeysDF$senseGroup[i+1]) & identical(senseKeysDF$key[i], senseKeysDF$key[i+1])){
         senseKeysDF$senseGroup[i+1] <- ""
      }
   }
   senseKeysDF <- senseKeysDF[senseKeysDF$senseGroup!="",]
   # clear repetitions round #3
   for (i in seq_along(senseKeysDF$key)){
      if(identical(senseKeysDF$senseGroup[i], senseKeysDF$senseGroup[i+1]) & identical(senseKeysDF$key[i], senseKeysDF$key[i+1])){
         senseKeysDF$senseGroup[i+1] <- ""
      }
   }
   senseKeysDF <- senseKeysDF[senseKeysDF$senseGroup!="",]
   # cleaning ok
   # join
   SensesDF <- left_join(senseKeysDF,LexicalSensesDF)
   colnames(SensesDF)[4] <- "note"
   SensesDF$definition[is.na(SensesDF$definition)] <- ""
   SensesDF$note[is.na(SensesDF$note)] <- ""
   SensesDF <- SensesDF[-which(SensesDF$definition=="" & SensesDF$note==""),]
   SensesDF <- SensesDF[,c(1,3,4)]
   #
   # ok
   #
   # Examples TABLE
   UsageExamplesDF$senseGroup <- gsub(".s\\d\\d.u\\d\\d", "", UsageExamplesDF$lexicog.UsageExample)
   UsageExamplesDF$example <- UsageExamplesDF$rdfs.value.la
   UsageExamplesDF$explanation <- unlist(lapply(seq_along(UsageExamplesDF$lexicog.UsageExample), function(i) str_flatten(UsageExamplesDF[i,c(4:6)])))

   ExamplesDF <- left_join(UsageExamplesDF[,c(7:9)],senseKeysDF)
   ExamplesDF <- ExamplesDF[,c(4,2,3)]

   write_tsv(allFormsDF, paste0(dictDataDir, "/NEHprojectForms.tsv"))
   write_tsv(LexinfoDF, paste0(dictDataDir, "/NEHprojectLexinfo.tsv"))
   write_tsv(SensesDF, paste0(dictDataDir, "/NEHprojectSenses.tsv"))
   write_tsv(ExamplesDF, paste0(dictDataDir, "/NEHprojectExamples.tsv"))

   DictDataDFsList <- list(LexicalEntriesDF=LexicalEntriesDF,canonicalFormsDF=canonicalFormsDF,otherFormsDF=otherFormsDF,LexicalSensesDF=LexicalSensesDF,UsageExamplesDF=UsageExamplesDF)

   return(DictDataDFsList)
}
