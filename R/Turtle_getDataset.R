# dictDataDir = path to the directory with TSV files exported from Numbers file (NOTE that it ends with /).
#dictDataDir <- "/Users/lucascdz/FILES/doutorado/corpus/1762_Fonseca/Fonseca.1798.parvumlexicum-0.4.1_tsv/"
#dictPrefix <- "Fonseca1798\\."
#target <- "LiLa"

TTLGetDataSet <- function(dictDataDir,dictPrefix,target){
   print(date())

   ############################
   ## GET SOURCE DATA TABLES ##
   ############################

   LexicalEntriesPath <- paste0(dictDataDir,dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^LexicalEntries"))])
   LexicalEntriesDF <- read.csv(LexicalEntriesPath, sep = "\t")

   FormsPath <- paste0(dictDataDir,dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^Forms"))])
   FormsDF <- read.csv(FormsPath, sep = "\t")

   LexicalSensesPath <- paste0(dictDataDir,dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^LexicalSenses"))])
   LexicalSensesDF <- read.csv(LexicalSensesPath, sep = "\t")

   UsageExamplesPath <- paste0(dictDataDir,dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^UsageExamples"))])
   UsageExamplesDF <- read.csv(UsageExamplesPath, sep = "\t")

   decompPath <- paste0(dictDataDir,dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^decomp"))])
   decompDF <- read.csv(decompPath, sep = "\t")

   if(str_detect(dictPrefix, "Velez")){
      crossRefPath <- paste0(dictDataDir,dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^crossRef"))])
      crossRefDF <- read.csv(crossRefPath, sep = "\t")
   }

   ########################
   # FILTER OUT DUPLICATES 'LexicalEntries' (SAME senseGroup AND lila_id[once not empty])
   ########################
   if(identical(target,"LiLa")){
      Duplicates <- unlist(lapply(seq_along(LexicalEntriesDF$ontolex.LexicalEntry), function(i) LexicalEntriesDF$ontolex.LexicalEntry[identical(LexicalEntriesDF$ontolex.sense[i], LexicalEntriesDF$ontolex.sense[i-1]) & identical(LexicalEntriesDF$ontolex.canonicalForm[i], LexicalEntriesDF$ontolex.canonicalForm[i-1]) & LexicalEntriesDF$ontolex.canonicalForm[i]!=''][i]))
      Duplicates <- Duplicates[!is.na(Duplicates)]
      LexicalEntriesDF <- LexicalEntriesDF[!LexicalEntriesDF$ontolex.LexicalEntry %in% Duplicates,]
   }


   ###################################
   # CREATE 'lexicog:Entries' DATA FRAME
   ###################################
   EntriesDF <- data.frame(lexicog.Entry=unique(gsub("\\.e\\d\\d[a-z]?$", "", LexicalEntriesDF$ontolex.LexicalEntry)), stringsAsFactors = F)
   EntriesDF$rdfs.label <- unlist(lapply(seq_along(EntriesDF$lexicog.Entry), function(i) paste0(gsub("([A-z]*).*","\\1", dictPrefix)," entry for '", FormsDF$ontolex.writtenRep[FormsDF$ontolex.Form == paste0(EntriesDF$lexicog.Entry[i],".e01.f01")],", ",FormsDF$ontolex.writtenRep[FormsDF$ontolex.Form == paste0(EntriesDF$lexicog.Entry[i],".e01.f02")],", ",FormsDF$ontolex.writtenRep[FormsDF$ontolex.Form == paste0(EntriesDF$lexicog.Entry[i],".e01.f03")],"'"))) %>%
      gsub(", '$", "'", .) %>%
      gsub(", '$", "'", .)
   EntriesDF$lexicog.describes <- LexicalEntriesDF$ontolex.LexicalEntry[str_detect(LexicalEntriesDF$ontolex.LexicalEntry, 'e01') & !str_detect(LexicalEntriesDF$ontolex.LexicalEntry, 'e\\d\\d[a-z]')]
   subComponentVec <- LexicalEntriesDF$ontolex.LexicalEntry[!str_detect(LexicalEntriesDF$ontolex.LexicalEntry, 'e01') & !str_detect(LexicalEntriesDF$ontolex.LexicalEntry, 'e\\d\\d[a-z]')]
   EntriesDF$lexicog.subComponent <- unlist(lapply(seq_along(EntriesDF$lexicog.Entry), function(i) str_flatten_comma(subComponentVec[str_detect(subComponentVec,EntriesDF$lexicog.Entry[i])])))
   # join skos.notes to Entries
   if(str_detect(dictPrefix, "Velez")){
      EntriesDF <- left_join(EntriesDF,crossRefDF)
   }
   if(str_detect(dictPrefix, "Fonseca")){
      EntriesDF$skos.note <- ''
      for (i in seq_along(EntriesDF$skos.note)){
         EntriesDF$skos.note[i] <- LexicalEntriesDF$POS.note[LexicalEntriesDF$ontolex.LexicalEntry == EntriesDF$lexicog.describes[i]]
      }
   }
   # adjust prefixes
   EntriesDF$lexicog.Entry <- gsub(dictPrefix, "Entry\\:", EntriesDF$lexicog.Entry)
   EntriesDF$lexicog.describes <- gsub(dictPrefix, "LexEntry\\:", EntriesDF$lexicog.describes)
   EntriesDF$lexicog.subComponent <- gsub(dictPrefix, "LxgComp\\:", EntriesDF$lexicog.subComponent)
   EntriesDF[is.na(EntriesDF)] <- ""


   ###################################
   # CREATE 'lexicog:Components' DATA FRAME
   ###################################
   LexComponentsDF <- data.frame(lexicog.LexicographicComponent = subComponentVec)
   LexComponentsDF$rdfs.label <- unlist(lapply(seq_along(LexComponentsDF$lexicog.LexicographicComponent), function(i) paste0("Subentry '", FormsDF$ontolex.writtenRep[FormsDF$ontolex.Form == paste0(LexComponentsDF$lexicog.LexicographicComponent[i],".f01")],", ",FormsDF$ontolex.writtenRep[FormsDF$ontolex.Form == paste0(LexComponentsDF$lexicog.LexicographicComponent[i],".f02")],", ",FormsDF$ontolex.writtenRep[FormsDF$ontolex.Form == paste0(LexComponentsDF$lexicog.LexicographicComponent[i],".f03")],"' under '", FormsDF$ontolex.writtenRep[FormsDF$ontolex.Form == paste0(gsub("e\\d\\d", "e01", LexComponentsDF$lexicog.LexicographicComponent[i]),".f01")], "'"))) %>%
      gsub(", ' under", "' under", .) %>%
      gsub(", ' under", "' under", .) %>%
      gsub("' under ''", "'", .) %>%
      gsub("\\], \\[", ", ", .)
   LexComponentsDF$lexicog.describes <- LexComponentsDF$lexicog.LexicographicComponent

   # Get 'skos.note' content from the LexicalEntry 'PRE.note' column
   LexComponentsDF$skos.note <- ''
   for (i in seq_along(LexComponentsDF$skos.note)){
      LexComponentsDF$skos.note[i] <- LexicalEntriesDF$PRE.note[LexicalEntriesDF$ontolex.LexicalEntry == LexComponentsDF$lexicog.describes[i]]
   }
   # Update prefixes
   LexComponentsDF$lexicog.LexicographicComponent <- gsub(dictPrefix, "LxgComp:", LexComponentsDF$lexicog.LexicographicComponent)
   LexComponentsDF$lexicog.describes <- gsub(dictPrefix, "LexEntry\\:", LexComponentsDF$lexicog.describes)



   ####################################
   # UPDATE 'ontolex:LexicalEntries' DATA FRAME
   ###################################

   # add links to lexical forms
   if('POS.note' %in% colnames(LexicalEntriesDF)){
      colnames(LexicalEntriesDF)[which(str_detect(colnames(LexicalEntriesDF),"POS.note"))] <- "ontolex.lexicalForm"
   }
   LexicalEntriesDF$ontolex.lexicalForm <- ''
   if(identical(target,"LiLa")){
      lexicalForm_idVec <- LexicalEntriesDF$ontolex.LexicalEntry[LexicalEntriesDF$ontolex.canonicalForm==""]
      for(i in seq_along(lexicalForm_idVec)){
         LexicalEntriesDF$ontolex.lexicalForm[LexicalEntriesDF$ontolex.LexicalEntry==lexicalForm_idVec[i]] <- paste0(lexicalForm_idVec[i],'.f01')
      }
   } else {
      lexicalForm_idVec <- FormsDF$ontolex.Form[str_detect(FormsDF$ontolex.Form, "f01")]
      LexicalEntriesDF$ontolex.lexicalForm <- unlist(lapply(seq_along(LexicalEntriesDF$ontolex.LexicalEntry), function(i) lexicalForm_idVec[str_detect(lexicalForm_idVec, paste0(LexicalEntriesDF$ontolex.LexicalEntry[i],"\\."))]))
      otherForm_idVec <- FormsDF$ontolex.Form[str_detect(FormsDF$ontolex.Form, "f01", negate = T)]
      LexicalEntriesDF$ontolex.otherForm <- unlist(lapply(seq_along(LexicalEntriesDF$ontolex.LexicalEntry), function(i) str_flatten_comma(otherForm_idVec[str_detect(otherForm_idVec, paste0(LexicalEntriesDF$ontolex.LexicalEntry[i],"\\."))])))
   }

   # add links to lexical senses
   sense_idVec <- unique(c(LexicalSensesDF$ontolex.LexicalSense, gsub("\\.u\\d\\d.?$", "", UsageExamplesDF$lexicog.UsageExample)))
   for (i in seq_along(LexicalEntriesDF$ontolex.sense)){
      LexicalEntriesDF$ontolex.sense[i] <- str_flatten_comma(sense_idVec[str_detect(sense_idVec, paste0(LexicalEntriesDF$ontolex.sense[i],"\\."))])
   }

   # add 'decomp.subterm' property to mwes
   if('PRE.note' %in% colnames(LexicalEntriesDF)){
      colnames(LexicalEntriesDF)[which(str_detect(colnames(LexicalEntriesDF),"PRE.note"))] <- "decomp.subterm"
   }
   LexicalEntriesDF$decomp.subterm <- ""
   for (i in seq_along(LexicalEntriesDF$decomp.subterm)){
      LexicalEntriesDF$decomp.subterm[i] <- str_flatten_comma(decompDF$decomp.subterm[decompDF$ontolex.LexicalEntry==LexicalEntriesDF$ontolex.LexicalEntry[i]])
   }

   # add 'rdfs.labels' to all instances
   for(i in seq_along(LexicalEntriesDF$ontolex.LexicalEntry)){
      LexicalEntriesDF$rdfs.label[i] <- paste0(gsub("([A-z]*).*","\\1", dictPrefix)," lexical entry for '", FormsDF$ontolex.writtenRep[FormsDF$ontolex.Form == paste0(LexicalEntriesDF$ontolex.LexicalEntry[i],".f01")], "'")
   }
   # remove square brackets '[' ']' from labels
   LexicalEntriesDF$rdfs.label <- gsub("\\[", "", LexicalEntriesDF$rdfs.label) %>%
         gsub("\\]", "", .)

   # add special labels for lexical entries created exclusively as MWE components
   for(i in which(str_detect(LexicalEntriesDF$ontolex.LexicalEntry, "[a-z]$"))){
      LexicalEntriesDF$rdfs.label[i] <- gsub("[A-z]+ lexical entry for '(.*)'", "Added lexical entry for [\\1], component of a multiword expression", LexicalEntriesDF$rdfs.label[i])
   }

   # updates all prefixes
   LexicalEntriesDF$ontolex.LexicalEntry <- gsub(dictPrefix, "LexEntry\\:", LexicalEntriesDF$ontolex.LexicalEntry)
   LexicalEntriesDF$ontolex.canonicalForm <- gsub(dictPrefix, "Form\\:", LexicalEntriesDF$ontolex.canonicalForm)
   LexicalEntriesDF$ontolex.lexicalForm <- gsub(dictPrefix, "Form\\:", LexicalEntriesDF$ontolex.lexicalForm)
   LexicalEntriesDF$decomp.subterm <- gsub(dictPrefix, "LexEntry\\:", LexicalEntriesDF$decomp.subterm)
   LexicalEntriesDF$ontolex.sense <- gsub(dictPrefix, "LexSense\\:", LexicalEntriesDF$ontolex.sense)
   if(identical(identical(target,"LiLa"), FALSE)){
      LexicalEntriesDF$ontolex.otherForm <- gsub(dictPrefix, "Form\\:", LexicalEntriesDF$ontolex.otherForm)
   }

   #####################################
   # UPGRADE 'ontolex:Forms' DATA FRAME
   #####################################
   FormsDF$ontolex.Form <- gsub(dictPrefix, "Form\\:", FormsDF$ontolex.Form)
   if(identical(target,"LiLa")){
      FormsDF <- FormsDF[FormsDF$ontolex.Form %in% LexicalEntriesDF$ontolex.lexicalForm,]
   }

   # cut off square brackets from 'writtenRep'
   FormsDF$ontolex.writtenRep <- gsub("\\[(.*)\\]", "\\1", FormsDF$ontolex.writtenRep)

   # get 'labels' for all Forms
   colnames(FormsDF)[which(str_detect(colnames(FormsDF),"baseForm"))] <- "rdfs.label"
   FormsDF$rdfs.label[FormsDF$rdfs.label!=""] <- paste0("-",FormsDF$ontolex.writtenRep[FormsDF$rdfs.label!=""])
   FormsDF$rdfs.label[FormsDF$rdfs.label==""] <- FormsDF$ontolex.writtenRep[FormsDF$rdfs.label==""]

   # normalize labels
   FormsDF$rdfs.label <- gsub("(.*)", "\\L\\1", FormsDF$rdfs.label, perl = T) %>%
      gsub("j", "i", .) %>%
      gsub("v", "u", .)


   #####################################
   # UPGRADE 'ontolex:LexicalSenses' DATA FRAME
   #####################################
   LexicalSensesAllDF <- data.frame(ontolex.LexicalSense=sense_idVec[order(sense_idVec)], stringsAsFactors = F)
   LexicalSensesAllDF <- left_join(LexicalSensesAllDF,LexicalSensesDF)

   # get rid of ending punctuation
   LexicalSensesAllDF$skos.note <- gsub("[\\,|\\:|\\;]$", "", LexicalSensesAllDF$skos.note)
   if('skos.definition.la' %in% colnames(LexicalSensesAllDF)){
      LexicalSensesAllDF$skos.definition.la <- gsub("[\\,|\\:|\\;|\\.]$", "", LexicalSensesAllDF$skos.definition.la)
   }
   LexicalSensesAllDF$skos.definition.pt <- gsub("[\\,|\\:|\\;|\\.]$", "", LexicalSensesAllDF$skos.definition.pt)
   LexicalSensesAllDF$lexinfo.note <- gsub("[\\,|\\:|\\;]$", "", LexicalSensesAllDF$lexinfo.note) %>%
      gsub("[\\,|\\:|\\;] \\|", "", .)

   # add labels
   if('skos.definition.la' %in% colnames(LexicalSensesAllDF)){
      LexicalSensesAllDF$rdfs.label.la <- unlist(lapply(seq_along(LexicalSensesAllDF$ontolex.LexicalSense), function(i) LexicalSensesAllDF$skos.definition.la[i]))
   }
   LexicalSensesAllDF$rdfs.label.pt <- unlist(lapply(seq_along(LexicalSensesAllDF$ontolex.LexicalSense), function(i) LexicalSensesAllDF$skos.definition.pt[i]))
   LexicalSensesAllDF$rdfs.label.en[str_detect(LexicalSensesAllDF$ontolex.LexicalSense, "s99")] <- "Further meanings (see related usage examples)"
   LexicalSensesAllDF$rdfs.comment[str_detect(LexicalSensesAllDF$ontolex.LexicalSense, "s99")] <- "This definitionless sense is an instance of the ontolex:LexicalSense class created to function as a container sense to convey dictionary examples and other phrase-level dictionary elements presented as a separate block by the lexicographer."
   LexicalSensesAllDF[is.na(LexicalSensesAllDF)] <- ""

   # tokenize content
   if('skos.definition.la' %in% colnames(LexicalSensesAllDF)){
      LexicalSensesAllDF$skos.definition.la <- gsub("([[:punct:]])", " \\1 ", LexicalSensesAllDF$skos.definition.la) %>%
         gsub("(.*)", " \\1 ", .) %>%
         gsub("\\s+", " ", .) %>%
         gsub("^\\s$", "", .)
   }
   LexicalSensesAllDF$skos.definition.pt <- gsub("([[:punct:]])", " \\1 ", LexicalSensesAllDF$skos.definition.pt) %>%
      gsub("(.*)", " \\1 ", .) %>%
      gsub("\\s+", " ", .) %>%
      gsub("^\\s$", "", .)

   LexicalSensesAllDF$lexicog.usageExample <- unlist(lapply(seq_along(LexicalSensesAllDF$ontolex.LexicalSense), function(i) str_flatten_comma(UsageExamplesDF$lexicog.UsageExample[str_detect(UsageExamplesDF$lexicog.UsageExample, paste0(LexicalSensesAllDF$ontolex.LexicalSense[i],"\\."))])))
   LexicalSensesAllDF$lexicog.usageExample <- gsub(dictPrefix, "UsageEx\\:", LexicalSensesAllDF$lexicog.usageExample)
   LexicalSensesAllDF$ontolex.LexicalSense <- gsub(dictPrefix, "LexSense\\:", LexicalSensesAllDF$ontolex.LexicalSense)


   #####################################
   # UPDATE 'lexicog:UsageExamples' DATA FRAME
   #####################################
   UsageExamplesDF$lexicog.UsageExample <- gsub(dictPrefix, "UsageEx\\:", UsageExamplesDF$lexicog.UsageExample)

   # remove ending punctuation and square brackets
   UsageExamplesDF$skos.note <- gsub("[\\,|\\:|\\;]$", "", UsageExamplesDF$skos.note) %>%
      gsub('\\[','',.) %>%
      gsub('\\]','',.)
   UsageExamplesDF$rdfs.value.la <- gsub("[\\,|\\:|\\;|\\.]$", "", UsageExamplesDF$rdfs.value.la) %>%
      gsub('\\[','',.) %>%
      gsub('\\]','',.)
   UsageExamplesDF$lexinfo.note <- gsub("[\\,|\\:|\\;]$", "", UsageExamplesDF$lexinfo.note) %>%
      gsub('\\[','',.) %>%
      gsub('\\]','',.)
   UsageExamplesDF$rdfs.value.pt <- gsub("[\\,|\\:|\\;|\\.]$", "", UsageExamplesDF$rdfs.value.pt) %>%
      gsub('\\[','',.) %>%
      gsub('\\]','',.)
   UsageExamplesDF$lexinfo.note.1 <- gsub("[\\,|\\:|\\;]$", "", UsageExamplesDF$lexinfo.note.1) %>%
      gsub('\\[','',.) %>%
      gsub('\\]','',.)

   #
   # add labels
   UsageExamplesDF$rdfs.label <- gsub("(.*)", "\\L\\1", UsageExamplesDF$rdfs.value.la, perl = T) %>%
      gsub("j", "i", .) %>%
      gsub("v", "u", .)

   # tokenize content
   UsageExamplesDF$rdfs.value.la <- gsub("([[:punct:]])", " \\1 ", UsageExamplesDF$rdfs.value.la) %>%
      gsub("(.*)", " \\1 ", .) %>%
      gsub("\\s+", " ", .) %>%
      gsub("^\\s$", "", .)
   UsageExamplesDF$rdfs.value.pt <- gsub("([[:punct:]])", " \\1 ", UsageExamplesDF$rdfs.value.pt) %>%
      gsub("(.*)", " \\1 ", .) %>%
      gsub("\\s+", " ", .) %>%
      gsub("^\\s$", "", .)


   #####################################
   # CREATE A LIST OF DATA FRAMES
   #####################################
   DictDataDFsList <- list(EntriesDF=EntriesDF,LexComponentsDF=LexComponentsDF,LexicalEntriesDF=LexicalEntriesDF,FormsDF=FormsDF,LexicalSensesDF=LexicalSensesAllDF,UsageExamplesDF=UsageExamplesDF)

   print(date())
   return(DictDataDFsList)
}

