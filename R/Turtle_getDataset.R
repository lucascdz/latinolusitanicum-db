# ARGUMENTS
sourceDir <- "./sources/Velez/Velez.1744.indextotiusartis.1.4.0/"
sourcePrefix <- "Velez1744\\."
targetPub <- "LiLa"

#GetDataset <- function(sourceDir,sourcePrefix,targetPub){
   print(date())

   ############################
   ## GET SOURCE DATA TABLES ##
   ############################

   LexicalEntriesPath <- paste0(sourceDir,dir(sourceDir)[which(str_detect(dir(sourceDir),"^LexicalEntries"))])
   LexicalEntriesDF <- read.csv(LexicalEntriesPath, sep = "\t")

   FormsPath <- paste0(sourceDir,dir(sourceDir)[which(str_detect(dir(sourceDir),"^Forms"))])
   FormsDF <- read.csv(FormsPath, sep = "\t")

   LexicalSensesPath <- paste0(sourceDir,dir(sourceDir)[which(str_detect(dir(sourceDir),"^LexicalSenses"))])
   LexicalSensesDF <- read.csv(LexicalSensesPath, sep = "\t")

   UsageExamplesPath <- paste0(sourceDir,dir(sourceDir)[which(str_detect(dir(sourceDir),"^UsageExamples"))])
   UsageExamplesDF <- read.csv(UsageExamplesPath, sep = "\t")

   DecompPath <- paste0(sourceDir,dir(sourceDir)[which(str_detect(dir(sourceDir),"^Decomp"))])
   DecompDF <- read.csv(DecompPath, sep = "\t")

   # FILTER OUT DUPLICATES 'LexicalEntries' (SAME senseGroup AND lila_id [once not empty])
   if(identical(targetPub,"LiLa")){
      LexicalEntriesDF$testDuplicates <- unlist(lapply(seq_along(LexicalEntriesDF$subClass), function(i) str_flatten(c(LexicalEntriesDF$senseGroup[i],LexicalEntriesDF$ontolex.canonicalForm[i]), collapse = '_')))
      for (i in seq_along(LexicalEntriesDF$testDuplicates)){
         if (identical(LexicalEntriesDF$testDuplicates[i],LexicalEntriesDF$testDuplicates[i-1])){
            LexicalEntriesDF$ontolex.LexicalEntry[i] <- NA
         }
      }
      LexicalEntriesDF <- LexicalEntriesDF[!is.na(LexicalEntriesDF$ontolex.LexicalEntry),]
   }

   # CREATE 'lexicog:Entries' DATA FRAME
   EntriesDF <- LexicalEntriesDF[str_detect(LexicalEntriesDF$ontolex.LexicalEntry,'e01$'),colnames(LexicalEntriesDF) %in% c("ontolex.LexicalEntry","PRE.note","POS.note")]
   colnames(EntriesDF) <- c("lexicog.describes","lexinfo.note","skos.note")
   EntriesDF$lexicog.Entry <- gsub('.e01$','',EntriesDF$lexicog.describes)
   # get form labels
   entryLabelDF <- FormsDF[str_detect(FormsDF$ontolex.Form,'\\.e01\\.'),colnames(FormsDF) %in% c('ontolex.Form',"ontolex.writtenRep")]
   entryLabelDF$lexicog.Entry <- gsub('\\.e\\d+.*','',entryLabelDF$ontolex.Form)
   entryLabelList <- split(entryLabelDF,entryLabelDF$lexicog.Entry)
   entryLabelDF <- data.frame(
      lexicog.Entry=names(entryLabelList),
      rdfs.label=unlist(lapply(seq_along(entryLabelList), function(i) str_flatten_comma(entryLabelList[[i]]$ontolex.writtenRep))),
      stringsAsFactors = F)
   entryLabelDF$rdfs.label <- unlist(lapply(seq_along(entryLabelDF$rdfs.label), function(i) paste0(gsub("([A-z]*).*","\\1", sourcePrefix)," entry for '", entryLabelDF$rdfs.label[i],"'")))
   EntriesDF <- left_join(EntriesDF,entryLabelDF)
   # get subComponents
   subComponentDF <- LexicalEntriesDF[!str_detect(LexicalEntriesDF$ontolex.LexicalEntry, 'e01') & !str_detect(LexicalEntriesDF$ontolex.LexicalEntry, 'e\\d\\d[a-z]'),]
   subComponentDF$lexicog.Entry <- gsub('\\.e\\d+.?$','',subComponentDF$ontolex.LexicalEntry)
   subComponentList <- split(subComponentDF,subComponentDF$lexicog.Entry)
   subComponentDF <- data.frame(
      lexicog.Entry=names(subComponentList),
      lexicog.subComponent=unlist(lapply(seq_along(subComponentList), function(i) str_flatten_comma(subComponentList[[i]]$ontolex.LexicalEntry))),
      stringsAsFactors = F)
   EntriesDF <- left_join(EntriesDF,subComponentDF)
   # adjust prefixes
   EntriesDF$lexicog.Entry <- gsub(sourcePrefix, "lexicogEntry\\:", EntriesDF$lexicog.Entry)
   EntriesDF$lexicog.describes <- gsub(sourcePrefix, "lexicalEntry\\:", EntriesDF$lexicog.describes)
   EntriesDF$lexicog.subComponent <- gsub(sourcePrefix, "lexicogComponent\\:", EntriesDF$lexicog.subComponent)
   EntriesDF[is.na(EntriesDF)] <- ""
   rownames(EntriesDF) <- NULL
   EntriesDF <- EntriesDF[,c(
      which(colnames(EntriesDF)=="lexicog.Entry"),
      which(colnames(EntriesDF)=="rdfs.label"),
      which(colnames(EntriesDF)=="lexicog.describes"),
      which(colnames(EntriesDF)=="lexinfo.note"),
      which(colnames(EntriesDF)=="lexicog.subComponent"),
      which(colnames(EntriesDF)=="skos.note"))]

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
   LexComponentsDF$lexicog.LexicographicComponent <- gsub(sourcePrefix, "lexicogComponent:", LexComponentsDF$lexicog.LexicographicComponent)
   LexComponentsDF$lexicog.describes <- gsub(sourcePrefix, "lexicalEntry\\:", LexComponentsDF$lexicog.describes)



   ####################################
   # UPDATE 'ontolex:LexicalEntries' DATA FRAME
   ###################################

   # add links to lexical forms
   if('POS.note' %in% colnames(LexicalEntriesDF)){
      colnames(LexicalEntriesDF)[which(str_detect(colnames(LexicalEntriesDF),"POS.note"))] <- "ontolex.lexicalForm"
   }
   LexicalEntriesDF$ontolex.lexicalForm <- ''
   if(identical(targetPub,"LiLa")){
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
      LexicalEntriesDF$decomp.subterm[i] <- str_flatten_comma(DecompDF$decomp.subterm[DecompDF$ontolex.LexicalEntry==LexicalEntriesDF$ontolex.LexicalEntry[i]])
   }

   # add 'rdfs.labels' to all instances
   for(i in seq_along(LexicalEntriesDF$ontolex.LexicalEntry)){
      LexicalEntriesDF$rdfs.label[i] <- paste0(gsub("([A-z]*).*","\\1", sourcePrefix)," lexical entry for '", FormsDF$ontolex.writtenRep[FormsDF$ontolex.Form == paste0(LexicalEntriesDF$ontolex.LexicalEntry[i],".f01")], "'")
   }
   # remove square brackets '[' ']' from labels
   LexicalEntriesDF$rdfs.label <- gsub("\\[", "", LexicalEntriesDF$rdfs.label) %>%
         gsub("\\]", "", .)

   # add special labels for lexical entries created exclusively as MWE components
   for(i in which(str_detect(LexicalEntriesDF$ontolex.LexicalEntry, "[a-z]$"))){
      LexicalEntriesDF$rdfs.label[i] <- gsub("[A-z]+ lexical entry for '(.*)'", "Added lexical entry for [\\1], component of a multiword expression", LexicalEntriesDF$rdfs.label[i])
   }

   # updates all prefixes
   LexicalEntriesDF$ontolex.LexicalEntry <- gsub(sourcePrefix, "lexicalEntry\\:", LexicalEntriesDF$ontolex.LexicalEntry)
   LexicalEntriesDF$ontolex.canonicalForm <- gsub(sourcePrefix, "Form\\:", LexicalEntriesDF$ontolex.canonicalForm)
   LexicalEntriesDF$ontolex.lexicalForm <- gsub(sourcePrefix, "Form\\:", LexicalEntriesDF$ontolex.lexicalForm)
   LexicalEntriesDF$decomp.subterm <- gsub(sourcePrefix, "lexicalEntry\\:", LexicalEntriesDF$decomp.subterm)
   LexicalEntriesDF$ontolex.sense <- gsub(sourcePrefix, "lexicalSense\\:", LexicalEntriesDF$ontolex.sense)
   if(identical(identical(targetPub,"LiLa"), FALSE)){
      LexicalEntriesDF$ontolex.otherForm <- gsub(sourcePrefix, "Form\\:", LexicalEntriesDF$ontolex.otherForm)
   }

   #####################################
   # UPGRADE 'ontolex:Forms' DATA FRAME
   #####################################
   FormsDF$ontolex.Form <- gsub(sourcePrefix, "Form\\:", FormsDF$ontolex.Form)
   if(identical(targetPub,"LiLa")){
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
   LexicalSensesAllDF$lexicog.usageExample <- gsub(sourcePrefix, "usageExample\\:", LexicalSensesAllDF$lexicog.usageExample)
   LexicalSensesAllDF$ontolex.LexicalSense <- gsub(sourcePrefix, "lexicalSense\\:", LexicalSensesAllDF$ontolex.LexicalSense)


   #####################################
   # UPDATE 'lexicog:UsageExamples' DATA FRAME
   #####################################
   UsageExamplesDF$lexicog.UsageExample <- gsub(sourcePrefix, "usageExample\\:", UsageExamplesDF$lexicog.UsageExample)

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
   # CREATE A LIST OF DATA FRAMES AND EXPORT AS TSV
   #####################################
   DictDatasetList <- list(EntriesDF=EntriesDF,LexComponentsDF=LexComponentsDF,LexicalEntriesDF=LexicalEntriesDF,FormsDF=FormsDF,LexicalSensesDF=LexicalSensesAllDF,UsageExamplesDF=UsageExamplesDF)

   # save dataset as TSV
   dictDataFolder <- paste0('./data/',gsub('^([A-z]+).*','\\L\\1',sourcePrefix, perl = T),targetPub,'-dataset/')
   system(paste0('mkdir ',dictDataFolder))
   lapply(seq_along(DictDatasetList), function(i) write_tsv(DictDatasetList[[i]], 'NAME' ))
   
   
   print(date())
   return(DictDataDFsList)
}

