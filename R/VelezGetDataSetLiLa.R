# arguments
# dictDataDir = path to the directory with TSV files exported from Numbers file.
# dictDataDir <- velezDataDir

GetDataSet <- function(dictDataDir){

   ## GET SOURCE DATA TABLES

   specialEntriesPath <- paste0(dictDataDir,dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^specialEntries"))])
   specialEntriesDF <- read.csv(specialEntriesPath, sep = "\t")

   LexicalEntriesPath <- paste0(dictDataDir,dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^LexicalEntries"))])
   LexicalEntriesDF <- read.csv(LexicalEntriesPath, sep = "\t")

   FormsPath <- paste0(dictDataDir,dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^Forms"))])
   FormsDF <- read.csv(FormsPath, sep = "\t")

   LexicalSensesPath <- paste0(dictDataDir,dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^LexicalSenses"))])
   LexicalSensesDF <- read.csv(LexicalSensesPath, sep = "\t")

   UsageExamplesPath <- paste0(dictDataDir,dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^UsageExamples"))])
   UsageExamplesDF <- read.csv(UsageExamplesPath, sep = "\t")

   NotesPath <- paste0(dictDataDir,dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^Notes"))])
   NotesDF <- read.csv(NotesPath, sep = "\t")

   crossRefPath <- paste0(dictDataDir,dir(dictDataDir)[which(str_detect(dir(dictDataDir),"^crossRef"))])
   crossRefDF <- read.csv(crossRefPath, sep = "\t")


   ###################################################################################################
   # FILTER OUT DUPLICATES 'LexicalEntries' (SAME lila_id AND senseGroup)
   Duplicates <- unlist(lapply(seq_along(LexicalEntriesDF$ontolex.LexicalEntry), function(i) LexicalEntriesDF$ontolex.LexicalEntry[identical(LexicalEntriesDF$senseGroup[i], LexicalEntriesDF$senseGroup[i-1]) & identical(LexicalEntriesDF$dcterms.identifier[i], LexicalEntriesDF$dcterms.identifier[i-1])][i]))
   Duplicates <- Duplicates[!is.na(Duplicates)]
   LexicalEntriesDF <- LexicalEntriesDF[!LexicalEntriesDF$ontolex.LexicalEntry %in% Duplicates,]
   # filter out 'lator legis' (not picked by the rule above)
   LexicalEntriesDF <- LexicalEntriesDF[LexicalEntriesDF$ontolex.LexicalEntry!='Velez1744.n1959.e13',]
   ###################################################################################################


   # CREATE 'lexicog:Entries' DATA FRAME
   EntriesDF <- data.frame(lexicog.Entry=lexicog.Entry_ids <- unique(gsub("\\.e\\d\\d$", "", LexicalEntriesDF$ontolex.LexicalEntry)), stringsAsFactors = F)
   EntriesDF$rdfs.label <- unlist(lapply(seq_along(EntriesDF$lexicog.Entry), function(i) paste0("Velez (1744) entry for '", FormsDF$ontolex.writtenRep[FormsDF$ontolex.Form == paste0(EntriesDF$lexicog.Entry[i],".e01.f0")], "'")))
   EntriesDF$lexicog.describes <- unlist(lapply(seq_along(EntriesDF$lexicog.Entry), function(i) str_flatten_comma(LexicalEntriesDF$ontolex.LexicalEntry[str_detect(LexicalEntriesDF$ontolex.LexicalEntry, EntriesDF$lexicog.Entry[i])])))
   EntriesDF$lexicog.subComponent <- EntriesDF$lexicog.describes

   EntriesDF <- left_join(EntriesDF,crossRefDF)
   EntriesDF <- rbind(EntriesDF,specialEntriesDF)

   EntriesDF$lexicog.Entry <- gsub("Velez1744", "velez_entry", EntriesDF$lexicog.Entry)
   EntriesDF$lexicog.describes <- gsub("^(.*e01), (.*)", "\\1", EntriesDF$lexicog.describes) %>%
      gsub("Velez1744", "velez_le", .)
   EntriesDF$lexicog.subComponent <- gsub("^(.*e01), (.*)", "\\2", EntriesDF$lexicog.subComponent) %>%
      gsub("^.*e01$", "", .) %>%
      gsub("Velez1744", "velez_comp", .)
   EntriesDF[is.na(EntriesDF)] <- ""


   # CREATE 'lexicog:Components' DATA FRAME
   ComponentsDF <- data.frame(lexicog.LexicographicComponent = LexicalEntriesDF$ontolex.LexicalEntry[str_detect(LexicalEntriesDF$ontolex.LexicalEntry, "e01", negate=T)], stringsAsFactors = F)
   ComponentsDF$rdfs.label <- unlist(lapply(seq_along(ComponentsDF$lexicog.LexicographicComponent), function(i) paste0("Lexicographic Component for subentry '", FormsDF$ontolex.writtenRep[FormsDF$ontolex.Form == paste0(ComponentsDF$lexicog.LexicographicComponent[i],".f0")],"' of '", FormsDF$ontolex.writtenRep[FormsDF$ontolex.Form == paste0(gsub("e\\d\\d", "e01", ComponentsDF$lexicog.LexicographicComponent[i]),".f0")], "'"))) %>%
      gsub("\\[", "", .) %>%
      gsub("\\]", "", .)
   ComponentsDF$skos.note <- unlist(lapply(seq_along(ComponentsDF$lexicog.LexicographicComponent), function(i) LexicalEntriesDF$rdfs.comment[LexicalEntriesDF$ontolex.LexicalEntry == ComponentsDF$lexicog.LexicographicComponent[i]]))
   ComponentsDF$lexicog.describes <- ComponentsDF$lexicog.LexicographicComponent %>%
      gsub("Velez1744", "velez_le", .)
   ComponentsDF$lexicog.LexicographicComponent <- gsub("Velez1744", "velez_comp", ComponentsDF$lexicog.LexicographicComponent)


   # UPGRADE 'ontolex:LexicalEntries' DATA FRAME
   #canonicalForm_idList <- FormsDF$ontolex.Form[str_detect(FormsDF$ontolex.Form, "f0")]
   #LexicalEntriesDF$ontolex.canonicalForm <- unlist(lapply(seq_along(LexicalEntriesDF$ontolex.LexicalEntry), function(i) canonicalForm_idList[str_detect(canonicalForm_idList, LexicalEntriesDF$ontolex.LexicalEntry[i])]))
   LexicalEntriesDF$ontolex.canonicalForm <- LexicalEntriesDF$dcterms.identifier

   #otherForm_idList <- FormsDF$ontolex.Form[str_detect(FormsDF$ontolex.Form, "f0", negate = T)]
   #LexicalEntriesDF$ontolex.otherForm <- unlist(lapply(seq_along(LexicalEntriesDF$ontolex.LexicalEntry), function(i) str_flatten_comma(otherForm_idList[str_detect(otherForm_idList, LexicalEntriesDF$ontolex.LexicalEntry[i])])))

   sense_idList <- unique(c(LexicalSensesDF$ontolex.LexicalSense, gsub("\\.u\\d\\d$", "", UsageExamplesDF$lexicog.UsageExample)))
   LexicalEntriesDF$ontolex.sense <- unlist(lapply(seq_along(LexicalEntriesDF$senseGroup), function(i) str_flatten_comma(sense_idList[str_detect(sense_idList, LexicalEntriesDF$senseGroup[i])])))

   for(i in seq_along(LexicalEntriesDF$ontolex.LexicalEntry)){
      if(str_detect(LexicalEntriesDF$ontolex.LexicalEntry[i], "e01")){
         LexicalEntriesDF$rdfs.label[i] <- paste0("Velez (1744) lexical entry for '", FormsDF$ontolex.writtenRep[FormsDF$ontolex.Form == paste0(LexicalEntriesDF$ontolex.LexicalEntry[i],".f0")], "'")
      }else{
         LexicalEntriesDF$rdfs.label[i] <- paste0("Velez (1744) lexical entry for '", FormsDF$ontolex.writtenRep[FormsDF$ontolex.Form == paste0(LexicalEntriesDF$ontolex.LexicalEntry[i],".f0")], "', subentry of '", FormsDF$ontolex.writtenRep[FormsDF$ontolex.Form == paste0(gsub("e\\d\\d", "e01", LexicalEntriesDF$ontolex.LexicalEntry[i]),".f0")], "'")
      }
   }

   LexicalEntriesDF <- left_join(LexicalEntriesDF, NotesDF)
   LexicalEntriesDF[is.na(LexicalEntriesDF)] <- ""

   LexicalEntriesDF$ontolex.LexicalEntry <- gsub("Velez1744", "velez_le", LexicalEntriesDF$ontolex.LexicalEntry)
   #LexicalEntriesDF$ontolex.canonicalForm <- gsub("Velez1744", "velez_form", LexicalEntriesDF$ontolex.canonicalForm)
   #LexicalEntriesDF$ontolex.otherForm <- gsub("Velez1744", "velez_form", LexicalEntriesDF$ontolex.otherForm)
   LexicalEntriesDF$ontolex.sense <- gsub("Velez1744", "velez_ls", LexicalEntriesDF$ontolex.sense)
   LexicalEntriesDF$ontolex.evokes <- gsub("velez_ls", "velez_lc", LexicalEntriesDF$ontolex.sense) %>%
      gsub("\\.s", "\\.c", .)
   #LexicalEntriesDF <- LexicalEntriesDF[,c(1,2,4:8,12,9:11,13)]
   LexicalEntriesDF <- LexicalEntriesDF[,c(1,2,6,8,11,9,10,12)]


   # UPGRADE 'ontolex:Forms' DATA FRAME
   #FormsDF <- FormsDF[,c(1,3:6)]
   #FormsDF$ontolex.Form <- gsub("Velez1744", "velez_form", FormsDF$ontolex.Form)



   # UPGRADE 'ontolex:LexicalSenses' DATA FRAME
   LexicalSensesAllDF <- data.frame(ontolex.LexicalSense=sense_idList[order(sense_idList)], stringsAsFactors = F)
   LexicalSensesAllDF <- left_join(LexicalSensesAllDF,LexicalSensesDF)
   LexicalSensesAllDF[is.na(LexicalSensesAllDF)] <- ""

   LexicalSensesAllDF$rdfs.label <- unlist(lapply(seq_along(LexicalSensesAllDF$ontolex.LexicalSense), function(i) paste(LexicalSensesAllDF$skos.definition.la[i], LexicalSensesAllDF$skos.definition.pt[i])))
   LexicalSensesAllDF$rdfs.label <- gsub("  ", " ", LexicalSensesAllDF$rdfs.label) %>%
      gsub("  ", " ", .) %>%
      gsub("  ", " ", .) %>%
      gsub("^ (.*)", "\\1", .) %>%
      gsub(" $", "", .) %>%
      gsub("[,|;|:]$", ".", .) %>%
      gsub("\\[", "", .) %>%
      gsub("\\]", "", .)

   LexicalSensesAllDF$rdfs.label[LexicalSensesAllDF$rdfs.label==""] <- "Undefined meaning (see related usage examples)."
   LexicalSensesAllDF$rdfs.label[str_detect(LexicalSensesAllDF$ontolex.LexicalSense, "s99")] <- "Further meanings (see related usage examples)."
   LexicalSensesAllDF$rdfs.comment[str_detect(LexicalSensesAllDF$ontolex.LexicalSense, "s99")] <- "This definitionless sense is an instance of the ontolex:LexicalSense class created to function as a container to convey dictionary examples and other phrase-level dictionary elements presented as a separate block by the lexicographer."
   LexicalSensesAllDF[is.na(LexicalSensesAllDF)] <- ""

   LexicalSensesAllDF <- LexicalSensesAllDF[,c(1,6,7,2:5)]
   LexicalSensesAllDF$ontolex.isLexicalizedSenseOf <- gsub("Velez1744", "velez_lc", LexicalSensesAllDF$ontolex.LexicalSense) %>%
      gsub("\\.s", "\\.c", .)
   LexicalSensesAllDF$lexicog.usageExample <- unlist(lapply(seq_along(LexicalSensesAllDF$ontolex.LexicalSense), function(i) str_flatten_comma(UsageExamplesDF$lexicog.UsageExample[str_detect(UsageExamplesDF$lexicog.UsageExample, LexicalSensesAllDF$ontolex.LexicalSense[i])])))
   LexicalSensesAllDF$lexicog.usageExample <- gsub("Velez1744", "velez_ue", LexicalSensesAllDF$lexicog.usageExample)
   LexicalSensesAllDF$ontolex.LexicalSense <- gsub("Velez1744", "velez_ls", LexicalSensesAllDF$ontolex.LexicalSense)


   # CREATE 'ontolex:LexicalConcepts' DATA FRAME
   LexicalConceptsDF <- data.frame(ontolex.LexicalConcept=LexicalSensesAllDF$ontolex.isLexicalizedSenseOf)
   LexicalConceptsDF$rdfs.label <- gsub("velez_lc.", "concept ", LexicalConceptsDF$ontolex.LexicalConcept)

   # UPDATE 'lexicog:UsageExamples' DATA FRAME
   UsageExamplesDF$lexicog.UsageExample <- gsub("Velez1744", "velez_ue", UsageExamplesDF$lexicog.UsageExample)
   UsageExamplesDF$rdfs.label <- UsageExamplesDF$rdfs.value.la
   UsageExamplesDF$rdfs.label <- gsub("[,|;|:]$", ".", UsageExamplesDF$rdfs.label) %>%
      gsub("\\[", "", .) %>%
      gsub("\\]", "", .)
   UsageExamplesDF <- UsageExamplesDF[,c(1,7,2:6)]


   DictDataDFsList <- list(EntriesDF=EntriesDF,ComponentsDF=ComponentsDF,LexicalEntriesDF=LexicalEntriesDF,LexicalSensesDF=LexicalSensesAllDF,LexicalConceptsDF=LexicalConceptsDF,UsageExamplesDF=UsageExamplesDF)
   saveRDS(DictDataDFsList, paste0(dictDataDir,"VelezDFList.rds"))

   return(DictDataDFsList)
}
