# ARGUMENTS
sourceDir <- "./sources/Velez/Velez.1744.indextotiusartis.1.4.0/"
sourcePrefix <- "Velez1744\\."
targetPub <- "LiLa"

#GetDataset <- function(sourceDir,sourcePrefix,targetPub){


   # GET SOURCE DATA TABLES
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
   

   # PREPARE 'lexicog:DataFrames'
   LexicogDF <- LexicalEntriesDF[,colnames(LexicalEntriesDF) %in% c("ontolex.LexicalEntry","PRE.note","POS.note")]
   colnames(LexicogDF) <- c("lexicog.describes","lexinfo.note","skos.note")

      
   # CREATE 'lexicog:Entries' DATA FRAME
   EntriesDF <- LexicogDF[str_detect(LexicogDF$lexicog.describes,'e01$'),]
   EntriesDF$lexicog.Entry <- gsub('.e01$','',EntriesDF$lexicog.describes)
   # get form labels
   entryLabelDF <- FormsDF[str_detect(FormsDF$ontolex.Form,'e01\\.f01'),colnames(FormsDF) %in% c('ontolex.Form',"ontolex.writtenRep")]
   entryLabelDF$lexicog.Entry <- gsub('\\.e\\d+.*','',entryLabelDF$ontolex.Form)
   entryLabelDF$rdfs.label <- unlist(lapply(seq_along(entryLabelDF$lexicog.Entry), function(i) paste0(gsub("([A-z]*).*","\\1", sourcePrefix),"’s entry for '", entryLabelDF$ontolex.writtenRep[i],"'")))
   EntriesDF <- left_join(EntriesDF,entryLabelDF[,colnames(entryLabelDF) %in% c("lexicog.Entry","rdfs.label")])
   # get subComponents
   subComponentDF <- LexicogDF[!str_detect(LexicalEntriesDF$ontolex.LexicalEntry, 'e01') & !str_detect(LexicalEntriesDF$ontolex.LexicalEntry, 'e\\d\\d[a-z]'),]
   subComponentDF$lexicog.Entry <- gsub('\\.e\\d+.?$','',subComponentDF$lexicog.describes)
   subComponentDF$lexicog.subComponent <- gsub('(.*)','\\1_comp',subComponentDF$lexicog.describes)
   subComponentList <- split(subComponentDF,subComponentDF$lexicog.Entry)
   subComponentDF <- data.frame(
      lexicog.Entry=names(subComponentList),
      lexicog.subComponent=unlist(lapply(seq_along(subComponentList), function(i) str_flatten_comma(subComponentList[[i]]$lexicog.subComponent))),
      stringsAsFactors = F)
   EntriesDF <- left_join(EntriesDF,subComponentDF)
   # adjust prefixes
   EntriesDF$lexicog.Entry <- gsub(sourcePrefix, "lexicogEntry\\:", EntriesDF$lexicog.Entry)
   EntriesDF$lexicog.describes <- gsub(sourcePrefix, "lexicalEntry\\:", EntriesDF$lexicog.describes)
   EntriesDF$lexicog.subComponent <- gsub(sourcePrefix, "lexicogComponent\\:", EntriesDF$lexicog.subComponent)
   EntriesDF[is.na(EntriesDF)] <- ""
   EntriesDF <- EntriesDF[,c(
      which(colnames(EntriesDF)=="lexicog.Entry"),
      which(colnames(EntriesDF)=="rdfs.label"),
      which(colnames(EntriesDF)=="lexinfo.note"),
      which(colnames(EntriesDF)=="lexicog.describes"),
      which(colnames(EntriesDF)=="lexicog.subComponent"),
      which(colnames(EntriesDF)=="skos.note"))]
   EntriesDF[is.na(EntriesDF)] <- ''
   rownames(EntriesDF) <- NULL
   ## EntriesDF DONE
   

   # CREATE 'lexicog:Components' DATA FRAME
   ComponentsDF <- LexicogDF[!str_detect(LexicogDF$lexicog.describes, 'e01') & !str_detect(LexicogDF$lexicog.describes, 'e\\d\\d[a-z]'),]
   ComponentsDF$lexicog.Entry <- gsub('\\.e\\d+.?$','',ComponentsDF$lexicog.describes)
   ComponentsDF$lexicog.LexicographicComponent <- gsub('(.*)','\\1_comp',ComponentsDF$lexicog.describes)
   # add labels
   compLabelDF <- FormsDF[str_detect(FormsDF$ontolex.Form,'\\.f01') & !str_detect(FormsDF$ontolex.Form,'\\.e01\\.'),colnames(FormsDF) %in% c('ontolex.Form',"ontolex.writtenRep")]
   compLabelDF$lexicog.LexicographicComponent <- gsub('^(.*)\\.f\\d+.*','\\1_comp',compLabelDF$ontolex.Form)
   compLabelDF$rdfs.label <- unlist(lapply(seq_along(compLabelDF$ontolex.Form), function(i) paste0(gsub("([A-z]*).*","\\1", sourcePrefix),"’s subentry for '", compLabelDF$ontolex.writtenRep[i],"'")))
   ComponentsDF <- left_join(ComponentsDF,compLabelDF)
   ComponentsDF <- ComponentsDF[,c(
      which(colnames(ComponentsDF)=="lexicog.LexicographicComponent"),
      which(colnames(ComponentsDF)=="rdfs.label"),
      which(colnames(ComponentsDF)=="lexinfo.note"),
      which(colnames(ComponentsDF)=="lexicog.describes"),
      which(colnames(ComponentsDF)=="skos.note"))]
   # update prefixes
   ComponentsDF$lexicog.LexicographicComponent <- gsub(sourcePrefix, "lexicogComponent:", ComponentsDF$lexicog.LexicographicComponent)
   ComponentsDF$lexicog.describes <- gsub(sourcePrefix, "lexicalEntry\\:", ComponentsDF$lexicog.describes)
   ComponentsDF[is.na(ComponentsDF)] <- ''
   rownames(ComponentsDF) <- NULL
   ## Components DONE
   
   # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
   
   # UPDATE 'ontolex:LexicalEntries' DATA FRAME
   # add links to lexical forms
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
   ontolexSensesDF <- data.frame(
      senses=unique(c(LexicalSensesDF$ontolex.LexicalSense, gsub("\\.u\\d\\d.?$", "", UsageExamplesDF$lexicog.UsageExample))),
      stringsAsFactors = F)
   ontolexSensesDF$senseGroup <- gsub('\\.s\\d+.*$','',ontolexSensesDF$senses)
   ontolexSensesList <- split(ontolexSensesDF,ontolexSensesDF$senseGroup)
   ontolexSensesDF <- data.frame(
      senseGroup=names(ontolexSensesList),
      ontolex.sense=unlist(lapply(seq_along(ontolexSensesList), function(i) str_flatten_comma(ontolexSensesList[[i]]$senses))),
      stringsAsFactors = F)
   LexicalEntriesDF <- left_join(LexicalEntriesDF,ontolexSensesDF)
   # add 'decomp.subterm' property to mwes
   DecompDF <- DecompDF[,c(which(str_detect(colnames(DecompDF),'ontolex|decomp')))]

      # >>>>>>>>>>>>>>>>> TEST decomp.blanknode <<<<<<<<<<<<<<<<<<
   
   LexicalEntriesDF <- left_join(LexicalEntriesDF,DecompDF)
   # add 'rdfs.labels' to all instances
   lexLabelDF <- FormsDF[str_detect(FormsDF$ontolex.Form,'\\.f01$'),colnames(FormsDF) %in% c('ontolex.Form',"ontolex.writtenRep")]
   lexLabelDF$ontolex.LexicalEntry <- gsub('\\.f01','',lexLabelDF$ontolex.Form)
   LexicalEntriesDF <- left_join(LexicalEntriesDF,lexLabelDF)
   LexicalEntriesDF$rdfs.label <- unlist(lapply(seq_along(LexicalEntriesDF$rdfs.label), function(i) paste0(gsub("([A-z]*).*","\\1", sourcePrefix),"’s lexical entry for '", LexicalEntriesDF$ontolex.writtenRep[i],"'"))) %>%
      gsub("\\[", "", .) %>%
      gsub("\\]", "", .)
   # add special labels for lexical entries created exclusively as MWE components
   for(i in which(str_detect(LexicalEntriesDF$ontolex.LexicalEntry, "[a-z]$"))){
      LexicalEntriesDF$rdfs.label[i] <- gsub("[A-z]+’s (.*)", "Added \\1, component of a multiword expression", LexicalEntriesDF$rdfs.label[i])
   }
   LexicalEntriesDF <- LexicalEntriesDF[,c(
      which(colnames(LexicalEntriesDF)=="ontolex.LexicalEntry"),
      which(colnames(LexicalEntriesDF)=="subClass"),
      which(colnames(LexicalEntriesDF)=="rdfs.label"),
      which(colnames(LexicalEntriesDF)=="decomp.subterm"),
      which(colnames(LexicalEntriesDF)=="ontolex.canonicalForm"),
      which(colnames(LexicalEntriesDF)=="ontolex.lexicalForm"),
      which(colnames(LexicalEntriesDF)=="lexinfo.note"),
      which(colnames(LexicalEntriesDF)=="ontolex.sense"),
      which(colnames(LexicalEntriesDF)=="skos.note"))]
   # update prefixes
   LexicalEntriesDF$ontolex.LexicalEntry <- gsub(sourcePrefix, "lexicalEntry\\:", LexicalEntriesDF$ontolex.LexicalEntry)
   LexicalEntriesDF$ontolex.canonicalForm <- gsub(sourcePrefix, "Form\\:", LexicalEntriesDF$ontolex.canonicalForm)
   LexicalEntriesDF$ontolex.lexicalForm <- gsub(sourcePrefix, "Form\\:", LexicalEntriesDF$ontolex.lexicalForm)
   LexicalEntriesDF$decomp.subterm <- gsub(sourcePrefix, "lexicalEntry\\:", LexicalEntriesDF$decomp.subterm)
   LexicalEntriesDF$ontolex.sense <- gsub(sourcePrefix, "lexicalSense\\:", LexicalEntriesDF$ontolex.sense)
   if(identical(identical(targetPub,"LiLa"), FALSE)){
      LexicalEntriesDF$ontolex.otherForm <- gsub(sourcePrefix, "Form\\:", LexicalEntriesDF$ontolex.otherForm)
   }
   LexicalEntriesDF[is.na(LexicalEntriesDF)] <- ''
   rownames(LexicalEntriesDF) <- NULL
   # LexicalEntries DONE
   
   # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

   # UPGRADE 'ontolex:Forms' DATA FRAME

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
   DictDatasetList <- list(EntriesDF=EntriesDF,ComponentsDF=ComponentsDF,LexicalEntriesDF=LexicalEntriesDF,FormsDF=FormsDF,LexicalSensesDF=LexicalSensesAllDF,UsageExamplesDF=UsageExamplesDF)

   # save dataset as TSV
   dictDataFolder <- paste0('./data/',gsub('^([A-z]+).*','\\L\\1',sourcePrefix, perl = T),targetPub,'-dataset/')
   system(paste0('mkdir ',dictDataFolder))
   lapply(seq_along(DictDatasetList), function(i) write_tsv(DictDatasetList[[i]], 'NAME' ))
   
   
   print(date())
   return(DictDataDFsList)
}

