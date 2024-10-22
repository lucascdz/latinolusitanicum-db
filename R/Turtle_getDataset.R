library(tidyverse)
library(jsonlite)

# ARGUMENTS
#sourceDir <- "./sources/Velez/Velez.1744.indextotiusartis.1.4.0/"
#sourcePrefix <- "Velez1744\\."
#targetPub <- "LiLa"

GetDataset <- function(sourceDir,sourcePrefix,targetPub,targetFolder){
   
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
   LexicogDF <- LexicogDF[!str_detect(LexicogDF$lexicog.describes,'[a-z]$'),]
   
   # CREATE 'lexicog:Entries' DATA FRAME
   LexicogEntriesDF <- LexicogDF[str_detect(LexicogDF$lexicog.describes,'e01$'),]
   LexicogEntriesDF$lexicog.Entry <- gsub('.e01$','',LexicogEntriesDF$lexicog.describes)
   # get form labels
   entryLabelDF <- FormsDF[str_detect(FormsDF$ontolex.Form,'e01\\.f01'),colnames(FormsDF) %in% c('ontolex.Form',"ontolex.writtenRep")]
   entryLabelDF$lexicog.Entry <- gsub('\\.e\\d+.*','',entryLabelDF$ontolex.Form)
   entryLabelDF$rdfs.label <- unlist(lapply(seq_along(entryLabelDF$lexicog.Entry), function(i) paste0(gsub("([A-z]*).*","\\1", sourcePrefix),"’s entry for '", entryLabelDF$ontolex.writtenRep[i],"'")))
   LexicogEntriesDF <- left_join(LexicogEntriesDF,entryLabelDF[,colnames(entryLabelDF) %in% c("lexicog.Entry","rdfs.label")])
   # get subComponents
   subComponentDF <- LexicogDF[!str_detect(LexicalEntriesDF$ontolex.LexicalEntry, 'e01') & !str_detect(LexicalEntriesDF$ontolex.LexicalEntry, 'e\\d\\d[a-z]'),]
   subComponentDF$lexicog.Entry <- gsub('\\.e\\d+.?$','',subComponentDF$lexicog.describes)
   subComponentDF$lexicog.subComponent <- gsub('(.*)','\\1_comp',subComponentDF$lexicog.describes)
   subComponentList <- split(subComponentDF,subComponentDF$lexicog.Entry)
   subComponentDF <- data.frame(
      lexicog.Entry=names(subComponentList),
      lexicog.subComponent=unlist(lapply(seq_along(subComponentList), function(i) str_flatten_comma(subComponentList[[i]]$lexicog.subComponent))),
      stringsAsFactors = F)
   LexicogEntriesDF <- left_join(LexicogEntriesDF,subComponentDF)
   # adjust prefixes
   LexicogEntriesDF$lexicog.Entry <- gsub(sourcePrefix, "lexicogEntry\\:", LexicogEntriesDF$lexicog.Entry)
   LexicogEntriesDF$lexicog.describes <- gsub(sourcePrefix, "lexicalEntry\\:", LexicogEntriesDF$lexicog.describes)
   LexicogEntriesDF$lexicog.subComponent <- gsub(sourcePrefix, "lexicogComponent\\:", LexicogEntriesDF$lexicog.subComponent)
   LexicogEntriesDF[is.na(LexicogEntriesDF)] <- ""
   LexicogEntriesDF <- LexicogEntriesDF[,c(
      which(colnames(LexicogEntriesDF)=="lexicog.Entry"),
      which(colnames(LexicogEntriesDF)=="rdfs.label"),
      which(colnames(LexicogEntriesDF)=="lexinfo.note"),
      which(colnames(LexicogEntriesDF)=="lexicog.describes"),
      which(colnames(LexicogEntriesDF)=="lexicog.subComponent"),
      which(colnames(LexicogEntriesDF)=="skos.note"))]
   LexicogEntriesDF[is.na(LexicogEntriesDF)] <- ''
   rownames(LexicogEntriesDF) <- NULL
   ## LexicogEntriesDF DONE
   
   
   # CREATE 'lexicog:Components' DATA FRAME
   LexicogComponentsDF <- LexicogDF[!str_detect(LexicogDF$lexicog.describes, 'e01') & !str_detect(LexicogDF$lexicog.describes, 'e\\d\\d[a-z]'),]
   LexicogComponentsDF$lexicog.Entry <- gsub('\\.e\\d+.?$','',LexicogComponentsDF$lexicog.describes)
   LexicogComponentsDF$lexicog.LexicographicComponent <- gsub('(.*)','\\1_comp',LexicogComponentsDF$lexicog.describes)
   # add labels
   compLabelDF <- FormsDF[str_detect(FormsDF$ontolex.Form,'\\.f01') & !str_detect(FormsDF$ontolex.Form,'\\.e01\\.'),colnames(FormsDF) %in% c('ontolex.Form',"ontolex.writtenRep")]
   compLabelDF$lexicog.LexicographicComponent <- gsub('^(.*)\\.f\\d+.*','\\1_comp',compLabelDF$ontolex.Form)
   compLabelDF$rdfs.label <- unlist(lapply(seq_along(compLabelDF$ontolex.Form), function(i) paste0(gsub("([A-z]*).*","\\1", sourcePrefix),"’s subentry for '", compLabelDF$ontolex.writtenRep[i],"'")))
   LexicogComponentsDF <- left_join(LexicogComponentsDF,compLabelDF)
   LexicogComponentsDF <- LexicogComponentsDF[,c(
      which(colnames(LexicogComponentsDF)=="lexicog.LexicographicComponent"),
      which(colnames(LexicogComponentsDF)=="rdfs.label"),
      which(colnames(LexicogComponentsDF)=="lexinfo.note"),
      which(colnames(LexicogComponentsDF)=="lexicog.describes"),
      which(colnames(LexicogComponentsDF)=="skos.note"))]
   # update prefixes
   LexicogComponentsDF$lexicog.LexicographicComponent <- gsub(sourcePrefix, "lexicogComponent:", LexicogComponentsDF$lexicog.LexicographicComponent)
   LexicogComponentsDF$lexicog.describes <- gsub(sourcePrefix, "lexicalEntry\\:", LexicogComponentsDF$lexicog.describes)
   LexicogComponentsDF[is.na(LexicogComponentsDF)] <- ''
   rownames(LexicogComponentsDF) <- NULL
   ## Components DONE
   
   
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
   DecompDF[DecompDF==''] <- NA
   DecompDF$decomp.subterm[is.na(DecompDF$decomp.subterm)] <- gsub('(.*)','[ ontolex:canonicalForm \\1 ]',DecompDF$lila_id.subterm[is.na(DecompDF$decomp.subterm)])
   DecompDF <- DecompDF[,c(which(str_detect(colnames(DecompDF),'ontolex|decomp')))]
   DecompList <- split(DecompDF,DecompDF$ontolex.LexicalEntry)
   DecompDF <- data.frame(
      ontolex.LexicalEntry=names(DecompList),
      decomp.subterm=unlist(lapply(seq_along(DecompList), function(i) str_flatten_comma(DecompList[[i]]$decomp.subterm,na.rm = T))),
      stringsAsFactors = F)
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
   # clear columns
   LexicalEntriesDF <- LexicalEntriesDF[,which(str_detect(colnames(LexicalEntriesDF),'subClass|decomp|lexicog|lexinfo|ontolex|rdfs|skos'))]
   LexicalEntriesDF <- LexicalEntriesDF[,-which(colnames(LexicalEntriesDF) %in% c('ontolex.Form','ontolex.writtenRep'))]
   LexicalEntriesDF <- LexicalEntriesDF[,c(1,2,order(colnames(LexicalEntriesDF))[-which(order(colnames(LexicalEntriesDF)) %in% 1:2)])]
   # update prefixes
   LexicalEntriesDF$ontolex.LexicalEntry <- gsub(sourcePrefix, "lexicalEntry\\:", LexicalEntriesDF$ontolex.LexicalEntry)
   LexicalEntriesDF$ontolex.canonicalForm <- gsub(sourcePrefix, "lexicalForm\\:", LexicalEntriesDF$ontolex.canonicalForm)
   LexicalEntriesDF$ontolex.lexicalForm <- gsub(sourcePrefix, "lexicalForm\\:", LexicalEntriesDF$ontolex.lexicalForm)
   LexicalEntriesDF$decomp.subterm <- gsub(sourcePrefix, "lexicalEntry\\:", LexicalEntriesDF$decomp.subterm)
   LexicalEntriesDF$ontolex.sense <- gsub(sourcePrefix, "lexicalSense\\:", LexicalEntriesDF$ontolex.sense)
   if(identical(identical(targetPub,"LiLa"), FALSE)){
      LexicalEntriesDF$ontolex.otherForm <- gsub(sourcePrefix, "lexicalForm\\:", LexicalEntriesDF$ontolex.otherForm)
   }
   LexicalEntriesDF[is.na(LexicalEntriesDF)] <- ''
   rownames(LexicalEntriesDF) <- NULL
   # LexicalEntries DONE
   
   
   # UPGRADE 'ontolex:Forms' DATA FRAME
   FormsDF$ontolex.Form <- gsub(sourcePrefix, "lexicalForm\\:", FormsDF$ontolex.Form)
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
   FormsDF <- FormsDF[,which(str_detect(colnames(FormsDF),'lexinfo|ontolex|rdfs|skos'))]

   
   # UPGRADE 'ontolex:LexicalSenses' DATA FRAME
   LexicalSensesAllDF <- data.frame(
      ontolex.LexicalSense=unique(c(LexicalSensesDF$ontolex.LexicalSense, gsub("\\.u\\d\\d.?$", "", UsageExamplesDF$lexicog.UsageExample))),
      stringsAsFactors = F)
   LexicalSensesAllDF <- left_join(LexicalSensesAllDF,LexicalSensesDF)
   LexicalSensesAllDF <- LexicalSensesAllDF[order(LexicalSensesAllDF$ontolex.LexicalSense),]
   rownames(LexicalSensesAllDF) <- NULL
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
      LexicalSensesAllDF$rdfs.label.la <- unlist(lapply(seq_along(LexicalSensesAllDF$ontolex.LexicalSense), function(i) LexicalSensesAllDF$skos.definition.la[i])) %>%
         gsub(',.*','',.)
   }
   LexicalSensesAllDF$rdfs.label.pt <- unlist(lapply(seq_along(LexicalSensesAllDF$ontolex.LexicalSense), function(i) LexicalSensesAllDF$skos.definition.pt[i])) %>%
      gsub(',.*','',.)
   LexicalSensesAllDF$rdfs.label.en[str_detect(LexicalSensesAllDF$ontolex.LexicalSense, "s99")] <- "Further meanings (see related usage examples)"
   LexicalSensesAllDF$rdfs.comment[str_detect(LexicalSensesAllDF$ontolex.LexicalSense, "s99")] <- "This definitionless sense is an instance of the ontolex:LexicalSense class created to function as a container sense to convey dictionary examples and other phrase-level dictionary elements presented as a separate block by the lexicographer."
   LexicalSensesAllDF[is.na(LexicalSensesAllDF)] <- ""
   # normalize spaces enclosing words
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
   # get usageExample List
   examplesDF <- UsageExamplesDF
   examplesDF$ontolex.LexicalSense <- gsub('\\.u\\d+.?','',examplesDF$lexicog.UsageExample)
   examplesList <- split(examplesDF,examplesDF$ontolex.LexicalSense)
   examplesDF <- data.frame(
      ontolex.LexicalSense=names(examplesList),
      lexicog.usageExample=unlist(lapply(seq_along(examplesList), function(i) str_flatten_comma(examplesList[[i]]$lexicog.UsageExample))),
      stringsAsFactors = F)
   LexicalSensesAllDF <- left_join(LexicalSensesAllDF,examplesDF)
   # update prefixes
   LexicalSensesAllDF$lexicog.usageExample <- gsub(sourcePrefix, "usageExample\\:", LexicalSensesAllDF$lexicog.usageExample)
   LexicalSensesAllDF$ontolex.LexicalSense <- gsub(sourcePrefix, "lexicalSense\\:", LexicalSensesAllDF$ontolex.LexicalSense)
   LexicalSensesAllDF <- LexicalSensesAllDF[,which(str_detect(colnames(LexicalSensesAllDF),'lexicog|lexinfo|ontolex|rdfs|skos'))]
   LexicalSensesAllDF <- LexicalSensesAllDF[,c(1,order(colnames(LexicalSensesAllDF))[-which(order(colnames(LexicalSensesAllDF))==1)])]
   LexicalSensesAllDF[is.na(LexicalSensesAllDF)] <- ''
   
   
   # UPDATE 'lexicog:UsageExamples' DATA FRAME
   # join skos.notes
   if(TRUE %in% str_detect(colnames(UsageExamplesDF),'skos.note_')){
      UsageExamplesDF[UsageExamplesDF==''] <- NA
      UsageExamplesDF$skos.note <- unlist(lapply(seq_along(UsageExamplesDF$lexicog.UsageExample), function(i) str_flatten(UsageExamplesDF[i,str_detect(colnames(UsageExamplesDF),'skos.note')],na.rm = T,collapse = ' ')))
      UsageExamplesDF <- UsageExamplesDF[,-which(str_detect(colnames(UsageExamplesDF),'skos.note_'))]
      UsageExamplesDF[is.na(UsageExamplesDF)] <- ''
   }
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
   # update prefix
   UsageExamplesDF$lexicog.UsageExample <- gsub(sourcePrefix, "usageExample\\:", UsageExamplesDF$lexicog.UsageExample)
   # clear and order columns
   UsageExamplesDF <- UsageExamplesDF[,which(str_detect(colnames(UsageExamplesDF),'lexicog|lexinfo|rdfs|skos'))]
   UsageExamplesDF <- UsageExamplesDF[,c(1,order(colnames(UsageExamplesDF))[-which(order(colnames(UsageExamplesDF))==1)])]
   
   
   # CREATE A LIST OF DATA FRAMES
   DictDatasetList <- list(LexicogEntriesDF=LexicogEntriesDF,LexicogComponentsDF=LexicogComponentsDF,LexicalEntriesDF=LexicalEntriesDF,LexicalFormsDF=FormsDF,LexicalSensesDF=LexicalSensesAllDF,UsageExamplesDF=UsageExamplesDF)
   
   # save dataset
   dictDataFolder <- paste0(targetFolder,gsub('^([A-z]+).*','\\L\\1',sourcePrefix, perl = T),'-data/')
   system(paste0('mkdir ',dictDataFolder))
   lapply(seq_along(DictDatasetList), function(i) write_tsv(DictDatasetList[[i]],paste0(dictDataFolder,gsub('DF','',names(DictDatasetList)[i]),'.tsv')))
   
   return(DictDatasetList)
   
}

