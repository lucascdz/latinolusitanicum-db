# ADD MARKERS TO ##literal.strings##

# argument
# SourceDFList <- VelezDFList

AddMarkers <- function(SourceDFList){

   SourceDFList$LexicogEntriesDF[SourceDFList$LexicogEntriesDF == ""] <- NA
   SourceDFList$LexicogEntriesDF$rdfs.label <- gsub("(.*)", "##\\1##", SourceDFList$LexicogEntriesDF$rdfs.label)
   SourceDFList$LexicogEntriesDF$skos.note <- gsub("(.*)", "##\\1##", SourceDFList$LexicogEntriesDF$skos.note)
   SourceDFList$LexicogEntriesDF[is.na(SourceDFList$LexicogEntriesDF)] <- ""

   SourceDFList$LexicogComponentsDF[SourceDFList$LexicogComponentsDF == ""] <- NA
   SourceDFList$LexicogComponentsDF$rdfs.label <- gsub("(.*)", "##\\1##", SourceDFList$LexicogComponentsDF$rdfs.label)
   SourceDFList$LexicogComponentsDF$skos.note <- gsub("(.*)", "##\\1##", SourceDFList$LexicogComponentsDF$skos.note)
   SourceDFList$LexicogComponentsDF[is.na(SourceDFList$LexicogComponentsDF)] <- ""

   SourceDFList$LexicalEntriesDF[SourceDFList$LexicalEntriesDF == ""] <- NA
   SourceDFList$LexicalEntriesDF$rdfs.label <- gsub("(.*)", "##\\1##", SourceDFList$LexicalEntriesDF$rdfs.label)
   SourceDFList$LexicalEntriesDF$lexinfo.note <- gsub("(.*)", "##\\1##", SourceDFList$LexicalEntriesDF$lexinfo.note)
   SourceDFList$LexicalEntriesDF$skos.note <- gsub("(.*)", "##\\1##", SourceDFList$LexicalEntriesDF$skos.note)
   SourceDFList$LexicalEntriesDF[is.na(SourceDFList$LexicalEntriesDF)] <- ""

   SourceDFList$LexicalFormsDF[SourceDFList$LexicalFormsDF == ""] <- NA
   SourceDFList$LexicalFormsDF$rdfs.label <- gsub("(.*)", "##\\1##", SourceDFList$LexicalFormsDF$rdfs.label)
   SourceDFList$LexicalFormsDF$skos.note <- gsub("(.*)", "##\\1##", SourceDFList$LexicalFormsDF$skos.note)
   SourceDFList$LexicalFormsDF$ontolex.writtenRep <- gsub("(.*)", "##\\1##", SourceDFList$LexicalFormsDF$ontolex.writtenRep)
   SourceDFList$LexicalFormsDF$lexinfo.note <- gsub("(.*)", "##\\1##", SourceDFList$LexicalFormsDF$lexinfo.note)
   SourceDFList$LexicalFormsDF[is.na(SourceDFList$LexicalFormsDF)] <- ""

   SourceDFList$LexicalSensesDF[SourceDFList$LexicalSensesDF == ""] <- NA
   SourceDFList$LexicalSensesDF$skos.note <- gsub("(.*)", "##\\1##", SourceDFList$LexicalSensesDF$skos.note)
   if('skos.definition.la' %in% colnames(SourceDFList$LexicalSensesDF)){
      SourceDFList$LexicalSensesDF$skos.definition.la <- gsub("(.*)", "##\\1## @la", SourceDFList$LexicalSensesDF$skos.definition.la)
   }
   SourceDFList$LexicalSensesDF$skos.definition.pt <- gsub("(.*)", "##\\1## @pt", SourceDFList$LexicalSensesDF$skos.definition.pt)
   SourceDFList$LexicalSensesDF$lexinfo.note <- gsub("(.*)", "##\\1##", SourceDFList$LexicalSensesDF$lexinfo.note)
   if('rdfs.label.la' %in% colnames(SourceDFList$LexicalSensesDF)){
      SourceDFList$LexicalSensesDF$rdfs.label.la <- gsub("(.*)", "##\\1## @la", SourceDFList$LexicalSensesDF$rdfs.label.la)
   }
   SourceDFList$LexicalSensesDF$rdfs.label.pt <- gsub("(.*)", "##\\1## @pt", SourceDFList$LexicalSensesDF$rdfs.label.pt)
   SourceDFList$LexicalSensesDF$rdfs.label.en <- gsub("(.*)", "##\\1## @en", SourceDFList$LexicalSensesDF$rdfs.label.en)
   SourceDFList$LexicalSensesDF$rdfs.comment <- gsub("(.*)", "##\\1##", SourceDFList$LexicalSensesDF$rdfs.comment)
   SourceDFList$LexicalSensesDF[is.na(SourceDFList$LexicalSensesDF)] <- ""

   SourceDFList$UsageExamplesDF[SourceDFList$UsageExamplesDF == ""] <- NA
   SourceDFList$UsageExamplesDF$skos.note <- gsub("(.*)", "##\\1##", SourceDFList$UsageExamplesDF$skos.note)
   SourceDFList$UsageExamplesDF$rdfs.value.la <- gsub("(.*)", "##\\1## @la", SourceDFList$UsageExamplesDF$rdfs.value.la)
   SourceDFList$UsageExamplesDF$lexinfo.note <- gsub("(.*)", "##\\1##", SourceDFList$UsageExamplesDF$lexinfo.note)
   SourceDFList$UsageExamplesDF$rdfs.value.pt <- gsub("(.*)", "##\\1## @pt", SourceDFList$UsageExamplesDF$rdfs.value.pt)
   SourceDFList$UsageExamplesDF$rdfs.label <- gsub("(.*)", "##\\1##", SourceDFList$UsageExamplesDF$rdfs.label)
   SourceDFList$UsageExamplesDF[is.na(SourceDFList$UsageExamplesDF)] <- ""

   return(SourceDFList)
}
