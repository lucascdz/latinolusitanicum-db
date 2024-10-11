# ADD MARKERS TO ##literal.strings##

# argument
# SourceDFList <- VelezDFList

AddMarkers <- function(SourceDFList){

   SourceDFList$EntriesDF[SourceDFList$EntriesDF == ""] <- NA
   SourceDFList$EntriesDF$rdfs.label <- gsub("(.*)", "##\\1##", SourceDFList$EntriesDF$rdfs.label)
   SourceDFList$EntriesDF$skos.note <- gsub("(.*)", "##\\1##", SourceDFList$EntriesDF$skos.note)
   SourceDFList$EntriesDF[is.na(SourceDFList$EntriesDF)] <- ""

   SourceDFList$LexComponentsDF[SourceDFList$LexComponentsDF == ""] <- NA
   SourceDFList$LexComponentsDF$rdfs.label <- gsub("(.*)", "##\\1##", SourceDFList$LexComponentsDF$rdfs.label)
   SourceDFList$LexComponentsDF$skos.note <- gsub("(.*)", "##\\1##", SourceDFList$LexComponentsDF$skos.note)
   SourceDFList$LexComponentsDF[is.na(SourceDFList$LexComponentsDF)] <- ""

   SourceDFList$LexicalEntriesDF[SourceDFList$LexicalEntriesDF == ""] <- NA
   SourceDFList$LexicalEntriesDF$rdfs.label <- gsub("(.*)", "##\\1##", SourceDFList$LexicalEntriesDF$rdfs.label)
   SourceDFList$LexicalEntriesDF$lexinfo.note <- gsub("(.*)", "##\\1##", SourceDFList$LexicalEntriesDF$lexinfo.note)
   SourceDFList$LexicalEntriesDF$skos.note <- gsub("(.*)", "##\\1##", SourceDFList$LexicalEntriesDF$skos.note)
   SourceDFList$LexicalEntriesDF[is.na(SourceDFList$LexicalEntriesDF)] <- ""

   SourceDFList$FormsDF[SourceDFList$FormsDF == ""] <- NA
   SourceDFList$FormsDF$rdfs.label <- gsub("(.*)", "##\\1##", SourceDFList$FormsDF$rdfs.label)
   SourceDFList$FormsDF$skos.note <- gsub("(.*)", "##\\1##", SourceDFList$FormsDF$skos.note)
   SourceDFList$FormsDF$ontolex.writtenRep <- gsub("(.*)", "##\\1##", SourceDFList$FormsDF$ontolex.writtenRep)
   SourceDFList$FormsDF$lexinfo.note <- gsub("(.*)", "##\\1##", SourceDFList$FormsDF$lexinfo.note)
   SourceDFList$FormsDF[is.na(SourceDFList$FormsDF)] <- ""

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
   SourceDFList$UsageExamplesDF$lexinfo.note.1 <- gsub("(.*)", "##\\1##", SourceDFList$UsageExamplesDF$lexinfo.note.1)
   SourceDFList$UsageExamplesDF$rdfs.label <- gsub("(.*)", "##\\1##", SourceDFList$UsageExamplesDF$rdfs.label)
   SourceDFList$UsageExamplesDF[is.na(SourceDFList$UsageExamplesDF)] <- ""

   return(SourceDFList)
}
