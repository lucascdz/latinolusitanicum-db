# ADD ##string## AND <url> MARKERS TO

# argument
# SourceDFList <- VelezDFList

AddMarkersToValues <- function(SourceDFList){

   SourceDFList$EntriesDF[SourceDFList$EntriesDF == ""] <- NA
   SourceDFList$EntriesDF$rdfs.label <- gsub("(.*)", "##\\1##", SourceDFList$EntriesDF$rdfs.label)
   SourceDFList$EntriesDF$rdfs.comment <- gsub("(.*)", "##\\1##", SourceDFList$EntriesDF$rdfs.comment)
   SourceDFList$EntriesDF[is.na(SourceDFList$EntriesDF)] <- ""

   SourceDFList$ComponentsDF[SourceDFList$ComponentsDF == ""] <- NA
   SourceDFList$ComponentsDF$rdfs.label <- gsub("(.*)", "##\\1##", SourceDFList$ComponentsDF$rdfs.label)
   SourceDFList$ComponentsDF[is.na(SourceDFList$ComponentsDF)] <- ""

   SourceDFList$LexicalEntriesDF[SourceDFList$LexicalEntriesDF == ""] <- NA
   SourceDFList$LexicalEntriesDF$dcterms.identifier <- gsub("(.*)", "<\\1>", SourceDFList$LexicalEntriesDF$dcterms.identifier)
   SourceDFList$LexicalEntriesDF$dcterms.relation <- gsub("(.*)", "<\\1>", SourceDFList$LexicalEntriesDF$dcterms.relation)
   SourceDFList$LexicalEntriesDF$rdfs.label <- gsub("(.*)", "##\\1##", SourceDFList$LexicalEntriesDF$rdfs.label)
   SourceDFList$LexicalEntriesDF$rdfs.comment <- gsub("(.*)", "##\\1##", SourceDFList$LexicalEntriesDF$rdfs.comment)
   SourceDFList$LexicalEntriesDF$lexinfo.note <- gsub("(.*)", "##\\1##", SourceDFList$LexicalEntriesDF$lexinfo.note)
   SourceDFList$LexicalEntriesDF$skos.note <- gsub("(.*)", "##\\1##", SourceDFList$LexicalEntriesDF$skos.note)
   SourceDFList$LexicalEntriesDF[is.na(SourceDFList$LexicalEntriesDF)] <- ""

   SourceDFList$FormsDF[SourceDFList$FormsDF == ""] <- NA
   SourceDFList$FormsDF$rdfs.comment <- gsub("(.*)", "##\\1##", SourceDFList$FormsDF$rdfs.comment)
   SourceDFList$FormsDF$skos.note <- gsub("(.*)", "##\\1##", SourceDFList$FormsDF$skos.note)
   SourceDFList$FormsDF$ontolex.writtenRep <- gsub("(.*)", "##\\1##", SourceDFList$FormsDF$ontolex.writtenRep)
   SourceDFList$FormsDF$lexinfo.note <- gsub("(.*)", "##\\1##", SourceDFList$FormsDF$lexinfo.note)
   SourceDFList$FormsDF[is.na(SourceDFList$FormsDF)] <- ""

   SourceDFList$LexicalSensesDF[SourceDFList$LexicalSensesDF == ""] <- NA
   SourceDFList$LexicalSensesDF$rdfs.label <- gsub("(.*)", "##\\1##", SourceDFList$LexicalSensesDF$rdfs.label)
   SourceDFList$LexicalSensesDF$skos.note <- gsub("(.*)", "##\\1##", SourceDFList$LexicalSensesDF$skos.note)
   SourceDFList$LexicalSensesDF$skos.definition.la <- gsub("(.*)", "##\\1## @la", SourceDFList$LexicalSensesDF$skos.definition.la)
   SourceDFList$LexicalSensesDF$skos.definition.pt <- gsub("(.*)", "##\\1## @pt", SourceDFList$LexicalSensesDF$skos.definition.pt)
   SourceDFList$LexicalSensesDF$lexinfo.note <- gsub("(.*)", "##\\1##", SourceDFList$LexicalSensesDF$lexinfo.note)
   SourceDFList$LexicalSensesDF[is.na(SourceDFList$LexicalSensesDF)] <- ""

   SourceDFList$LexicalConceptsDF[SourceDFList$LexicalConceptsDF == ""] <- NA
   SourceDFList$LexicalConceptsDF$rdfs.label <- gsub("(.*)", "##\\1##", SourceDFList$LexicalConceptsDF$rdfs.label)
   SourceDFList$LexicalConceptsDF[is.na(SourceDFList$LexicalConceptsDF)] <- ""

   SourceDFList$UsageExamplesDF[SourceDFList$UsageExamplesDF == ""] <- NA
   SourceDFList$UsageExamplesDF$rdfs.comment <- gsub("(.*)", "##\\1##", SourceDFList$UsageExamplesDF$rdfs.comment)
   SourceDFList$UsageExamplesDF$skos.note <- gsub("(.*)", "##\\1##", SourceDFList$UsageExamplesDF$skos.note)
   SourceDFList$UsageExamplesDF$rdfs.value.la <- gsub("(.*)", "##\\1## @la", SourceDFList$UsageExamplesDF$rdfs.value.la)
   SourceDFList$UsageExamplesDF$lexinfo.note <- gsub("(.*)", "##\\1##", SourceDFList$UsageExamplesDF$lexinfo.note)
   SourceDFList$UsageExamplesDF$rdfs.value.pt <- gsub("(.*)", "##\\1## @pt", SourceDFList$UsageExamplesDF$rdfs.value.pt)
   SourceDFList$UsageExamplesDF$lexinfo.note.1 <- gsub("(.*)", "##\\1##", SourceDFList$UsageExamplesDF$lexinfo.note.1)
   SourceDFList$UsageExamplesDF[is.na(SourceDFList$UsageExamplesDF)] <- ""

   return(SourceDFList)

}

