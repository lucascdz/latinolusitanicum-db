# argument
# SourceDFList <- VelezDFList
# dictDataDir <- velezDataDir

# library
ConvertToList <- function(SourceDFList, dictDataDir, resource.LexicogList, resource.LexiconList){
   print(date())
   source("./R/DataFrame2List.R")

   # CREATE LISTS FOR EACH DATA FRAME ( function version = simpler)
   EntriesList <- DataFrame2List(SourceDFList$EntriesDF)
   ComponentsList <- DataFrame2List(SourceDFList$LexComponentsDF)
   LexicalEntriesList <- DataFrame2List(SourceDFList$LexicalEntriesDF)
   FormsList <- DataFrame2List(SourceDFList$FormsDF)
   LexicalSensesList <- DataFrame2List(SourceDFList$LexicalSensesDF)
   UsageExamplesList <- DataFrame2List(SourceDFList$UsageExamplesDF)

   # UNIFY IN ONE BIG LIST AND SAVE IT INTO RDS AND JSON FILES
   CompleteList <- list(LEXICOGRAPHIC_RESOURCE=list(resource.Lexicog=resource.LexicogList), LEXICOGRAPHIC_ENTRIES=EntriesList, LEXICOGRAPHIC_COMPONENTS=ComponentsList, LEXICON=list(resource.Lexicon=resource.LexiconList), LEXICAL_ENTRIES=LexicalEntriesList, LEXICAL_FORMS=FormsList, LEXICAL_SENSES=LexicalSensesList, LEXICOGRAPHIC_EXAMPLES=UsageExamplesList)
   saveRDS(CompleteList, paste0(dictDataDir,gsub("([A-z]*).*", "\\1", dictPrefix),"DataList.rds"))

   print(date())
   return(CompleteList)
}
