# argument
# SourceDFList <- VelezDFList
# dictDataDir <- velezDataDir

# library
ConvertToList <- function(SourceDFList, sourceDir, resource.LexicogList, resource.LexiconList){

   #source("./R/DataFrame2List.R")
   DataFrame2List <- function(SourceDF){
      List <- split(SourceDF[,c(2:length(SourceDF))], seq(nrow(SourceDF[,c(2:length(SourceDF))])))
      names(List) <- SourceDF[,1]
      for(i in seq_along(List)){
         List[[i]][which(List[[i]] == '')] <- NULL
      }
      return(List)
   }
   
   # CREATE LISTS FOR EACH DATA FRAME ( function version = simpler)
   LexicogEntriesList <- DataFrame2List(SourceDFList$LexicogEntriesDF)
   LexicogComponentsList <- DataFrame2List(SourceDFList$LexicogComponentsDF)
   LexicalEntriesList <- DataFrame2List(SourceDFList$LexicalEntriesDF)
   LexicalFormsList <- DataFrame2List(SourceDFList$LexicalFormsDF)
   LexicalSensesList <- DataFrame2List(SourceDFList$LexicalSensesDF)
   UsageExamplesList <- DataFrame2List(SourceDFList$UsageExamplesDF)
   
   # UNIFY IN ONE BIG LIST AND SAVE IT INTO RDS AND JSON FILES
   CompleteList <- list(
      LEXICOGRAPHIC_RESOURCE=list(lilaResource.Lexicog=resource.LexicogList), 
      LEXICOGRAPHIC_ENTRIES=LexicogEntriesList, 
      LEXICOGRAPHIC_COMPONENTS=LexicogComponentsList, 
      LEXICON=list(lilaResource.Lexicon=resource.LexiconList), 
      LEXICAL_ENTRIES=LexicalEntriesList, 
      LEXICAL_FORMS=LexicalFormsList, 
      LEXICAL_SENSES=LexicalSensesList, 
      LEXICOGRAPHIC_EXAMPLES=UsageExamplesList)
   #saveRDS(CompleteList, paste0(dictDataDir,gsub("([A-z]*).*", "\\1", dictPrefix),"DataList.rds"))
   
   return(CompleteList)
}
