# argument
# SourceDF = any DataFrame up to 12 columns

## SIMPLE VERSION
DataFrame2List <- function(SourceDF){
   List <- split(SourceDF[,c(2:length(SourceDF))], seq(nrow(SourceDF[,c(2:length(SourceDF))])))
   names(List) <- SourceDF[,1]
   for(i in seq_along(List)){
      List[[i]][which(List[[i]] == '')] <- NULL
   }
   return(List)
}


