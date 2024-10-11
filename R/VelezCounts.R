library(tidyverse)

lilaDataDF <- readRDS("./data/lila_labels_full.rds")
lilaDataDF <- lilaDataDF[,c(1,3)]
lilaDataDF$lila_id <- gsub("^lemma_", "lemma/", lilaDataDF$lila_id)
lilaDataDF$lila_id <- gsub("^ipolemma_", "hypolemma/", lilaDataDF$lila_id)

dictDataList <- readRDS("/Users/lucascdz/FILES/doutorado/corpus/1608_Velez.1744/Velez.1744.indextotiusartis.1.1.0_tsv/VelezDFList.rds")
dictDataDF <- dictDataList$LexicalEntriesDF
dictDataDF <- dictDataDF[,c(which(colnames(dictDataDF)=="ontolex.LexicalEntry"),which(colnames(dictDataDF)=="ontolex.canonicalForm"))]
dictDataDF$ontolex.canonicalForm <- gsub("^http://lila-erc.eu/data/id/", "", dictDataDF$ontolex.canonicalForm)
colnames(dictDataDF)[2] <- "lila_id"
dictDataDF <- left_join(dictDataDF, lilaDataDF)

dccDataDF <- read.csv("/Users/lucascdz/FILES/doutorado/corpus/2020_DCC_Latin_Core_Vocabulary/DCC_Latin_Core_Vocabulary_LINKED.tsv", sep = "\t")
dccDataDF$ontolex.canonicalForm <- gsub("^http://lila-erc.eu/data/id/", "", dccDataDF$ontolex.canonicalForm)
colnames(dccDataDF)[2] <- "lila_id"
dccDataDF <- left_join(dccDataDF, lilaDataDF)

dccDataTable <- data.frame(table(dccDataDF$upostag), stringsAsFactors = F)

###
# LEXICAL ENTRIES
###

# lexical entries total = 4675
nrow(dictDataDF)

# lexical entries among top 1000 = 647 (covers 65% of top 1000 most common words)
length(dictDataDF$ontolex.LexicalEntry[dictDataDF$lila_id %in% dccDataDF$lila_id])

# VERBAL lexical entries among top 1000 = 247 (covers 78% of verbs pertaining to the top 1000 most common words)
length(dictDataDF$ontolex.LexicalEntry[dictDataDF$lila_id %in% dccDataDF$lila_id & dictDataDF$upostag == "VERB"])

# ADVERBIAL lexical entries among top 1000 = 84 (covers 75% of adverbs pertaining to the top 1000 most common words)
length(dictDataDF$ontolex.LexicalEntry[dictDataDF$lila_id %in% dccDataDF$lila_id & dictDataDF$upostag == "ADV"])

# ADJECTIVE lexical entries among top 1000 = 77 (covers 61% of adjectives pertaining to the top 1000 most common words)
length(dictDataDF$ontolex.LexicalEntry[dictDataDF$lila_id %in% dccDataDF$lila_id & dictDataDF$upostag == "ADJ"])

# NOUN lexical entries among top 1000 = 146 (covers 43% of nouns pertaining to the top 1000 most common words)
length(dictDataDF$ontolex.LexicalEntry[dictDataDF$lila_id %in% dccDataDF$lila_id & dictDataDF$upostag == "NOUN"])

# ADPOSITIONAL lexical entries among top 1000 = 25 (covers 92% of adpositions pertaining to the top 1000 most common words)
length(dictDataDF$ontolex.LexicalEntry[dictDataDF$lila_id %in% dccDataDF$lila_id & dictDataDF$upostag == "ADP"])

# CCONJ lexical entries among top 1000 = 15 (covers 100% of coordinating conjunctions pertaining to the top 1000 most common words)
length(dictDataDF$ontolex.LexicalEntry[dictDataDF$lila_id %in% dccDataDF$lila_id & dictDataDF$upostag == "CCONJ"])

# SCONJ lexical entries among top 1000 = 7 (covers 70% of subordinating conjunctions pertaining to the top 1000 most common words)
length(dictDataDF$ontolex.LexicalEntry[dictDataDF$lila_id %in% dccDataDF$lila_id & dictDataDF$upostag == "SCONJ"])

# PRON lexical entries among top 1000 = 15 (covers 100% of pronouns pertaining to the top 1000 most common words)
length(dictDataDF$ontolex.LexicalEntry[dictDataDF$lila_id %in% dccDataDF$lila_id & dictDataDF$upostag == "PRON"])



###
# LEXICOGRAPHIC ENTRIES BY CHAPTER
###

chapterDataDF <- read.csv("/Users/lucascdz/FILES/doutorado/corpus/1608_Velez.1744/Velez.1744.indextotiusartis.0.1.2-ONLY_PAGE_REF.tsv", sep = "\t")
chapterDataDF <- chapterDataDF[,c(1,18)]
colnames(chapterDataDF) <- c("ontolex.LexicalEntry", "chapter")
chapterDataDF$ontolex.LexicalEntry <- gsub("Velez.1744.e", "velez_le.n", chapterDataDF$ontolex.LexicalEntry) %>%
   gsub("(.*)", "\\1.e01", .)
chapterDataDF <- left_join(chapterDataDF, dictDataDF)

length(chapterDataDF$chapter[chapterDataDF$upostag=="NOUN"])
chapterNOUN <- data.frame(table(chapterDataDF$chapter[chapterDataDF$upostag=="NOUN"]), stringsAsFactors = F)
# result: from 1105 NOUN lexicographic entries:
#    - 423 (38%) points to the "de generibus nominum" chapter
#    - 311 (28%) points to the "anomala sive inaequalia" chapter
#    - 191 (17%) points to the "de nominum declinatione pt2" chapter

length(chapterDataDF$chapter[chapterDataDF$upostag=="VERB"])
chapterVERB <- data.frame(table(chapterDataDF$chapter[chapterDataDF$upostag=="VERB"]), stringsAsFactors = F)
# result: from 1252 VERB lexicographic entries:
#    - 662 (53%) points to the "de constructione" chapter
#    - 461 (37%) points to the "de verborum praeteritis et supinis" chapter

length(chapterDataDF$chapter[chapterDataDF$upostag=="ADJ"])
chapterADJ <- data.frame(table(chapterDataDF$chapter[chapterDataDF$upostag=="ADJ"]), stringsAsFactors = F)
# result: from 536 ADJ lexicographic entries:
#    - 348 (65%) points to the "de constructione" chapter
#    - 44 (8%) points to the "rudimenta de octo partibus orationis" chapter

length(chapterDataDF$chapter[chapterDataDF$upostag=="ADV"])
chapterADV <- data.frame(table(chapterDataDF$chapter[chapterDataDF$upostag=="ADV"]), stringsAsFactors = F)
# result: from 224 ADV lexicographic entries:
#    - 100 (45%) points to the "de constructione" chapter
#    - 51 (23%) points to the "rudimenta de octo partibus orationis" chapter

