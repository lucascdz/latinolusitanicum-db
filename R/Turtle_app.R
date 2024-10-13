sourceDir <- "./sources/Velez/Velez.1744.indextotiusartis.1.4.1/"
sourcePrefix <- "Velez1744\\."
targetPub <- "LiLa"
targetFolder <- '../Latin-Portuguese-dictionaries/'
system(paste0('mkdir ',targetFolder))

source('./R/Turtle_pipe.R')

## Velez total time = 30 sec.
## Fonseca total time = 13 min.

######
# CONSIDER EXCLUDING ENTRIES WITHOUT LEXICAL SENSE (but preserve those mentioned in 'decomp')
#############

