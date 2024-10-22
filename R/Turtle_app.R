# APP for building Turtle RDF files from Dictionary source datasets

#Dict <- 'Velez'
#Dict <- 'Fonseca'
Dict <- 'Cardoso'

if(Dict=='Velez'){
   sourceDir <- "../latinolusitanicum-sources/Velez/Velez.1744.indextotiusartis.1.4.2/"
   sourcePrefix <- "Velez1744\\."
}
if(Dict=='Fonseca'){
   sourceDir <- "../latinolusitanicum-sources/Fonseca/Fonseca.1798.parvumlexicum-0.4.5/"
   sourcePrefix <- "Fonseca1798\\."
}
if(Dict=='Cardoso'){
   sourceDir <- "../latinolusitanicum-sources/Cardoso/Cardoso.1570.latinolusitanicum-0.2.8/"
   sourcePrefix <- "Cardoso1570\\."
}
targetPub <- "LiLa"
targetFolder <- '../Latin-Portuguese-dictionaries/'
system(paste0('mkdir ',targetFolder))
source('./R/Turtle_pipeline.R')

## Velez total time = 30 sec.
## Fonseca total time = 07 min.
## Cardoso total time = 04 min.

## REMEMBER VALIDATE TURTLE FILES THROUGH COMMAND LINE 'ttl [file]'

######
# CONSIDER EXCLUDING ENTRIES WITHOUT LEXICAL SENSE (but preserve those mentioned in 'decomp')
#############

