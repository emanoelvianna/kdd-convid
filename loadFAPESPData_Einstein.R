# https://stackoverflow.com/questions/23324872/rstudio-not-picking-the-encoding-im-telling-it-to-use-when-reading-a-file
# Sys.setlocale(category = "LC_ALL", locale = "pt_PT.UTF-8")
# Sys.setlocale("LC_ALL", "pt_BR.ISO8859-1")
#NOTES:
#- padronizar o texto em resultados: colocando tudo para minusculo, por exemplo.
#[1] "DETECTADO"                "DETECTADO (POSITIVO)"     "Inconclusivo"            
# [4] "INCONCLUSIVO"             "Inconclusivo "            "INCONCLUSIVO "           
# [7] "NA"                       "N√ÉO DETECTADO"            "N√ÉO DETECTADO (NEGATIVO)"

setwd("C:/Users/renan/Desktop/UFRGS/BaseDeDados/Trabalho P·tico")

# load packages
library(reshape2)
library(tidyr)
library(dplyr)


##### -- utils -- ######
removeDupWords <- function(x){
   x <- tolower(x)
   paste(unique(trimws(unlist(strsplit(x,split=" ",fixed=F,perl=T)))),collapse = 
            " ")
}


#### -- load einstein data -- ####

einstein.pacientes <- read.csv("C:/Users/renan/Desktop/UFRGS/BaseDeDados/Trabalho P·tico/EINSTEIN_Exames_2.csv", sep="|", stringsAsFactors=FALSE,header=TRUE,encoding="UTF-8")
einstein.exames <- read.csv("C:/Users/renan/Desktop/UFRGS/BaseDeDados/Trabalho P·tico/EINSTEIN_Exames_2", sep="\t", stringsAsFactors=FALSE,header=TRUE,encoding="UTF-8")

colnames(einstein.pacientes) <- toupper(colnames(einstein.pacientes))
colnames(einstein.exames) <- toupper(colnames(einstein.exames))

#### -- Pre-process PATIENTS data ---####
df.einstein.pacientes <- einstein.pacientes %>%
   mutate(IC_SEXO = tolower(IC_SEXO)) %>%
   mutate(CD_PAIS = tolower(CD_PAIS)) %>%
   mutate(CD_UF = tolower(CD_UF)) %>%
   mutate(CD_MUNICIPIO = tolower(stringi::stri_trans_general(CD_MUNICIPIO,"Latin-ASCII"))) %>% 
   mutate(CD_CEP = tolower(CD_CEP)) 


#### -- Pre-process EXAMES data ---####
## transform data, such that each exam (variable) becomes a column.
## unique entries are given by id patient + date, since a patient may have
## the same exam collected in multiple dates
# df.einstein.exames <- separate(einstein.exames,"DE_EXAME", into = c("DE_EXAME_A", "DE_EXAME_B"), sep = ",") 
df.einstein.exames <- einstein.exames %>%
   # mutate(DE_EXAME_A = gsub("-", " ", DE_EXAME_A)) %>%
   mutate(DE_ANALITO = gsub("-", " ", DE_ANALITO)) %>%
   mutate(DE_ANALITO = gsub(",", "", DE_ANALITO)) %>%
   mutate(DE_ANALITO = gsub(">", "", DE_ANALITO)) %>%
   mutate(DE_ANALITO = gsub(":", "", DE_ANALITO)) %>%
   mutate(DE_RESULTADO =  tolower(DE_RESULTADO)) %>%
   mutate(DE_RESULTADO=stringi::stri_trans_general(DE_RESULTADO,"Latin-ASCII"))


df.einstein.exames <- data.frame(df.einstein.exames %>% unite("DE_EXAME_ANALITO", c("DE_EXAME","DE_ANALITO"), sep=" ", remove = FALSE,na.rm = TRUE))
df.einstein.exames$DE_EXAME_ANALITO <- df.einstein.exames$DE_EXAME_ANALITO %>% 
   stringi::stri_trans_general("Latin-ASCII") %>%
   gsub(",","",.) %>%
   gsub(" - "," ",.) %>%
   tolower() %>%
   gsub("covid 19","covid19",.) %>%
   sapply(., FUN = function(x) removeDupWords(x)) %>%
   gsub(" ","_",.)
   gsub("__","_",.)

   
##until now, I rearrange only the value, and create a new df with ID_PACIENTE, variable_name, CD_UNIDADE and DE_VALOR_REFERENCIA
df.einstein.exames.notes <- df.einstein.exames[,c("ID_PACIENTE","DT_COLETA","DE_EXAME_ANALITO","CD_UNIDADE","DE_VALOR_REFERENCIA")]

df.einstein.exames<-dcast(df.einstein.exames,ID_PACIENTE+DT_COLETA~DE_EXAME_ANALITO,value.var = c("DE_RESULTADO"),fun.aggregate = function(x) paste(x[1]))

df.einstein <- merge(df.einstein.pacientes, df.einstein.exames, by=c("ID_PACIENTE"), all.x = TRUE, all.y = TRUE)
colnames(df.einstein) <- tolower(colnames(df.einstein))
write.table(df.einstein,"einstein_merged.csv",sep="\t",col.names=TRUE,row.names = FALSE,quote=FALSE)
# save(list=c("einstein.exames","einstein.pacientes"),file="Grupo_einstein_Dataset_Covid19_Complete_RAW.RData")
save(list=c("df.einstein","df.einstein.exames.notes","df.einstein.pacientes"),file="Grupo_einstein_Dataset_Covid19_Complete_PREPROC.RData")



# statistics
# how many patients?
length(unique(df.einstein$ID_PACIENTE))
# how many DT_COLETA per patient?
x<-df.einstein %>% group_by(ID_PACIENTE) %>%
   summarize(n=n())
table(x$n)


