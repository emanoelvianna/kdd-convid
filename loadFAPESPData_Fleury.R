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


#### -- load FLEURY data -- ####

fleury.pacientes <- read.csv("/Users/renan/Desktop/UFRGS/BaseDeDados/Trabalho P·tico/GrupoFleury_Pacientes_3.csv", sep="|", stringsAsFactors=FALSE,header=TRUE,encoding="UTF-8")
fleury.exames <- read.csv("/Users/renan/Desktop/UFRGS/BaseDeDados/Trabalho P·tico/GrupoFleury_Exames_3.csv", sep="|", stringsAsFactors=FALSE,header=TRUE,encoding="UTF-8")

#### -- Pre-process PATIENTS data ---####
df.fleury.pacientes <- fleury.pacientes %>%
   mutate(IC_SEXO = tolower(IC_SEXO)) %>%
   mutate(CD_PAIS = tolower(CD_PAIS)) %>%
   mutate(CD_UF = tolower(CD_UF)) %>%
   mutate(CD_MUNICIPIO = tolower(stringi::stri_trans_general(CD_MUNICIPIO,"Latin-ASCII"))) %>% 
   mutate(CD_CEPREDUZIDO = tolower(CD_CEPREDUZIDO)) 


#### -- Pre-process EXAMES data ---####
## transform data, such that each exam (variable) becomes a column.
## unique entries are given by id patient + date, since a patient may have
## the same exam collected in multiple dates
df.fleury.exames <- separate(fleury.exames,"DE_EXAME", into = c("DE_EXAME_A", "DE_EXAME_B"), sep = ",") 
df.fleury.exames <- df.fleury.exames %>%
   mutate(DE_EXAME_A = gsub("-", " ", DE_EXAME_A)) %>%
   mutate(DE_ANALITO = gsub("-", " ", DE_ANALITO)) %>%
   mutate(DE_ANALITO = gsub(",", "", DE_ANALITO)) %>%
   mutate(DE_RESULTADO =  tolower(DE_RESULTADO)) %>%
   mutate(DE_RESULTADO=stringi::stri_trans_general(DE_RESULTADO,"Latin-ASCII"))
   
   
df.fleury.exames <- data.frame(df.fleury.exames %>% unite("DE_EXAME_ANALITO", c("DE_EXAME_A","DE_ANALITO"), sep=" ", remove = FALSE,na.rm = TRUE))
df.fleury.exames$DE_EXAME_ANALITO <- df.fleury.exames$DE_EXAME_ANALITO %>% 
   stringi::stri_trans_general("Latin-ASCII") %>% 
   gsub(",","",.) %>% 
   gsub(" - "," ",.) %>% 
   tolower() %>%
   gsub("covid 19","covid19",.) %>%
   sapply(., FUN = function(x) removeDupWords(x)) %>%
   gsub(" ","_",.) %>%
   gsub("__","_",.)

##until now, I rearrange only the value, and create a new df with ID_PACIENTE, variable_name, CD_UNIDADE and DE_VALOR_REFERENCIA
df.fleury.exames.notes <- df.fleury.exames[,c("ID_PACIENTE","DT_COLETA","DE_EXAME_ANALITO","CD_UNIDADE","DE_VALOR_REFERENCIA")]

df.fleury.exames<-dcast(df.fleury.exames,ID_PACIENTE+DT_COLETA~DE_EXAME_ANALITO,value.var = c("DE_RESULTADO"),fun.aggregate = function(x) paste(x[1]))

df.fleury <- merge(df.fleury.pacientes, df.fleury.exames, by=c("ID_PACIENTE"), all.x = TRUE, all.y = TRUE)

# save(list=c("fleury.exames","fleury.pacientes"),file="Grupo_Fleury_Dataset_Covid19_Complete_RAW.RData")
save(list=c("df.fleury","df.fleury.exames.notes","df.fleury.pacientes"),file="Grupo_Fleury_Dataset_Covid19_Complete_PREPROC.RData")



# statistics
# how many patients?
length(unique(df.fleury$ID_PACIENTE))
# how many DT_COLETA per patient?
x<-df.fleury %>% group_by(ID_PACIENTE) %>%
   summarize(n=n())
table(x$n)

table(df.fleury$`novo_coronavirus_2019_(sars_cov_2)_covid19_deteccao_por_pcr`)

