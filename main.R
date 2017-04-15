rm(list = ls())
setwd("~/Data\ Mining/Data\ Stream/Classification/Fuzzy-OnDemand/")
library(stream)
library(class)
library(ggplot2)
library (caret)

MAX_TEST <- 1         #Quantidade max de TESTES do algoritmo
RESULTS_HISTORY <- c()
SUM_RESULTS_HISTORY <- c()


#------------------------------------------------------Variáveis do Dataset--------------------------------------------------------------------------------------
#Separa o Conjunto de teste e o de treino 1 por 1
TRAINING_DATASET = read.csv("DS_Datasets/Synthetic/Stationary/BG_10k/BarsGaussAN0_10000.csv")[c(TRUE, FALSE), ]
TEST_DATASET = read.csv("DS_Datasets/Synthetic/Stationary/BG_10k/BarsGaussAN0_10000.csv")[c(FALSE, TRUE), ]



TRAINING_SET_SIZE <-nrow(TRAINING_DATASET)    #Quantidade de instacias no dataset de treino
TEST_SET_SIZE <-nrow(TEST_DATASET)            #Quantidade de instancia no dataset de teste


NATTRIBUTES <- ncol(TRAINING_DATASET) -1      #Calcula quantidade de atributos presentes no dataset
class_levels <- 1:max(as.numeric(names(table(as.factor(TEST_DATASET[,NATTRIBUTES+1])))))

for(teste in 1:MAX_TEST){
  
  #----------------------------------------------------------------------------------------------------------------------------------------------------------------  
  
  source("Fuzzy-Ondemand.R")
  RESULTS_HISTORY <- cbind(RESULTS_HISTORY,list(teste = HISTORY))
  SUM_RESULTS_HISTORY <- cbind(SUM_RESULTS_HISTORY,list(teste = SUM_HISTORY))
}


#fmic_centers <- as.data.frame(get.centers(MICROCLUSTERS))
#fmic_classes <- as.data.frame(t(get.class(MICROCLUSTERS)))
#colnames(fmic_classes) <- "class_center"
#fmic <- cbind(fmic_centers,fmic_classes)
#mics <- +title+geom_point(data = fmic, aes(x=V1,y=V2),shape=fmic$class_center,color="black")
#plot_name <-  "Graphx/Teste.pdf"

pdf("teste.pdf")

TRAINING_INDEX <- INITNUMBER
for(chunk in 1:length(HISTORY)){
  title_name <- paste0("Buffer")
  title <- ggtitle(title_name)
  end_buffer <- TRAINING_INDEX + BUFFER_SIZE + KFIT
  print(ggplot(TEST_DATASET[TRAINING_INDEX:end_buffer,], aes(x=x,y=y))+geom_point(aes(color = factor(x = class, levels = c(1,2,3,4) ),shape=factor(x = class, levels = c(1,2,3,4) ))))

  cat("INIT: ",TRAINING_INDEX,"it: ",end_buffer,"\n")
  TRAINING_INDEX <- end_buffer
}
dev.off()

# pdf("chunk.pdf")
# 
# TRAINING_INDEX <- INITNUMBER
# for(chunk in 1:length(HISTORY)){
#   title_name <- paste0("Buffer")
#   title <- ggtitle(title_name)
#   end_buffer <- TRAINING_INDEX + BUFFER_SIZE + KFIT
#   if(chunk == 6){
#     end_buffer_it <- TRAINING_INDEX
#     while(end_buffer_it <= end_buffer){
#       TRAINING_INDEX <- end_buffer_it
#       end_buffer_it <- end_buffer_it + points_until_store
#       print(ggplot(TEST_DATASET[TRAINING_INDEX:end_buffer_it,], aes(x=x,y=y))+geom_point(aes(color = factor(x = class, levels = c(1,2,3,4) ),shape=factor(x = class, levels = c(1,2,3,4) ))))
#       cat(TRAINING_INDEX,"-",end_buffer_it,"\n")
#       }
#   }
#   cat("INIT: ",TRAINING_INDEX,"it: ",end_buffer,"\n")
#   TRAINING_INDEX <- end_buffer
# }
# dev.off()


PARAMETERS = c(EXEC = MAX_TEST, DATASET = "BG_10k",INITNUMBER = INITNUMBER, MICROCLUSTER_RATIO = MICROCLUSTER_RATIO, FRAME_MAX_CAPACITY = FRAME_MAX_CAPACITY, BUFFER_SIZE = BUFFER_SIZE, KFIT = KFIT, M = M, POINTS_PER_UNIT_TIME = POINTS_PER_UNIT_TIME, PHI = PHI, P = P, STORE_MC = STORE_MC, FUZZY_M = FUZZY_M, FUZZY_THETA = FUZZY_THETA )
source("results_evaluate.R")
#Avaliar resultados
EVALUATED_RESULTS <- results.evaluate(SUM_RESULTS_HISTORY)
#Salvar resultados
saveRDS(PARAMETERS, "Results.Data/parameters.rds")
saveRDS(EVALUATED_RESULTS,  "Results.Data/results.rds")



cat("Fuzzy-OnDemand")
