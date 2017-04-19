rm(list = ls())
setwd("~/Data\ Stream/Classifiers/Tiago/Fuzzy-OnDemand/")
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



pdf("Fuzzy_Buffers.pdf")

TRAINING_INDEX <- INITNUMBER
for(chunk in 1:length(HISTORY)){
  end_buffer <- TRAINING_INDEX + BUFFER_SIZE + KFIT
  
  mic <- cbind(TRAINING_HISTORY[[chunk]]$training_set,class_mic =TRAINING_HISTORY[[chunk]]$labels)
  test_points <- TRAINING_DATASET[TRAINING_INDEX:end_buffer,]
  test_points$class <- as.factor(test_points$class)
  
  mic_point <- geom_point(data = mic, aes(x = V1 , y = V2),color = mic$class_mic, shape = 10, size=10)
  point <- geom_point(aes(x=x,y=y,color=class,shape=1), color = test_points$class,shape = 20)
  theme <- theme(panel.background = element_rect(fill = "white", colour = "grey50"),plot.title = element_text(hjust = 0.5))
  title_name <- paste("Buffer",chunk)
  title <- ggtitle(title_name)
 
  print(ggplot(test_points)+point+mic_point+title+theme)

  cat("INIT: ",TRAINING_INDEX,"it: ",end_buffer,"\n")
  TRAINING_INDEX <- end_buffer
}
dev.off()



PARAMETERS = c(EXEC = MAX_TEST, DATASET = "Bench2_1k",INITNUMBER = INITNUMBER, MICROCLUSTER_RATIO = MICROCLUSTER_RATIO, FRAME_MAX_CAPACITY = FRAME_MAX_CAPACITY, BUFFER_SIZE = BUFFER_SIZE, KFIT = KFIT, M = M, POINTS_PER_UNIT_TIME = POINTS_PER_UNIT_TIME, PHI = PHI, P = P, STORE_MC = STORE_MC, FUZZY_M = FUZZY_M, FUZZY_THETA = FUZZY_THETA )
source("results_evaluate.R")
#Avaliar resultados
EVALUATED_RESULTS <- results.evaluate(SUM_RESULTS_HISTORY)
dir.create("Results.Data",showWarnings = FALSE)
#Salvar resultados
saveRDS(PARAMETERS, "Results.Data/parameters.rds")
saveRDS(EVALUATED_RESULTS,  "Results.Data/results.rds")
saveRDS(SUM_RESULTS_HISTORY,  "Results.Data/sum_results.rds")




cat("Fuzzy-OnDemand")
