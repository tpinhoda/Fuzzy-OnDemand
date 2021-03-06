
  #----------------------------------------------Variáveis de inicializacao do algoritmo-----------------------------------------------------------------------------------------
  INITNUMBER <- EXP["INITNUMBER",1]            #Quantidade inicial de pontos que serão utilizados na criacao dos micro-grupos iniciais  
  MICROCLUSTER_RATIO <- EXP["MICROCLUSTER_RATIO",1]      #Quantidade de micro-grupos máxima por classe na criacao inicial
  FRAME_MAX_CAPACITY <- EXP["FRAME_MAX_CAPACITY",1]      #Quantidade de snapshost por frame
  BUFFER_SIZE <- EXP["BUFFER_SIZE",]          #Quantidade de pontos a ser recebida até para q seja feito o teste no fluxo de teste
  KFIT <- EXP["KFIT",1]                  #Quantidade de pontos que serão testadas
  T <- EXP["T",1]                       #Multiplica pela distancia do micro-grupo mais proximo para definir o limite maximo do micro-grupo inicial
  M <- EXP["M_F",1]                    #Porcentagem de ultimos pontos a chegar no micro-grupo
  POINTS_PER_UNIT_TIME <- EXP["POINTS_PER_UNIT_TIME",1]   #Pontos que chegarao a cada 1 unidade de tempo
  
  PHI <- EXP["PHI",1]                #Limiar para decidir se um mcrogrupo é deletado ou merge
  P <- EXP["P",1]                       #Quantidade de horizontes para a classificacão
  STORE_MC <- EXP["STORE_MC",1]                #Intervalo de tempo para armazenar um snapshot
  FUZZY_M <- EXP["FUZZY_M",1]                  #Parametro de fuzzyficação
  FUZZY_THETA <- EXP["FUZZY_THETA",1]            #Verifica se cria ou nao um novo micro-grupo baseado nesse threshold de pertinencia
  #-------------------------------------------Variáveis globais inicializadas automaticamente-------------------------------------------------------------------
  FRAME_NUMBER = round(log2(TRAINING_SET_SIZE))      #Quantidade de frames que haverá na tabela geométrica
  FRAMES = 0:(FRAME_NUMBER-1)                    #Lista dos números dos frames ordenada de forma crescente (0 - framenumber-1)
  SNAPSHOTS = lapply(FRAMES,function(frame){list(frame_number=frame,frame_slot=c())}) #Estrutura da tabela geométrica
  MC_ID <- 0                                    #Contador de id dos micro-grupos
  TIME <- 0                                     #Contador de tempo em unit * 1000
  HORIZONS <- 2^(1:FRAME_MAX_CAPACITY)*1000              #Horizontes para verificar os kfit pontos
  HORIZONS <- c(c(250,500,1000),HORIZONS)
  HISTORY <- c()  
  SUM_HISTORY <- c()
  SUM_PREDICTION <- c()
  SUM_LABELS <- c()
  TRAINING_HISTORY <- c()
  HISTORY_MICS <- c()
  HISTORY_HORIZON  <- c()
  miss_class <- 0
  MAX_MICS <- n_class*MICROCLUSTER_RATIO
  MIN_MICS <- n_class*2
  '%nin%' <- Negate('%in%')
  #------------------------------------------------------Inicializa funcões--------------------------------------------------------------------------------------------
  source("Utils-Fuzzy.R")
  
  #==============================================================INÍCIO===========================================================================================
  #Fase 1 - Inicializacão - OFFLINE

    cat("Teste:",teste,"\n")
    #Criar fluxo de dados do conjunto de treino e teste
    TRAINING_STREAM <- create.datastream(TRAINING_DATASET)
    TEST_STREAM <- create.datastream(TEST_DATASET)  
    
    #Dividir os INITNUMBER primeiros pontos do fluxo de treino por classe
    inicialization_points <- get_points(TRAINING_STREAM, n = INITNUMBER, class = TRUE) #Obtem os INITNUMBER primeiros pontos do fluxo
    splitted_points <- split.class(inicialization_points)                              #Separa os pontos por classe
    
    #Atualiza o tempo do sistema
    TIME <- (INITNUMBER/POINTS_PER_UNIT_TIME)*1000
    
    #Utilizar o kmeans em cada grupo de classe e retornar k microgrupos para cada classe
    
    splitted_microclusters <- lapply(splitted_points, function(class_set){
      if(nrow(class_set) > 0){
        list(cluster = kmeans( class_set[, 1:NATTRIBUTES], MICROCLUSTER_RATIO, nstart = 5),class=class_set[1,NATTRIBUTES+1])
      }else NA
    })
    complete_cases <- !is.na(splitted_microclusters)
    splitted_microclusters <- splitted_microclusters[complete_cases]
    
    #Sumariza os grupos em micro-grupos
    MICROCLUSTERS <- sapply(splitted_microclusters, function(class_set_mc){
      apply(class_set_mc$cluster$centers,1,function(center){
        create.microcluster(center,class_set_mc$class)
      })                           
    })
    MICROCLUSTERS_SIZE = length(MICROCLUSTERS)
    
    #Calculo do limite de acão maximo de cada micro-grupo inicial
    
    #Não tem isso no FMIC (Definir uma forma soft para tratat problemas de sobreposição de micro-grupos)
    #dist_centers_matrix <- dist.microclusters(MICROCLUSTERS) #calcula a distancia entre todos os micro-grupos inciais
    
    #mc_initial_max_boundary <- T*apply(dist_centers_matrix,1,function(dists_microcluster){
    #                                        c(min( dists_microcluster[dists_microcluster!=min(dists_microcluster)] ))    
    #                                     }) 
    #==============================================================================================================================================
    
  
  #Fase 2 - Manutencão ONLINE
  remaining_points = TRAINING_SET_SIZE - INITNUMBER #pontos restantes no fluxo de treino a serem processados
  displacement = INITNUMBER + KFIT
  it <- 0
  while(remaining_points >= BUFFER_SIZE+KFIT){
      it <- it + 1
      cat("Buffer: ", it ,"\n")
      can_merge <- rep(1,length(MICROCLUSTERS))
      remaining_points_buffer <- BUFFER_SIZE
      points_until_store <- STORE_MC*POINTS_PER_UNIT_TIME
      #Processa BUFFER_SIZE pontos do fluxo de treino por STORE_MC*POINTS_PER_UNIT
      while(remaining_points_buffer > 0){
        
        training_points <- get_points(TRAINING_STREAM, n=points_until_store, class = TRUE)
        points_index <- 0
        #Atualizacao dos micro-gupos
        for(stream_point in get.rows(training_points)){
          points_index <- points_index + 1
          class_stream_point <- training_points[points_index,NATTRIBUTES+1]    #Classe do ponto
          stream_point <- t(stream_point[1:NATTRIBUTES])                     #Ponto sem a classe
      
          #Calcular distancia e a pertinência do ponto para os microgrupos de sua classe e retorna os com pertinencia maior que 0.1
          nearest <- nearest.microcluster(MICROCLUSTERS,stream_point,class_stream_point)
          #Calcula o Micro-grupo com menor relevancia caso precise de merge ou delete
          min_relevant <- find.min.relevant(MICROCLUSTERS)
          class_minrelavant <- find.min.relevant.by.class(MICROCLUSTERS,class_stream_point)
          
          if(is.empty(nearest))
            check.relevance(min_relevant,stream_point,class_stream_point)
          else{
            max_membership <- max(nearest[,2])
            #print(max_membership)
            if(max_membership >= FUZZY_THETA ){
              
              for(nearest_mic in get.rows(nearest))
                add.point(nearest_mic,stream_point)
            }else{
              check.relevance(class_minrelevant,stream_point,class_stream_point)
            }  
          }
         #MERGE
          if(MICROCLUSTERS_SIZE > MIN_MICS){
              mic_index <- 1
              while(mic_index <= MICROCLUSTERS_SIZE){
                if((can_merge[mic_index])&(length(MICROCLUSTERS)>MIN_MICS)){
                  #print("merge")
                  merge.microclusters(MICROCLUSTERS[[mic_index]],mic_index)
                  MICROCLUSTERS_SIZE <- length(MICROCLUSTERS)
                }else
                  mic_index <- mic_index + 1
              }
          }
          
        }
        
        #salvar snapshot
        TIME <- TIME + (STORE_MC*1000)
        remaining_points_buffer <- remaining_points_buffer - points_until_store
        store.snapshot(MICROCLUSTERS,TIME)
      }
      
      print(length(MICROCLUSTERS))
      #Fitting dos kfit pontos do fluxo de treino
      kfit_points <- get_points(TRAINING_STREAM, n=KFIT, class = TRUE)
      knn_testset <- kfit_points[,-(NATTRIBUTES+1)]
      knn_labelstest <- kfit_points[,(NATTRIBUTES+1)]
      HORIZONS_FITTING <- c()
      old_accuracy <- 0
      for(h in HORIZONS){
        horizon_microluster <- relating.microcluster(TIME,h)
        
        knn_trainingset <-as.data.frame(get.centers(horizon_microluster))
        
        knn_trainingset[knn_trainingset == 0] <- NA
        knn_trainingset[knn_trainingset == -Inf] <- NA
        knn_trainingset[knn_trainingset ==  Inf] <- NA
        
        classes <- as.factor(t(get.class(horizon_microluster)))
        complete_cases <- complete.cases(knn_trainingset)
        knn_trainingset <- knn_trainingset[complete_cases, ]
        classes <- classes[complete_cases]
        y_pred <- knn(knn_trainingset,knn_testset,classes,k=1)
        accuracy_kfit <- calculate.accuracy(y_pred,knn_labelstest)
        HORIZONS_FITTING <- c(HORIZONS_FITTING,list(list(training_set = knn_trainingset, accuracy = accuracy_kfit, labels = classes)))
        if(accuracy_kfit > old_accuracy){
          old_accuracy <- accuracy_kfit
          best <- horizon_microluster
        }
      }
      
      
      HISTORY_HORIZON <- c(HISTORY_HORIZON,list(best))
      #Fase de teste
      best_horizons <- get.besthorizons(HORIZONS_FITTING,P)
      # if(length(names(table(best_horizons[[1]]$labels))) < n_class){
      #   cat("Pode parar")
      #   Sys.sleep(1000)
      # }  
      test_points <- get_points(TEST_STREAM, n=BUFFER_SIZE+displacement,class = TRUE)
      test_set <- test_points[,-(NATTRIBUTES+1)]
      labels_test <- as.character(as.numeric(test_points[,(NATTRIBUTES+1)]))
      horizons_pred <- c()
      
      #calcula o knn para cada p horizon
      for(horizon in best_horizons){
        training_set <- horizon$training_set
        training_labels <- horizon$labels
        test_pred <- knn(training_set,test_set,training_labels,k=1)
        if(length(names(table(training_labels)))<n_class){
          miss_class <- miss_class + (n_class - length(names(table(training_labels))) )
        }
        horizons_pred <- cbind(horizons_pred,test_pred)
        TRAINING_HISTORY <- cbind(TRAINING_HISTORY, list(horizon))
      }
      
      #pegar a classe que mais aparece nos p horizontes resultados para cada exemplo de test
       prediction <- apply(horizons_pred,1,function(row){(names(which.max(table(row))))})
       
       labels_test <- factor(labels_test,levels = class_levels)
       SUM_LABELS <- c(SUM_LABELS,labels_test)
       SUM_LABELS <- factor(SUM_LABELS,levels = class_levels)
       
       prediction <- factor(prediction,levels = class_levels )
       SUM_PREDICTION <- c(SUM_PREDICTION,prediction)
       SUM_PREDICTION <- factor(SUM_PREDICTION,levels = class_levels)
       
       confusion_matrix <- confusionMatrix(prediction,labels_test)
       sum_confusion_matrix <- confusionMatrix(SUM_PREDICTION,SUM_LABELS)
       
       cat("Accuracy: ", sum_confusion_matrix$overall[1], "Time:",TIME/1000,"\n")
      
      
      #Pega os BUFFER_SIZE+displacement pontos do fluxo de treino para deixa-los no mesmo tempo
      displacement <- KFIT
      remaining_points <- remaining_points - (BUFFER_SIZE + KFIT)
      HISTORY <- cbind(HISTORY,list(matrix = confusion_matrix))
      SUM_HISTORY <- cbind(SUM_HISTORY,list(matrix = sum_confusion_matrix))
      
      
  }