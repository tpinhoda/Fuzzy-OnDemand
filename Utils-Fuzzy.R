#============================================FUNCÕES============================================================================
create.datastream <- function(dataset){
  #Transformar um data set em um fluxo
  #Cria o fluxo a partir do dataset escolhido
  #Caso queira um fluxo infinito mude o atributo loop para TRUE
  stream <- DSD_Memory(dataset[,c(1:NATTRIBUTES)], class=dataset[,NATTRIBUTES+1], loop = FALSE)  
  return(stream)
}

split.class <- function(dataset){
  #Cria uma lista de pontos separados pela classe
  class_list <-dataset[,NATTRIBUTES+1]
  splitted <- split(dataset,class_list)
  return(splitted)
}

create.microcluster <- function(center,class){
  #Cria um novo micro-grupo
  MC_ID <<- MC_ID + 1
  return(list(CF1x=center,CF2x=center^2,CF1t=TIME,CF2t=TIME^2,n=1,M=1,class_id=class,id=MC_ID))
}

get.centers <- function(MICROCLUSTERS){
  #Retorna o centro de todos os micro-grupos
  return(t(sapply(MICROCLUSTERS, function(microcluster){
      microcluster$CF1x/microcluster$M
  })))
}

get.class <- function(MICROCLUSTERS){
  #Retorna o centro de todos os micro-grupos
  return(t(sapply(MICROCLUSTERS, function(microcluster){
      microcluster$class_id
  })))
}

dist.microclusters <- function(MICROCLUSTERS){
  #Retorna uma matrix de distancia entro todosos micro-grupos
  
  micro_clusters_centers <- get.centers(MICROCLUSTERS)
  
  #Calcula a distancia entre os micro-grupos
  dist_centers <- dist(micro_clusters_centers)
  dist_centers <- as.matrix(dist_centers)
  
  #renomeia as linhas e colunas
  rownames(dist_centers) <- c(1:length(MICROCLUSTERS))
  colnames(dist_centers) <- c(1:length(MICROCLUSTERS))
  
  return(dist_centers)
}

find.microclusters <-function(MICROCLUSTERS,class){
  #retorna a lista de ids de micro-grupos da mesma classe
  ids_class <- c()
  for(microcluster_index in 1:MICROCLUSTERS_SIZE)
    if(MICROCLUSTERS[[microcluster_index]]$class_id == class)
       ids_class <- c(ids_class,microcluster_index)
  return(ids_class)  
}

get.distances <- function(MICROCLUSTERS,point){
  microclusters_centers <- get.centers(MICROCLUSTERS)              #Recupera os centros CF1x/n de todos os microclusters
  distances <- apply(microclusters_centers,1,function(centers){    #Calcula a distancia do novo ponto para todos os microclusters
                    dist(rbind(centers, point))
                })
  return(distances)
}
is.empty <- function(x) return(length(x) == 0)

calculate.membership <- function(distances,mic_size){
  membership <- numeric()
  ## Calculating membership values for p in all availableFMiC
  for(j in 1:mic_size){
    s <- 0
    for(k in 1:mic_size) {
      if(distances[j] > 0 && distances[k]>0)
        s <- s + ((distances[j]/distances[k])^(2/(FUZZY_M-1)))
    }
    if(s > 0)
      membership <- c(membership, 1/s)
    else
      membership <- c(membership, s)
  }
  return(membership)
}

nearest.microcluster <- function(MICROCLUSTERS,point,class){
  class_microclusters <- find.microclusters(MICROCLUSTERS,class)
  if(is.empty(class_microclusters))
    return(class_microclusters)
  point_distances <- get.distances(MICROCLUSTERS,point)
  point_memberships <- calculate.membership(point_distances,MICROCLUSTERS_SIZE)

  thresholded_memberships <- point_memberships[class_microclusters] > 0.1

  nearest_index <- class_microclusters[thresholded_memberships]
  nearest_dist <-  point_memberships[class_microclusters[thresholded_memberships]]
  nearests <- cbind(index = nearest_index,membership = nearest_dist)
  return(nearests)
}


mean.timestamp <- function(microcluster){
  points_number <- microcluster$n
  std_timestamp <- sqrt((microcluster$CF2t/points_number)-(microcluster$CF1t/points_number)^2)
  mean_timestamp <- microcluster$CF1t/points_number
  
  
    mean_timestamp <- qnorm(((M*points_number)/(2*points_number)),mean_timestamp,std_timestamp) 
  return(mean_timestamp)
    
}

find.min.relevant <- function(MICROCLUSTERS){
  mean_timestamps <- sapply(MICROCLUSTERS,function(microcluster){mean.timestamp(microcluster)})
  min_relevance <- min(mean_timestamps)
  min_relevant_index <- which.min(mean_timestamps)
  return(c(index = min_relevant_index, relevance = min_relevance))
}


find.min.relevant.by.class <- function(MICROCLUSTERS,class){
  mean_timestamps <<- sapply(MICROCLUSTERS,function(microcluster){if(microcluster$class_id == class){
                                                                  mean.timestamp(microcluster)
                                                                }else{
                                                                  Inf
                                                                }})
  min_relevance <- min(mean_timestamps)
  min_relevant_index <- which.min(mean_timestamps)
  return(c(index = min_relevant_index, relevance = min_relevance))
}


check.relevance <- function(min._relevant, point_stream, class){
  if(length(MICROCLUSTERS) < MAX_MICS){
   mic <- create.microcluster(point_stream,class)
   MICROCLUSTERS <<- c(MICROCLUSTERS,list(mic))
   can_merge <<- c(can_merge,1)
  }else{
      delete.microcluster(min_relevant['index'], point_stream,class)
      can_merge[min_relevant['index']] <<- 1
  }  
}

delete.microcluster <- function(microcluster_index,point,class){

  MICROCLUSTERS[[microcluster_index]]$CF1x <<-  point
  MICROCLUSTERS[[microcluster_index]]$CF2x <<-  point*point
  MICROCLUSTERS[[microcluster_index]]$CF1t <<-  TIME
  MICROCLUSTERS[[microcluster_index]]$CF2t <<-  TIME^2
  MICROCLUSTERS[[microcluster_index]]$n <<-  1
  MICROCLUSTERS[[microcluster_index]]$class_id <<- class
  MC_ID <<- MC_ID + 1
  #cat("deleted ", MICROCLUSTERS[[microcluster_index]]$id,"\n")
  MICROCLUSTERS[[microcluster_index]]$id <<- MC_ID
  MICROCLUSTERS[[microcluster_index]]$M <<- 1
  
  #Calculo do limite de acão maximo de cada micro-grupo inicial
  
}


sum.microclusters <- function(mc1,mc2){

  MICROCLUSTERS[[mc1]]$CF1x <<-  MICROCLUSTERS[[mc1]]$CF1x + MICROCLUSTERS[[mc2]]$CF1x
  MICROCLUSTERS[[mc1]]$CF2x <<-  MICROCLUSTERS[[mc1]]$CF2x + MICROCLUSTERS[[mc2]]$CF2x  

  MICROCLUSTERS[[mc1]]$M <<-  MICROCLUSTERS[[mc1]]$M + MICROCLUSTERS[[mc2]]$M
  MICROCLUSTERS[[mc1]]$n <<-  MICROCLUSTERS[[mc1]]$n + MICROCLUSTERS[[mc2]]$n
  MICROCLUSTERS[[mc1]]$id <<- c(MICROCLUSTERS[[mc1]]$id,MICROCLUSTERS[[mc2]]$id )
  
  # MICROCLUSTERS[[mc1]]$CF1t <<-  MICROCLUSTERS[[mc1]]$CF1t + MICROCLUSTERS[[mc2]]$CF1t
  # MICROCLUSTERS[[mc1]]$CF2t <<-  MICROCLUSTERS[[mc1]]$CF2t + MICROCLUSTERS[[mc2]]$CF2t  

  if(MICROCLUSTERS[[mc2]]$CF1t > MICROCLUSTERS[[mc1]]$CF1t)
    MICROCLUSTERS[[mc1]]$CF1t <- MICROCLUSTERS[[mc2]]$CF1t
  if(MICROCLUSTERS[[mc2]]$CF2t > MICROCLUSTERS[[mc1]]$CF2t)
    MICROCLUSTERS[[mc1]]$CF2t <- MICROCLUSTERS[[mc2]]$CF2t

}

merge.microclusters <- function(mic,index_mic){
  
  class <- mic$class_id
  center <- mic$CF1x/mic$M
  class_microclusters <- find.microclusters(MICROCLUSTERS,class)
  
  if(!is.empty(class_microclusters)){
    point_distances <- get.distances(MICROCLUSTERS,center)
    point_class_distance <- point_distances[class_microclusters]
    
    point_memberships <- calculate.membership(point_distances[class_microclusters],length(point_class_distance))
    thresholded_memberships <- point_memberships > FUZZY_THETA
    nearest_index <- class_microclusters[thresholded_memberships]
    #cat("min_class: ",min_relevant_class,"\n")
    for(index in nearest_index ){
      if(can_merge[index]){
        if(MICROCLUSTERS[[index]]$class_id !=  MICROCLUSTERS[[index_mic]]$class_id){
          cat("de ruim mesmo no merge \n")
          cat(" id: ",MICROCLUSTERS[[index]]$class_id," id: ",MICROCLUSTERS[[index_mic]]$class_id,"\n")
          Sys.sleep(10000)
        }
        sum.microclusters(index,index_mic)
        can_merge[index] <<- 0  
      }  
    }
    can_merge <<- can_merge[-index_mic]
    MICROCLUSTERS <<- MICROCLUSTERS[-index_mic]
  }
}

get.maxboundary <- function(MICROCLUSTERS,mic_index){
  mic <- MICROCLUSTERS[[mic_index]]
  if(mic$n == 1)
    return(mc_initial_max_boundary[mic_index])
  rmsd <- sqrt(sum(mic$CF2x)/mic$n - sum(mic$CF1x^2)/mic$n^2)
  return(T*rmsd)
}

add.point <- function(microcluster,point){
  microcluster_index <- microcluster[1]
  microcluster_membership <- microcluster[2]
  MICROCLUSTERS[[microcluster_index]]$CF1x <<- MICROCLUSTERS[[microcluster_index]]$CF1x + (point*microcluster_membership)
  MICROCLUSTERS[[microcluster_index]]$CF2x <<- MICROCLUSTERS[[microcluster_index]]$CF2x + ((point^2)*microcluster_membership)
  MICROCLUSTERS[[microcluster_index]]$CF1t <<- MICROCLUSTERS[[microcluster_index]]$CF1t + TIME
  MICROCLUSTERS[[microcluster_index]]$CF2t <<- MICROCLUSTERS[[microcluster_index]]$CF2t + TIME^2
  MICROCLUSTERS[[microcluster_index]]$n <<- MICROCLUSTERS[[microcluster_index]]$n + 1
  MICROCLUSTERS[[microcluster_index]]$M <<- MICROCLUSTERS[[microcluster_index]]$M + microcluster_membership
  
  
}

get.rows = function(training_points) lapply(seq_len(nrow(training_points)),function(index) return(data.matrix(training_points[index,])))

mod<-function(x,m){ 
  t1<-x/m
  return(x-t1*m)
}

store.snapshot <- function(MICROCLUSTERS, TIME){
  for(frame_number in FRAMES){
    frame_index <- frame_number + 1
    if((TIME%%(2^frame_number)==0) & (TIME%%(2^(frame_number+1))>0))  {
      if(as.numeric(length(SNAPSHOTS[[frame_index]]$frame_slot)) < FRAME_MAX_CAPACITY){
        SNAPSHOTS[[frame_index]]$frame_slot <<- c(SNAPSHOTS[[frame_index]]$frame_slot,list(list(MICROCLUSTERS = MICROCLUSTERS, time = TIME)))
      }else{
        SNAPSHOTS[[frame_index]]$frame_slot <<- c(SNAPSHOTS[[frame_index]]$frame_slot[2:FRAME_MAX_CAPACITY],list(list(MICROCLUSTERS = MICROCLUSTERS, time = TIME))) 
      }
    }  
  }
}


get.centersclass <- function(MICROCLUSTERS){
  return(t(sapply(MICROCLUSTERS, function(microcluster){
    c(as.numeric(microcluster$CF1x/microcluster$M),microcluster$class_id)
  })))
}


find.snapshot <- function(t){
  while(t >= 0){
    for(fr in FRAMES+1){
      snaps <- 1
      while(snaps <= length(SNAPSHOTS[[fr]]$frame_slot)){
        if(SNAPSHOTS[[fr]]$frame_slot[[snaps]]$time == t){
          #cat("time :", SNAPSHOTS[[fr]]$frame_slot[[snaps]]$time,"\n" )
          return(SNAPSHOTS[[fr]]$frame_slot[[snaps]]$MICROCLUSTERS)
        }
        snaps <- snaps + 1
      }
    }
    t <- t - 1
  }  
}

relating.microcluster <- function(time,horizon) {
#  cat("==========================horizon======================\n")
#  cat("TIME: ",time,"h: ",horizon)
  mc1 <- find.snapshot(time)
  mc2 <- find.snapshot(time-horizon)
  idmc1 <- 1
  
  while(idmc1 < length(mc1)){
    idmc2 <- 1
    
    str_id1 <<- paste(" ",mc1[[idmc1]]$id," ", collapse = " ")
    while(idmc2 < length(mc2)&length(mc1[[idmc1]]$id)>0){
      # relating <- TRUE
      # for(index in mc2[[idmc2]]$id){
      #   if(!index %in% mc1[[idmc1]]$id){
      #     relating <- FALSE
      #   }
      #   aux <- aux[-match(index,aux)]
      # }
      str_id2 <<- paste(" ",mc2[[idmc2]]$id," ", collapse = " ")  
      if(!is.empty(grep(str_id2,str_id1))){
        #cat("str1:",str_id1,"str2",str_id2,"\n")
         if(mc1[[idmc1]]$class_id !=  mc2[[idmc2]]$class_id){
           cat("de ruim mesmo \n")
           cat(" id: ",mc1[[idmc1]]$id," id: ",mc2[[idmc2]]$id,"\n")
           cat("diff: ",mc1[mc1[[idmc1]]$id %nin% mc2[[idmc2]]$id],"\n")
           Sys.sleep(10000)
         }
         
        #cat("idmc1 : ",idmc1, "idmc2: ",idmc2,"\n")
        #cat(" id: ",mc1[[idmc1]]$id," id: ",mc2[[idmc2]]$id,"\n")
        
        mc1[[idmc1]]$CF1x <- mc1[[idmc1]]$CF1x - mc2[[idmc2]]$CF1x
        mc1[[idmc1]]$CF2x <- mc1[[idmc1]]$CF2x - mc2[[idmc2]]$CF2x
        mc1[[idmc1]]$CF1t <- mc1[[idmc1]]$CF1t - mc2[[idmc2]]$CF1t
        mc1[[idmc1]]$CF2t <- mc1[[idmc1]]$CF2t - mc2[[idmc2]]$CF2t
        mc1[[idmc1]]$n <- mc1[[idmc1]]$n - mc2[[idmc2]]$n
        mc1[[idmc1]]$M <- mc1[[idmc1]]$M - mc2[[idmc2]]$M
        mc1[[idmc1]]$id <-as.numeric(strsplit(gsub(str_id2,"",str_id1)," ")[[1]])
        mc1[[idmc1]]$id <- mc1[[idmc1]]$id[complete.cases(mc1[[idmc1]]$id)]
        str_id1 <<- paste(" ",mc1[[idmc1]]$id," ", collapse = " ")
         #  if(length(mc1[[idmc1]]$id)> 0){
         #    
         #    cat("diff: ",mc1[[idmc1]]$id,"\n")
         # #   Sys.sleep(10)
         #  }
        # 
      }  
      idmc2 <- idmc2 + 1
    }
    idmc1 <- idmc1 + 1
  }  
  return(mc1)
}

eucDist <- function(x1, x2){ return(sqrt(sum((x1 - x2) ^ 2))) }

calculate.accuracy <- function(y_pred,label){
  index_label <- 1
  right <- 0

  for(y in y_pred){
    if(y == label[index_label])
      right <- right + 1
    index_label <- index_label + 1
  }
  return(right/index_label)
}

get.besthorizons <- function(HORIZONS_FITTING,P){
  count_class <- sapply(HORIZONS_FITTING,function(horizon){length(names(table(horizon$labels)))})
  most_class <- count_class == max(count_class)
  print(count_class[most_class])
  accuracy <- sapply(HORIZONS_FITTING,function(horizon){horizon$accuracy})
  best_accuracy <- order(accuracy[most_class],decreasing = T)[1:P]
  best_horizons <- c()
  for(index in best_accuracy){
    best_horizons <- c(best_horizons,list(HORIZONS_FITTING[[index]]))
  }
  return(best_horizons)
}