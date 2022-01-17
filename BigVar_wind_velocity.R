## Read in data

wind_tab = read.csv("...")[,-1]
wind = as.matrix(wind_tab)

big_var<-function(p){
  for(j in 1:2) {
    if (j==1){
      wind_train <- wind[(round(0.2*dim(wind)[1])+1):dim(wind)[1],]
      wind_valid <- wind[0:round(0.2*dim(wind)[1]),]
    }
    else{
      wind_train <- wind[0:round(0.8*dim(wind)[1]),]
      wind_valid <- wind[(round(0.8*dim(wind)[1])+1):dim(wind)[1],]
    }
    mod1<-constructModel(wind_train,p=p,"Basic",gran=c(50,10),RVAR=FALSE,h=1,cv="Rolling",
                         MN=FALSE,verbose=TRUE,IC=TRUE)
    results=cv.BigVAR(mod1)
    prediction = t(predict(results,n.ahead=1))
    
    if (p==1){
      for (i in p:(dim(wind_valid)[1]-1)){
        results@Zvals <- rbind(1,as.matrix(unname(wind_valid[i,])))
        prediction <- rbind(prediction,t(predict(results,n.ahead=1)))
      }
    }
    
    if (p==2){
      results@Zvals <- rbind(1,as.matrix(unname(wind_valid[1,])),
                             as.matrix(unname(wind_train[dim(wind_train)[1],])))
      prediction <- rbind(prediction,t(predict(results,n.ahead=1)))
      for (i in p:(dim(wind_valid)[1]-1)){
        results@Zvals <- rbind(1,as.matrix(unname(wind_valid[i,])),
                               as.matrix(unname(wind_valid[i-1,])))
        prediction <- rbind(prediction,t(predict(results,n.ahead=1)))
      }
    }
    
    if (p==3){
      results@Zvals <- rbind(1,as.matrix(unname(wind_valid[1,])),
                             as.matrix(unname(wind_train[dim(wind_train)[1],])),
                             as.matrix(unname(wind_train[dim(wind_train)[1]-1,])))
      prediction <- rbind(prediction,t(predict(results,n.ahead=1)))
      results@Zvals <- rbind(1,as.matrix(unname(wind_valid[2,])),
                             as.matrix(unname(wind_valid[1,])),
                             as.matrix(unname(wind_train[dim(wind_train)[1],])))
      prediction <- rbind(prediction,t(predict(results,n.ahead=1)))
      for (i in p:(dim(wind_valid)[1]-1)){
        results@Zvals <- rbind(1,as.matrix(unname(wind_valid[i,])),
                               as.matrix(unname(wind_valid[i-1,])),
                               as.matrix(unname(wind_valid[i-2,])))
        prediction <- rbind(prediction,t(predict(results,n.ahead=1)))
      }
    }

    if (p==4){
      results@Zvals <- rbind(1,as.matrix(unname(wind_valid[1,])),
                             as.matrix(unname(wind_train[dim(wind_train)[1],])),
                             as.matrix(unname(wind_train[dim(wind_train)[1]-1,])),
                             as.matrix(unname(wind_train[dim(wind_train)[1]-2,])))
      prediction <- rbind(prediction,t(predict(results,n.ahead=1)))
      results@Zvals <- rbind(1,as.matrix(unname(wind_valid[2,])),
                             as.matrix(unname(wind_valid[1,])),
                             as.matrix(unname(wind_train[dim(wind_train)[1],])),
                             as.matrix(unname(wind_train[dim(wind_train)[1]-1,])))
      prediction <- rbind(prediction,t(predict(results,n.ahead=1)))
      results@Zvals <- rbind(1,as.matrix(unname(wind_valid[3,])),
                             as.matrix(unname(wind_valid[2,])),
                             as.matrix(unname(wind_valid[1,])),
                             as.matrix(unname(wind_train[dim(wind_train)[1],])))
      prediction <- rbind(prediction,t(predict(results,n.ahead=1)))
      for (i in p:(dim(wind_valid)[1]-1)){
        results@Zvals <- rbind(1,as.matrix(unname(wind_valid[i,])),
                               as.matrix(unname(wind_valid[i-1,])),
                               as.matrix(unname(wind_valid[i-2,])),
                               as.matrix(unname(wind_valid[i-3,])))
        prediction <- rbind(prediction,t(predict(results,n.ahead=1)))
      }
    }
        
    colnames(prediction) <- colnames(wind_tab)
    write.table(prediction, paste('prediction_BigVAR_split_',as.character(j),'_p=',
                                  as.character(p),'.txt',sep=""))
  }
}

big_var(1)
big_var(2)
big_var(3)
big_var(4)
