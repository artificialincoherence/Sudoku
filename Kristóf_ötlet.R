game<-beolvas(hard)
jobbmegold(game)
mat<-matrix(as.numeric(gsub('[1-9]',1,msudoku)),9,9)# ha vissza kell menni korábbra az tök könnyû ösze kell szorozni az 1 és 0-ból álló mátrixot az addigival


memory_mat<-list(mat)
####memory_num<-matrix(0,1,3)
#####colnames(memory_num)<-c('hely','szam','index')

if(sum(rowSums(jps))==0) {print("Nincs megoldás.")}
i<-1
j<-1

repeat{
  #milehet(msudoku)
  #jps<-t((colSums(psol)==3))
  #rs_jps<-rowSums(t((colSums(psol)==3)))
  rs_jps<-rowSums(jps)
  # rs_jps
  #sum(rs_jps) # ha ez nulla, akkor kell visszalépni
  repp<-replace(rs_jps,which(rs_jps==0),99)
  # repp
  ri<-which(repp==min(repp))
  
  # for(i in 1:length(ri)){}
  ri<-ri[i]
  
  # jps
  
  # for (j in 1:length(which(jps[ri,,drop=F]==T)))
  
  mat<-matrix(as.numeric(gsub('[1-9]',1,msudoku)),9,9)# ha vissza kell menni korábbra az tök könnyû ösze kell szorozni az 1 és 0-ból álló mátrixot az addigival
  memory_mat<-append(memory_mat,list(mat))  
  
  uj_szam<<-which(jps[ri,,drop=F]==T)[j] # ezt a számot kell beírni az ri helyre az eredeti mátrixban
  # msus<<-msudoku
  
  # mat<-matrix(as.numeric(gsub('[1-9]',1,msudoku)),9,9)# ha vissza kell menni korábbra az tök könnyû ösze kell szorozni az 1 és 0-ból álló mátrixot az addigival
  # memory_mat<-append(memory_mat,list(mat))
  
  msudoku[ri]<-uj_szam
  
  
  #megold(msudoku)
  
  # compare<-megold(msudoku)
  # compare<-((megold(msudoku)-msudoku)!=0)
  
  # memory_mat
  
  #############memory_num<-rbind(memory_num,matrix(c(ri,uj_szam,j),1,3))#arrayt hogy lehet bõvíteni?
  # memory_num
  print(msudoku)
  jobbmegold(msudoku)
  
  if(prod(msudoku)!=0 && sum(rowSums(jps))==0){
    msudoku<<-msudoku*memory_mat[[length(memory_mat)]]
    memory_mat[length(memory_mat)]<-NULL
    ifelse(j+1>length(which(jps[ri,,drop=F]==T)),
           ifelse(i+1>length(ri),
                  print("Nincs megoldás!"),
                  i<-(i+1) &
                    j<-1),
           j<-j+1)
  }
  
  if(prod(msudoku)==0 && sum(rowSums(jps))==0){
    msudoku<<-msudoku*memory_mat[[length(memory_mat)]]
    memory_mat[length(memory_mat)]<-NULL
    ifelse(j+1>length(which(jps[ri,,drop=F]==T)),
           ifelse(i+1>length(ri),
                  print("Nincs megoldás!"),
                  i<-(i+1) &
                    j<-1),
           j<-j+1)
  }
  
  
  
  
  print(i)
  print(j)
  
  if(prod(msudoku)!=0 && sum(rowSums(jps))==0){
    print(as.sudoku(msudoku))
    break
  }