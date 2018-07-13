# El�sz�r le kell futtatni az eg�szet a "----------" vonalig

easy = "9 2 5   3, 43278   ,  89    1,6   3  7 ,2 5   4 9, 1  9   6,3    19  ,   46935 ,7   8 2 4"
medium = "    7 4 1,   3 4  9,3     28 ,8  9   56,         ,65   1  7, 43     8,9  4 8   ,1 2 5    "
hard = "7   4 1  ,    63  7,   8  4 2,1 3    9 , 4     7 , 8    3 4,6 5  8   ,3  69    ,  7 1   5"
extreme1 = "1   68   ,5 9  4 7 , 2 9    3,     2  7, 6     4 ,9  4     ,2    1 8 , 7 2  5 4,   84   9"
extreme2 = "   7   2 , 7   59  ,  4  1 6 , 9   7  2,3       8,6  2   7 , 4 3  1  ,  69   8 , 1   4   "
extreme3 = "   4 28  ,7    9  1,    8  73, 2   79  ,    2    ,  31   6 ,84  1    ,3  2    5,  79 8   "
extreme4 = "  45    6, 2  6  7  ,8    73  ,5    17  , 3  8  9 ,  27    8,  78    9, 8  4  6 ,4    51  "
hardest = "8        ,  36     , 7  9 2  , 5   7   ,    457  ,   1   3 ,  1    68,  85   1 , 9    4  "

#beolvassa a megadott j�t�kot
beolvas<-function(sudoku){
msus<<-matrix(0,9,9)
msudoku<<-matrix(0,9,9)
colnames(msudoku)<<-paste("C",c(1:9),sep = "")
rownames(msudoku)<<-paste("R",c(1:9),sep = "")
for(i in 1:9){
msudoku[i,]<<-rbind(as.numeric(unlist(strsplit(unlist(strsplit(sudoku, ","))[i],NULL))[1:9]))}                                                 
msudoku[is.na(msudoku)]<<-0
#print(msudoku)
}

milehet<-function(msudoku){
psol<<-array(0,c(3,9,81)) # lehets�ges �rt�kek m�trixa
for(i in 1:9){for (j in 1:9){
  psol[1,j,which(msudoku[i,]==0)*9-9+i]<<-sum(!sum(msudoku[i,]==j)) # lehets�ges �rt�kek sor szerint
  psol[2,j,which(msudoku[,i]==0)+(i-1)*9]<<-sum(!sum(msudoku[,i]==j)) # lehets�ges �rt�kek oszlop szerint
  if(msudoku[(j-1)*9+i]==0){for(k in 1:9){ #lehets�ges �rt�kek kis n�gyzetek szerint
    psol[3,k,(j-1)*9+i]<<-sum(!sum(msudoku[(1+3*(ceiling(i/3)-1)):(3+3*(ceiling(i/3)-1)),(1+3*(ceiling(j/3)-1)):(3+3*(ceiling(j/3)-1))]==k))
  }}}}
jps<<-t(colSums(psol)==3)
}





jobeir<-function(){
  
  milehet(msudoku)# meghat�rozza jps -t hogy hova lehet �rni mit
  
  mstart<<-msudoku # hogy loopn�l �ssze tudja hasonl�tani a mostanival
  
  #ez n�zi, hogy �sszesen csak 1 �rt�k lehetett-e
 hely<<-which(rowSums(jps)==1)# melyik helyen lehet csak 1 alapb�l
  if(length(hely)>0){
  ertek<<-row(t(jps[hely,,drop=F]))[t(jps[hely,,drop=F])]#col(jps[hely,,drop=F])[jps[hely,,drop=F]]#ezeken a helyeken mit kell be�rni
  msudoku[hely]<<-ertek
  # p=0
  # for(p in 1:length(hely)){
  # jps[hely[p],ertek[p]]<<-F # �t�rja null�ra azt a helyet, ahol csak ezt az egyet lehetett be�rni
  }
  
  #diag(jps[hely,ertek])<<-F
    # subm<<-jps[hely,ertek]# vissza�rja hogy ne vizsg�lja �jra k�s�bb, amit most kivett
    # diag(subm)<<-F
    # jps[hely,ertek]<<-subm}#diag(jps[hely1,ertek1])<<-F ilyesmivel nem futott le (elvileg bug)
    milehet(msudoku)
  for(i in 1:9){
    #oszlop szerinti egyszeres eset
    oszlop<<-(9*(i-1)+1):(9*(i-1)+9)
    ertek1<<-which(colSums(jps[oszlop,])==1)
    hely1<<-oszlop[row(jps[oszlop,ertek1,drop=F])[jps[oszlop,ertek1]]] # drop, hogy m�trix maradjon, �s tudja a sorokat n�zni
    msudoku[hely1]<<-ertek1}# be�rja az egy�rtelm� �rt�ket
    
 milehet(msudoku)
    #diag(jps[hely1,ertek1])<<-F #a be�rt helyre vissza�rja, hogy m�r nem lehet
    # subm<<-jps[hely1,ertek1]# vissza�rja hogy ne vizsg�lja �jra k�s�bb, amit most kivett
    # diag(subm)<<-F
    # jps[hely1,ertek1]<<-subm#diag(jps[hely1,ertek1])<<-F ilyesmivel nem futott le (elvileg bug)
    
#sor szerinti egyszeres eset
  for(i in 1:9){
    sor<<-seq(i,72+i,9)
    ertek2<<-which(colSums(jps[sor,])==1)
    hely2<<-sor[row(jps[sor,ertek2,drop=F])[jps[sor,ertek2]]] # drop, hogy m�trix maradjon, �s tudja a sorokat n�zni
    msudoku[hely2]<<-ertek2}
    
    # subm<<-jps[hely1,ertek1]# vissza�rja hogy ne vizsg�lja �jra k�s�bb, amit most kivett
    # diag(subm)<<-F
    # jps[hely1,ertek1]<<-subm
    # #diag(jps[hely2,ertek2])<<-F
    
 milehet(msudoku)
    #t�mb szerinti egyszeres eset
  for(i in 1:9){
    tomb<<-c(seq((3*((i-1)%%3)+1+(ceiling(i/3)-1)*27),(3*((i-1)%%3)+3+(ceiling(i/3)-1)*27),1),
            seq((9+3*((i-1)%%3)+1+(ceiling(i/3)-1)*27),(9+3*((i-1)%%3)+3+(ceiling(i/3)-1)*27),1),
            seq(18+3*((i-1)%%3)+1+(ceiling(i/3)-1)*27,18+3*((i-1)%%3)+3+(ceiling(i/3)-1)*27,1))
    ertek3<<-which(colSums(jps[tomb,])==1) 
    hely3<<-tomb[row(jps[tomb,ertek3,drop=F])[jps[tomb,ertek3]]] # drop, hogy m�trix maradjon, �s tudja a sorokat n�zni
    msudoku[hely3]<<-ertek3}
    
    # subm<<-jps[hely3,ertek3]# vissza�rja hogy ne vizsg�lja �jra k�s�bb, amit most kivett
    # diag(subm)<<-F
    # jps[hely3,ertek3]<<-subm
    # #diag(jps[hely3,ertek3])<<-F
    
  }



jobbmegoldback<-function(game){
  msudoku<<-game
  m<-Sys.time()
  mstart<<-matrix(0,9,9)
  while(sum(msudoku-mstart)){
    jobeir()}
  # print(msudoku)
  # print(Sys.time()-m)
  }

#rosszul t�lt�tte-e fel?

check<-function(){
  msudoku[msudoku==0]<-NA
  # sum(unlist(c(apply(msudoku,1,table),apply(msudoku,2,table)))!=1)==0
  hanyszor<-c(apply(msudoku,1,table),apply(msudoku,2,table))
  
  for(i in 1:9){
    tomb<-c(seq((3*((i-1)%%3)+1+(ceiling(i/3)-1)*27),(3*((i-1)%%3)+3+(ceiling(i/3)-1)*27),1),
            seq((9+3*((i-1)%%3)+1+(ceiling(i/3)-1)*27),(9+3*((i-1)%%3)+3+(ceiling(i/3)-1)*27),1),
            seq(18+3*((i-1)%%3)+1+(ceiling(i/3)-1)*27,18+3*((i-1)%%3)+3+(ceiling(i/3)-1)*27,1))
    hanyszor<-c(hanyszor,list(table(msudoku[tomb])))}
  sum(unlist(hanyszor)!=1)==0
}


lepjhatudsz<-function(){#kiveszi az utols� m�trixot �s �rt�keket, majd megn�zi, hogy az ezel�tti j�-e, ha nem �jra lefut
  nummemory<<-nummemory[-length(nummemory[,1]),,drop=F]
  matrixmemory<<-matrixmemory[,,-dim(matrixmemory)[3],drop=F]
  msudoku<<-matrixmemory[,,dim(matrixmemory)[3]]
  
  milehet(msudoku)
  minszam<<-min(rowSums(jps)[rowSums(jps)!=0])
  ezek<<-which(rowSums(jps)==minszam)
  
  actual<<-dim(matrixmemory)[3]
  prevind1<<-nummemory[actual,3]
  prevind2<<-nummemory[actual,4]
  
  #ide kell egy rekurz�v f�ggv�ny
  if(tail(which(jps[ezek[prevind1],]),1)==nummemory[actual,2]){
    lepjhatudsz()
  } else {
    prevind2<<-prevind2+1
    nummemory[actual,2]<<-which(jps[ezek[prevind1],])[prevind2]
    nummemory[actual,4] <<- prevind2
  }
}




talaldmeg<-function(){
  #temp_sud<-msudoku
  nullhely<<-tail(nummemory,1)[1] 
  #for(u in ezek){
  tipp<<-tail(nummemory,1)[2] 
  #eddig ez volt minhely<-which(jps[u,])
  # if(sum(msudoku==matrixmemory[,,actual])==81 && tail(which(jps[ezek[prevind1],]),1)!=nummemory[actual,2]){#ha megegyezik �s a bels� index nem az utols� volt
  # minhely<-minhely[-1:-prevind2]
  # }
  #for(v in minhely){
  #szamold<-szamold+1
  msudoku[nullhely]<<-tipp
  #nummemory<-rbind(nummemory,c(u,v,which(u==ezek),which(v==minhely)))
  jobbmegoldback(msudoku)
  #matrixmemory<-abind(matrixmemory,msudoku,along=3)
  if(sum(jps)==0 && sum(msudoku==0)==0 && check()){#ha m�r nem lehet be�rni, �s nincsen  nulla, akkor megoldottuk
    #print(msudoku)
  } else if((sum(jps)==0 && sum(msudoku==0)!=0) || !check()){# nem lehet be�rni, de van m�g nulla - visszal�p
    msudoku<<-matrixmemory[,,dim(matrixmemory)[3]]#legyen �jra az el�z� m�trix a vizsg�land�
    
    actual <<- dim(matrixmemory)[3]
    prevind1 <<- nummemory[actual,3]
    prevind2 <<- nummemory[actual,4]
    
    milehet(msudoku)
    #ide kell egy rekurz�v f�ggv�ny
    if(tail(which(jps[ezek[prevind1],]),1)!=nummemory[actual,2]){
      prevind2 <<- prevind2+1
      nummemory[actual,2] <<- which(jps[ezek[prevind1],])[prevind2]
      nummemory[actual,4] <<- prevind2
      
      talaldmeg()
      # } else if(tail(ezek,1)!=nummemory[actual,1]){
      #         prevind1<-prevind1 + 1
      #         nummemory[actual,1] <- ezek[prevind1]
      #         nummemory[actual,2] <- which(jps[ezek[prevind1],])[1]
      #         nummemory[actual,3] <- prevind1
      #         nummemory[actual,4] <- 1
    } else{ 
      lepjhatudsz()
      
      talaldmeg()#visszal�p egy kor�bbon vizsg�lt m�trixra
    }
    #matrixmemory<-matrixmemory[,,-dim(matrixmemory)[3]] # t�r�lje ki a mem�ri�b�l az utols� rossz m�trixot
    
    
    #milehet(msudoku)
    # minszam<-min(rowSums(jps)[rowSums(jps)!=0])
    # ezek<-which(rowSums(jps)==minszam)
    # 
    # actual<-dim(matrixmemory)[3]
    # prevind1<-nummemory[actual-1,3]
    # prevind2<-nummemory[actual-1,4]
    # 
    # if(tail(which(jps[ezek[prevind1],]),1)==nummemory[actual,2]){#ha megegyezik �s a bels� index az utols� volt, ez is benne volt sum(msudoku==matrixmemory[,,actual])==81 && 
    #   ezek<-ezek[-1:-prevind1]
    # }
    # 
    
    
    
  } else {#lehet be�rni, �s van is  m�g nulla, teh�t tippel megint
    
    #milehet(msudoku)
    minszam<<-min(rowSums(jps)[rowSums(jps)!=0])
    ezek<<-which(rowSums(jps)==minszam)
    nummemory<<-rbind(nummemory,c(ezek[1],which(jps[ezek[1],])[1],1,1))
    matrixmemory<<-abind(matrixmemory,msudoku,along=3)
    talaldmeg()
  }
}





msolve<-function(name){
  mer<-Sys.time()
  beolvas(name)
  milehet(msudoku)
  
  # sum(rowSums(jps))==0#els� felt�tel
  # sum(jps)==0 #ez is el�g nem kell az el�z�
  
  
  
  
  #szamold<-0
  
  
  minszam<<-min(rowSums(jps)[rowSums(jps)!=0])
  ezek<<-which(rowSums(jps)==minszam)
  nummemory<<-matrix(c(ezek[1],which(jps[ezek[1],])[1],1,1),1,4)
  colnames(nummemory)<<-c("nullhely","tipp","prevind1","prevind2")
  matrixmemory<<-array(msudoku,c(9,9,1))
  
  talaldmeg()
  
  print(msudoku)
  print(Sys.time()-mer)}





#----------
#valamelyik el�re bevitt j�t�k
sudokujatek<-hardest
#beolvas f�ggv�nybe az egyik inputot a 6 k�z�l (most csak easy-re fut le j�l, a t�bbire nincs egy�rtelm� l�p�s)
beolvas(sudokujatek)

#ezt v�ltoztat�s n�lk�l lehet futtatni, mert msudoku-nak menti a beolvas�s ut�n
msolve(sudokujatek)
