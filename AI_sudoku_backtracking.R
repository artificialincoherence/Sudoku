# Először le kell futtatni az egészet a "---------" vonalig a végén

#ezeket kell telepíteni
#install.packages("abind")
library(abind) 

#vizualizációhoz, és a megoldás gyorsaságának összehasonlításához
#install.packages("sudokuAlt")
library(sudokuAlt)

easy = "9 2 5   3, 43278   ,  89    1,6   3  7 ,2 5   4 9, 1  9   6,3    19  ,   46935 ,7   8 2 4"
medium = "    7 4 1,   3 4  9,3     28 ,8  9   56,         ,65   1  7, 43     8,9  4 8   ,1 2 5    "
hard = "7   4 1  ,    63  7,   8  4 2,1 3    9 , 4     7 , 8    3 4,6 5  8   ,3  69    ,  7 1   5"
extreme1 = "1   68   ,5 9  4 7 , 2 9    3,     2  7, 6     4 ,9  4     ,2    1 8 , 7 2  5 4,   84   9"
extreme2 = "   7   2 , 7   59  ,  4  1 6 , 9   7  2,3       8,6  2   7 , 4 3  1  ,  69   8 , 1   4   "
extreme3 = "   4 28  ,7    9  1,    8  73, 2   79  ,    2    ,  31   6 ,84  1    ,3  2    5,  79 8   "
extreme4 = "  45    6, 2  6  7  ,8    73  ,5    17  , 3  8  9 ,  27    8,  78    9, 8  4  6 ,4    51  "
hardest = "8        ,  36     , 7  9 2  , 5   7   ,    457  ,   1   3 ,  1    68,  85   1 , 9    4  "

#csak a végéhez, hogy könnyen lehessen választani
game<-c(easy,medium,hard,extreme1,extreme2,extreme3,extreme4,hardest)

#beolvassa a megadott játékot
beolvas<-function(sudoku){
msus<<-matrix(0,9,9)
msudoku<<-matrix(0,9,9)
colnames(msudoku)<<-paste("C",c(1:9),sep = "") #1:9 #
rownames(msudoku)<<-paste("R",c(1:9),sep = "") #c("A","B","C","D","E","F","G","H","I") # 
for(i in 1:9){
msudoku[i,]<<-rbind(as.numeric(unlist(strsplit(unlist(strsplit(sudoku, ","))[i],NULL))[1:9]))}                                                 
msudoku[is.na(msudoku)]<<-0
#print(msudoku)
}

#megmondja, hogy az üres cellákban milyen értékek szerepelhetnek
milehet<-function(msudoku){
psol<<-array(0,c(3,9,81)) # lehetséges értékek mátrixa
for(i in 1:9){for (j in 1:9){
  psol[1,j,which(msudoku[i,]==0)*9-9+i]<<-sum(!sum(msudoku[i,]==j)) # lehetséges értékek sor szerint
  psol[2,j,which(msudoku[,i]==0)+(i-1)*9]<<-sum(!sum(msudoku[,i]==j)) # lehetséges értékek oszlop szerint
  if(msudoku[(j-1)*9+i]==0){for(k in 1:9){ #lehetséges értékek kis négyzetek szerint
    psol[3,k,(j-1)*9+i]<<-sum(!sum(msudoku[(1+3*(ceiling(i/3)-1)):(3+3*(ceiling(i/3)-1)),(1+3*(ceiling(j/3)-1)):(3+3*(ceiling(j/3)-1))]==k))
  }}}}
jps<<-t(colSums(psol)==3)
}# lehetséges értékek jobban oszlopban cella száma, ahol TRUE van oda mehet az oszlop szerinti szám





#az üres helyekre beírja az egyértelmű eseteket
jobeir<-function(){
  
  milehet(msudoku)# meghatározza jps -t hogy hova lehet írni mit
  
  mstart<<-msudoku # hogy loopnál össze tudja hasonlítani a mostanival
  
  #ez nézi, hogy összesen csak 1 érték lehetett-e
 hely<<-which(rowSums(jps)==1)# melyik helyen lehet csak 1 alapból
  if(length(hely)>0){
  ertek<<-row(t(jps[hely,,drop=F]))[t(jps[hely,,drop=F])]#ezeken a helyeken mit kell beírni
  msudoku[hely]<<-ertek
  }
  
 #milehet(msudoku)
 
 #oszlop szerinti egyszeres eset
  for(i in 1:9){
    oszlop<<-(9*(i-1)+1):(9*(i-1)+9)
    ertek1<<-which(colSums(jps[oszlop,])==1)
    hely1<<-oszlop[row(jps[oszlop,ertek1,drop=F])[jps[oszlop,ertek1]]] # drop, hogy mátrix maradjon, és tudja a sorokat nézni
    msudoku[hely1]<<-ertek1}# beírja az egyértelmű értéket
    
 #milehet(msudoku)
    
#sor szerinti egyszeres eset
  for(i in 1:9){
    sor<<-seq(i,72+i,9)
    ertek2<<-which(colSums(jps[sor,])==1)
    hely2<<-sor[row(jps[sor,ertek2,drop=F])[jps[sor,ertek2]]] # drop, hogy mátrix maradjon, és tudja a sorokat nézni
    msudoku[hely2]<<-ertek2}
    
  #milehet(msudoku)
  #tömb szerinti egyszeres eset
  for(i in 1:9){
    tomb<<-c(seq((3*((i-1)%%3)+1+(ceiling(i/3)-1)*27),(3*((i-1)%%3)+3+(ceiling(i/3)-1)*27),1),
            seq((9+3*((i-1)%%3)+1+(ceiling(i/3)-1)*27),(9+3*((i-1)%%3)+3+(ceiling(i/3)-1)*27),1),
            seq(18+3*((i-1)%%3)+1+(ceiling(i/3)-1)*27,18+3*((i-1)%%3)+3+(ceiling(i/3)-1)*27,1))
    ertek3<<-which(colSums(jps[tomb,])==1) 
    hely3<<-tomb[row(jps[tomb,ertek3,drop=F])[jps[tomb,ertek3]]] # drop, hogy mátrix maradjon, és tudja a sorokat nézni
    msudoku[hely3]<<-ertek3}
    
}


# annyiszor lefut egymás után a jobeir(), amennyiszer tud újat beírni
jobbmegoldback<-function(game){
  msudoku<<-game
  #m<-Sys.time()
  mstart<<-matrix(0,9,9)
  while(sum(msudoku-mstart)){
    jobeir()}
  # print(msudoku)
  # print(Sys.time()-m)
  }

#rosszul töltötte-e fel? megnézi, hogy jó-e az eredmény (vagy részeredmény)

check<-function(){
  msudoku[msudoku==0]<-NA # hogy a nullákat ne számolja a gyakorisági táblában
  
  hanyszor<-c(apply(msudoku,1,table),apply(msudoku,2,table)) # sor és oszlop szerint csak 1
  
  for(i in 1:9){# tömbök szerint ellenőriz
    tomb<-c(seq((3*((i-1)%%3)+1+(ceiling(i/3)-1)*27),(3*((i-1)%%3)+3+(ceiling(i/3)-1)*27),1),
            seq((9+3*((i-1)%%3)+1+(ceiling(i/3)-1)*27),(9+3*((i-1)%%3)+3+(ceiling(i/3)-1)*27),1),
            seq(18+3*((i-1)%%3)+1+(ceiling(i/3)-1)*27,18+3*((i-1)%%3)+3+(ceiling(i/3)-1)*27,1))
    hanyszor<-c(hanyszor,list(table(msudoku[tomb])))}
  sum(unlist(hanyszor)!=1)==0 # TRUE ha nincs ismétlődés, FALSE, ha van
}


# a következő függvényhez kell, ha már adott cellában megnézte az összes lehetséges esetet, 
#és egyikre se talált további jó megoldást, akkor visszalép az előző cellába, ahova a legkevesebbet 
#lehetett beírni
#kiveszi az utolsó mátrixot és értékeket, majd megnézi, hogy az ezelőtti jó-e, ha nem újra lefut
lepjhatudsz<-function(){
  nummemory<<-nummemory[-length(nummemory[,1]),,drop=F]
  matrixmemory<<-matrixmemory[,,-dim(matrixmemory)[3],drop=F]
  msudoku<<-matrixmemory[,,dim(matrixmemory)[3]]
  
  milehet(msudoku)
  minszam<<-min(rowSums(jps)[rowSums(jps)!=0])
  ezek<<-which(rowSums(jps)==minszam)
  
  actual<<-dim(matrixmemory)[3]
  prevind1<<-nummemory[actual,3]
  prevind2<<-nummemory[actual,4]
  
  
  if(tail(which(jps[ezek[prevind1],]),1)==nummemory[actual,2]){
    lepjhatudsz()
  } else {
    prevind2<<-prevind2+1
    nummemory[actual,2]<<-which(jps[ezek[prevind1],])[prevind2]
    nummemory[actual,4] <<- prevind2
  }
}



# egy mátrixba gyűjti (nullhely), hogy mi lesz a következő tipp, amit beír a sudokuba
# és minden kimenet esetén frissíti ezt a mátrixot mielőtt újra meghívná magát a függvény
talaldmeg<-function(){
  
  nullhely<<-tail(nummemory,1)[1] 
  
  tipp<<-tail(nummemory,1)[2] 
  
  msudoku[nullhely]<<-tipp
  
  jobbmegoldback(msudoku)
  
  if(sum(jps)==0 && sum(msudoku==0)==0 && check()){#ha már nem lehet beírni, és nincsen  nulla, akkor megoldottuk
    #print(msudoku)
  } else if((sum(jps)==0 && sum(msudoku==0)!=0) || !check()){# nem lehet beírni, de van még nulla, vagy rosszul töltötte ki - visszalép
    msudoku<<-matrixmemory[,,dim(matrixmemory)[3]]#legyen újra az előző mátrix a vizsgálandó
    
    actual <<- dim(matrixmemory)[3]
    prevind1 <<- nummemory[actual,3]
    prevind2 <<- nummemory[actual,4]
    
    milehet(msudoku)
    
    if(tail(which(jps[ezek[prevind1],]),1)!=nummemory[actual,2]){
      prevind2 <<- prevind2+1
      nummemory[actual,2] <<- which(jps[ezek[prevind1],])[prevind2]
      nummemory[actual,4] <<- prevind2
      
      talaldmeg()

    } else{ 
      lepjhatudsz()#visszalép egy korábbon vizsgált mátrixra
      
      talaldmeg()
    }
    
  } else {#lehet beírni, és van is  még nulla, tehát tippel megint
    
    #milehet(msudoku)
    minszam<<-min(rowSums(jps)[rowSums(jps)!=0])
    ezek<<-which(rowSums(jps)==minszam)
    nummemory<<-rbind(nummemory,c(ezek[1],which(jps[ezek[1],])[1],1,1))
    matrixmemory<<-abind(matrixmemory,msudoku,along=3)
    talaldmeg()
  }
}




# egyszerűsíti a megoldás menetét, és meghatározza a kezdeti értékeket a talaldmeg()- hez
msolve<-function(name){
  mer<-Sys.time()
  beolvas(name)
  milehet(msudoku)
  
  minszam<<-min(rowSums(jps)[rowSums(jps)!=0])
  ezek<<-which(rowSums(jps)==minszam)
  nummemory<<-matrix(c(ezek[1],which(jps[ezek[1],])[1],1,1),1,4)
  colnames(nummemory)<<-c("nullhely","tipp","prevind1","prevind2")
  matrixmemory<<-array(msudoku,c(9,9,1))
  
  talaldmeg()
  
  print(msudoku)
  print(Sys.time()-mer)}


#beépített megoldás idő összehasonlításához
beepmegold<-function(game){
  cal<-Sys.time()
  beolvas(game)
  solveSudoku(msudoku)
  print(Sys.time()-cal)
}



#---------#


#valamelyik előre bevitt játék kiválasztása (easy,medium,hard,extreme1,extreme2,extreme3,extreme4,hardest)
sudokujatek<-select.list(game,title = "1:easy, 2:medium, 3:hard, 4:extreme1, 5:extreme2, 6:extreme3, 7:extreme4, 8:hardest")

#sudoku megoldás sajáttal 
msolve(sudokujatek)
plot(as.sudoku(msudoku), col = "DarkGray", par(bg="black"), colGame="#FE4DBD", colSolution="#b241f4", lty= "solid")
#sudoku megoldásbeépítettel # csak idő összehasonlításához
beepmegold(sudokujatek)
