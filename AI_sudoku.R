
# Először le kell futtatni az egészet a "----------" vonalig

easy = "9 2 5   3, 43278   ,  89    1,6   3  7 ,2 5   4 9, 1  9   6,3    19  ,   46935 ,7   8 2 4"
medium = "    7 4 1,   3 4  9,3     28 ,8  9   56,         ,65   1  7, 43     8,9  4 8   ,1 2 5    "
hard = "7   4 1  ,    63  7,   8  4 2,1 3    9 , 4     7 , 8    3 4,6 5  8   ,3  69    ,  7 1   5"
extreme1 = "1   68   ,5 9  4 7 , 2 9    3,     2  7, 6     4 ,9  4     ,2    1 8 , 7 2  5 4,   84   9"
extreme2 = "   7   2 , 7   59  ,  4  1 6 , 9   7  2,3       8,6  2   7 , 4 3  1  ,  69   8 , 1   4   "
extreme3 = "   4 28  ,7    9  1,    8  73, 2   79  ,    2    ,  31   6 ,84  1    ,3  2    5,  79 8   "

#beolvassa a megadott játékot
beolvas<-function(sudoku){
msus<<-matrix(0,9,9)
msudoku<<-matrix(0,9,9)
colnames(msudoku)<<-paste("C",c(1:9),sep = "")
rownames(msudoku)<<-paste("R",c(1:9),sep = "")
for(i in 1:9){
msudoku[i,]<<-rbind(as.numeric(unlist(strsplit(unlist(strsplit(easy, ","))[i],NULL))[1:9]))}                                                 
msudoku[is.na(msudoku)]<<-0
#print(msudoku)
}


megold<-function(msudoku){
  m<-Sys.time()
  mstart<<-msudoku
while(sum(msudoku-msus)){
  psol<-array(0,c(3,9,81)) # lehetséges értékek mátrixa
for(i in 1:9){for (j in 1:9){
  psol[1,j,which(msudoku[i,]==0)*9-9+i]<-sum(!sum(msudoku[i,]==j)) # lehetséges értékek sor szerint
  psol[2,j,which(msudoku[,i]==0)+(i-1)*9]<-sum(!sum(msudoku[,i]==j)) # lehetséges értékek oszlop szerint
  if(msudoku[(j-1)*9+i]==0){for(k in 1:9){ #lehetséges értékek kis négyzetek szerint
  psol[3,k,(j-1)*9+i]<-sum(!sum(msudoku[(1+3*(ceiling(i/3)-1)):(3+3*(ceiling(i/3)-1)),(1+3*(ceiling(j/3)-1)):(3+3*(ceiling(j/3)-1))]==k))
  }}}
  ms<<-msudoku}

#elmentem új névvel, hogy össze tudjam hasonlítani
  msus<-msudoku
  
  
#beírja a számot, ha csak 1 lehetséges szám van, ami mind a három szűrő szerint szerepelhet
for(i in 1:81){
  if(sum(colSums(psol[,,i])==3)==1){
    msudoku[i]<-col(psol[,,80])[1,colSums(psol[,,i])==3]
  }}}
  print(msudoku)
  print(Sys.time()-m)}


#----------

#beolvas függvénybe az egyik inputot a 6 közül (most csak easy-re fut le jól, a többire nincs egyértelmű lépés)
beolvas(easy)

#ezt változtatás nélkül lehet futtatni, mert msudoku-nak menti a beolvasás után
megold(msudoku)

#megnézni, hogy milyen új értékeket írt be az eredetihez képest
megold(msudoku)-msudoku



jps<-t(colSums(psol)==3)

for(i in 1:9){for (j in 1:9){
  if(msudoku[(j-1)*9+i]==0){for(k in 1:9){ #lehetséges értékek kis négyzetek szerint
    psol[3,k,(j-1)*9+i]<-sum(!sum(msudoku[(1+3*(ceiling(i/3)-1)):(3+3*(ceiling(i/3)-1)),(1+3*(ceiling(j/3)-1)):(3+3*(ceiling(j/3)-1))]==k))
  }}}}

#genereál sok véletlen mátrixot 

beolvas(easy)
n<-10*sum(msudoku==0)
fit<-rep(0,n)
gen<-array(0,c(9,9,n))
for(k in 1:n){beolvas(easy)
for(i in 1:3){for (j in 1:3){
  m<-msudoku[(1+3*(i-1)):(3+3*(i-1)),(1+3*(j-1)):(3+3*(j-1))]
  if(sum(m==0)>0){a<-min(elem[(1+3*(i-1)):(3+3*(i-1)),(1+3*(j-1)):(3+3*(j-1))][elem[(1+3*(i-1)):(3+3*(i-1)),(1+3*(j-1)):(3+3*(j-1))] %in% which(msudoku==0)])
  m[m==0]<-sample(which(psol[3,,a]==1))
  msudoku[(1+3*(i-1)):(3+3*(i-1)),(1+3*(j-1)):(3+3*(j-1))]<-m
  }
}
} 
  gen[,,k]<-msudoku
  ut<-rep(0,81)
  for(i in 1:81){
    ut[i]<-sum(msudoku[row(msudoku)[i],]==msudoku[i])+sum(msudoku[,col(msudoku)[i]]==msudoku[i])-1
  }
  fit[k]<-sum(ut)
}

# a legjobb mátrixnak visszaadja a a cellánkénti előfordulási értékeket
ez<-gen[,,which(fit==min(fit))[1]]
for(i in 1:81){
  ut[i]<-sum(ez[row(ez)[i],]==ez[i])+sum(ez[,col(ez)[i]]==ez[i])-1
}

#véletlen feltöltéshez kellett
elem<-matrix(1:81,9,9)
elem[(1+3*(i-1)):(3+3*(i-1)),(1+3*(j-1)):(3+3*(j-1))]

sample(which(psol[3,,6]==1))
m21[m21==0]<-sample(which(psol[3,,6]==1))

# melyik helyen melyik számok lehetnek - jobban
jps<-t(colSums(psol)==3)
for(i in 1:9){gn<-which(colSums(jps[1:9,])==1)
              msudoku[which(jps[1:9,gn]==T)]<-gn
            jps[1:9,gn]<-F
}

which(rowSums(colSums(psol)==3)[,1:9])==1


(colSums(psol)==3)[,1:9]



ms<-msudoku

ut<-array(0,c(2,9,81)) # lehetséges értékek mátrixa
for(i in 1:9){for (j in 1:9){
  ut[1,j,which(msudoku[i,]!=0)*9-9+i]<-sum(msudoku[i,]==j) # lehetséges értékek sor szerint
  ut[2,j,which(msudoku[,i]!=0)+(i-1)*9]<-sum(msudoku[,i]==j) # lehetséges értékek oszlop szerint
  # if(msudoku[(j-1)*9+i]==0){for(k in 1:9){ #lehetséges értékek kis négyzetek szerint
  #   psol[3,k,(j-1)*9+i]<-sum(!sum(msudoku[(1+3*(ceiling(i/3)-1)):(3+3*(ceiling(i/3)-1)),(1+3*(ceiling(j/3)-1)):(3+3*(ceiling(j/3)-1))]==k))
  # 
}}

ut<-rep(0,81)
for(i in 1:81){
  ut[i]<-sum(msudoku[row(msudoku)[i],]==msudoku[i])+sum(msudoku[,col(msudoku)[i]]==msudoku[i])-1
}
sum(ut)


  ut[1,j,which(msudoku[i,]!=0)*9-9+i]<-sum(msudoku[i,]==j) # lehetséges értékek sor szerint
  ut[2,j,which(msudoku[,i]!=0)+(i-1)*9]<-sum(msudoku[,i]==j) # lehetséges értékek oszlop szerint
  # if(msudoku[(j-1)*9+i]==0){for(k in 1:9){ #lehetséges értékek kis négyzetek szerint
  #   psol[3,k,(j-1)*9+i]<-sum(!sum(msudoku[(1+3*(ceiling(i/3)-1)):(3+3*(ceiling(i/3)-1)),(1+3*(ceiling(j/3)-1)):(3+3*(ceiling(j/3)-1))]==k))
  # 

w<-t(colSums(ut))


