
# Elõször le kell futtatni az egészet a "----------" vonalig

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
colnames(msudoku)<<-c(1:9)
rownames(msudoku)<<-c(1:9)
for(i in 1:9){
msudoku[i,]<<-rbind(as.numeric(unlist(strsplit(unlist(strsplit(sudoku, ","))[i],NULL))[1:9]))}                                                 
msudoku[is.na(msudoku)]<<-0
print(msudoku)}


megold<-function(msudoku){
  mstart<<-msudoku
while(sum(msudoku-msus)){
  psol<-array(0,c(3,9,81)) # lehetséges értékek mátrixa
for(i in 1:9){for (j in 1:9){
  psol[1,j,which(msudoku[i,]==0)*9-9+i]<-sum(!sum(msudoku[i,]==j)) # lehetséges értékek sor szerint
  psol[2,j,which(msudoku[,i]==0)+(i-1)*9]<-sum(!sum(msudoku[,i]==j)) # lehetséges értékek oszlop szerint
  if(msudoku[(j-1)*9+i]==0){for(k in 1:9){ #lehetséges értékek négyzetek szerint
  psol[3,k,(j-1)*9+i]<-sum(!sum(msudoku[(1+3*(ceiling(i/3)-1)):(3+3*(ceiling(i/3)-1)),(1+3*(ceiling(j/3)-1)):(3+3*(ceiling(j/3)-1))]==k))
  }}}}

#elmentem új névvel, hogy össze tudjam hasonlítani
  msus<-msudoku
#beírja a számot, ha csak 1 lehetséges szám van, ami mind a három szûrõ szerint szerepelhet
for(i in 1:81){
  if(sum(colSums(psol[,,i])==3)==1){
    msudoku[i]<-col(psol[,,80])[1,colSums(psol[,,i])==3]
  }}}
  print(msudoku)}


#----------

#beolvas függvénybe az egyik inputot a 6 közül (most csak easy-re fut le jól, a többire nincs egyértelmû lépés)
beolvas(easy)

#ezt változtatás nélkül lehet futtatni, mert msudoku-nak menti a beolvasás után
megold(msudoku)

#megnézni, hogy milyen új értékeket írt be az eredetihez képest
megold(msudoku)-msudoku

