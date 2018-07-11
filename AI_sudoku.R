


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
msudoku[i,]<<-rbind(as.numeric(unlist(strsplit(unlist(strsplit(sudoku, ","))[i],NULL))[1:9]))}                                                 
msudoku[is.na(msudoku)]<<-0
#print(msudoku)
}

milehet<-function(msudoku){
psol<<-array(0,c(3,9,81)) # lehetséges értékek mátrixa
for(i in 1:9){for (j in 1:9){
  psol[1,j,which(msudoku[i,]==0)*9-9+i]<<-sum(!sum(msudoku[i,]==j)) # lehetséges értékek sor szerint
  psol[2,j,which(msudoku[,i]==0)+(i-1)*9]<<-sum(!sum(msudoku[,i]==j)) # lehetséges értékek oszlop szerint
  if(msudoku[(j-1)*9+i]==0){for(k in 1:9){ #lehetséges értékek kis négyzetek szerint
    psol[3,k,(j-1)*9+i]<<-sum(!sum(msudoku[(1+3*(ceiling(i/3)-1)):(3+3*(ceiling(i/3)-1)),(1+3*(ceiling(j/3)-1)):(3+3*(ceiling(j/3)-1))]==k))
  }}}}}


megold<-function(msudoku){
  m<-Sys.time()
  mstart<<-msudoku
while(sum(msudoku-msus)){
  milehet(msudoku)
  
#elmentem új névvel, hogy össze tudjam hasonlítani
  msus<<-msudoku
  
#beírja a számot, ha csak 1 lehetséges szám van, ami mind a három szűrő szerint szerepelhet
for(i in 1:81){
  if(sum(colSums(psol[,,i])==3)==1){
    msudoku[i]<<-col(psol[,,80])[1,colSums(psol[,,i])==3]
  }}}
  print(msudoku)
  print(Sys.time()-m)}




#genereál sok véletlen mátrixot 

generate<-function(easy){
  beolvas(easy)
  zerohelyek<<-which(msudoku==0)
  milehet(msudoku)
  n<<-10*sum(msudoku==0)
  fit<<-rep(0,n)
  elem<-matrix(1:81,9,9)
  gen<<-array(0,c(9,9,n))
  for(k in 1:n){beolvas(easy)
    for(i in 1:3){for (j in 1:3){
      m<<-msudoku[(1+3*(i-1)):(3+3*(i-1)),(1+3*(j-1)):(3+3*(j-1))]
      if(sum(m==0)>0){a<<-min(elem[(1+3*(i-1)):(3+3*(i-1)),(1+3*(j-1)):(3+3*(j-1))][elem[(1+3*(i-1)):(3+3*(i-1)),(1+3*(j-1)):(3+3*(j-1))] %in% which(msudoku==0)])
      m[m==0]<<-sample(which(psol[3,,a]==1))
      msudoku[(1+3*(i-1)):(3+3*(i-1)),(1+3*(j-1)):(3+3*(j-1))]<<-m
      }
    }
    } 
    gen[,,k]<<-msudoku
    ut<<-rep(0,81)
    for(i in zerohelyek){#soronként, oszloponként megszámolja mennyi van az adott számból
      ut[i]<<-sum(msudoku[row(msudoku)[i],]==msudoku[i])+sum(msudoku[,col(msudoku)[i]]==msudoku[i])-1
    }
    fit[k]<<-sum(ut)
  }
  
  # a legjobb mátrixnak visszaadja a a cellánkénti előfordulási értékeket
  ez<<-gen[,,which(fit==min(fit))[1]]
  ut1<<-rep(0,81)
  for(i in zerohelyek){
    ut1[i]<<-sum(ez[row(ez)[i],]==ez[i])+sum(ez[,col(ez)[i]]==ez[i])-1
  }
  beolvas(easy) # hogy az eredeti kitöltetlen mátrix maradjon ezzela  névvel
}

# felcseréli a vektor két elemét
felcsere <- function(regisorrend,ezt,erre) {
  sorrend<<-regisorrend
  #ujsor<<-sorrend
  sorrend[c(ezt,erre)] <<- sorrend[c(erre,ezt)]
  #sorrend<<-ujsor
} 

sorrend





#ezelőtt futtasd generate-et
#felcserél két elemet kisnégyzeten belül 
valaszt<-function(){
  #ut2<<-ut1
  eze<<-ez # hogy a végén lássam mi változott
  elem<<-matrix(1:81,9,9)
  worst<<-sample(which(ut1==max(ut1)),1) # kiválaszt a legrosszabbak közül 1-et véletlenül
  ro<<-ceiling(row(elem)[worst]/3)
  co<<-ceiling(col(elem)[worst]/3)
  toligr<<-(1+3*(ro-1)):(3+3*(ro-1))
  toligc<<-(1+3*(co-1)):(3+3*(co-1))
  
  # for(i in zerohelyek){
  #   ut2[i]<<-sum(ez[row(ez)[i],]==ez[i])+sum(ez[,col(ez)[i]]==ez[i])-1
  # }
  
  m<<-ez[toligr,toligc]
  csere<<-ez[worst] #m[which(elem[toligr,toligc]==worst)] # ezt a számot akarom kicserélni
  sorrend<<-m[which(msudoku[toligr,toligc]==0)]# ebben a sorrendben írta be vélelenül
  ezt<<-which(sorrend==csere) # amelyiket kiválasztottam mint legrosszabb
  minfit()
  
  felcsere(regisorrend,ezt,errem)
  
  ez[toligr,toligc][msudoku[toligr,toligc]==0]<<-sorrend # beírja az új sorrendben a számokat
  
  for(i in zerohelyek){
    ut1[i]<<-sum(ez[row(ez)[i],]==ez[i])+sum(ez[,col(ez)[i]]==ez[i])-1
  }
  #print(ez-eze)
}


sorrend
ezt

#forgatós
valaszt<-function(){
  eze<<-ez # hogy a végén lássam mi változott
  elem<<-matrix(1:81,9,9)
  worst<<-sample(which(ut1==max(ut1)),1) # kiválaszt a legrosszabbak közül 1-et véletlenül
  ro<<-ceiling(row(elem)[worst]/3)
  co<<-ceiling(col(elem)[worst]/3)
  toligr<<-(1+3*(ro-1)):(3+3*(ro-1))
  toligc<<-(1+3*(co-1)):(3+3*(co-1))
  
  for(i in zerohelyek){
    ut1[i]<<-sum(ez[row(ez)[i],]==ez[i])+sum(ez[,col(ez)[i]]==ez[i])-1
  }
  
  m<<-ez[toligr,toligc]
  csere<<-ez[worst] #m[which(elem[toligr,toligc]==worst)] # ezt a számot akarom kicserélni
  sorrend<<-m[which(msudoku[toligr,toligc]==0)]# ebben a sorrendben írta be vélelenül
  sorrend<<-sorrend[c(length(sorrend),1:(length(sorrend)-1))]
  ez[toligr,toligc][msudoku[toligr,toligc]==0]<<-sorrend # beírja az új sorrendben a számokat
  
  for(i in zerohelyek){
    ut1[i]<<-sum(ez[row(ez)[i],]==ez[i])+sum(ez[,col(ez)[i]]==ez[i])-1
  }
}


#legkisebb fit
minfit<-function(){
  fitertek<<-rep(0,(length(sorrend)))
  tarts<<-ez
  regisorrend<<-sorrend
  for(j in which(regisorrend!="")[-ezt]){
    erre<<-j
    ez<<-tarts
    felcsere(regisorrend,ezt,erre)
    ez[toligr,toligc][msudoku[toligr,toligc]==0]<<-sorrend # beírja az új sorrendben a számokat
    
    for(i in zerohelyek){
      ut1[i]<<-sum(ez[row(ez)[i],]==ez[i])+sum(ez[,col(ez)[i]]==ez[i])-1
    }
    fitertek[j]<<-sum(ut1)}
    errem<<-order(fitertek)[2]
}




#----------

#beolvas függvénybe az egyik inputot a 6 közül (most csak easy-re fut le jól, a többire nincs egyértelmű lépés)
beolvas(easy)

#ezt változtatás nélkül lehet futtatni, mert msudoku-nak menti a beolvasás után
megold(msudoku)

#megnézni, hogy milyen új értékeket írt be az eredetihez képest
megold(msudoku)-msudoku



jps<-t(colSums(psol)==3)


generate(easy)

# nszam<-length(zerohelyek)
# fitrossz<-sum(ut1)
while(sum(ut1) > length(zerohelyek)){
  Sys.sleep(0.2)
  valaszt()
  print(sum(ut1)-length(zerohelyek))
}

t<-system.time()
while(sum(ut1) > 60){
  valaszt()
}
system.time()-t

Sys.sleep(0.7)
ut1

s<-ut1
valaszt()
ut1-s



length(sorrend)
ms

worst<-which(ut1==max(ut1))[1]
msudoku[33]
for(i in 1:3){for (j in 1:3){
  
}
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
















elem<-matrix(1:81,9,9)
worst<-sample(which(ut1==max(ut1)),1) # kiválaszt a legrosszabbak közül 1-et véletlenül
ro<-ceiling(row(elem)[worst]/3)
co<-ceiling(col(elem)[worst]/3)
toligr<-(1+3*(ro-1)):(3+3*(ro-1))
toligc<-(1+3*(co-1)):(3+3*(co-1))
m<-ez[toligr,toligc]
csere<-ez[worst]#m[which(elem[toligr,toligc]==worst)] # ezt a számot akarom kicserélni
sorrend<-m[which(msudoku[toligr,toligc]==0)]# ebben a sorrendben írta be vélelenül
ezt<-which(sorrend==csere) # amelyiket kiválasztottam mint legrosszabb
#erre<<-sample(which(sorrend!=csere),1) # egy másik véletlenszerűen választott
erre<-order(ut1[elem[toligr,toligc][msudoku[toligr,toligc]==0]],decreasing = T)[2]
felcsere(sorrend,ezt,erre)
ez[toligr,toligc][msudoku[toligr,toligc]==0]<-sorrend # beírja az új sorrendben a számokat

for(i in zerohelyek){
  ut1[i]<<-sum(ez[row(ez)[i],]==ez[i])+sum(ez[,col(ez)[i]]==ez[i])-1
}




