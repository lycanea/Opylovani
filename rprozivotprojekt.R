#Nech?? existuje hypotetick? populace rostlin s hypotetickou strukturou a hypotetick? opylova?? s hypotetick?mi rozhodovac?mi vlastnostmi 
#Nech?? se tento opylova?? pohybuje v populaci od rostliny k rostlin?? na z?klad?? sv?ho rozhodnut? 
#Nech?? je v?stupem graf pohybu opylova??e zv?razn??n? ??ervenou lini? 



opylovac <- function(nevracisezpet,maxpocetpreletu){
  #vygeneruje dataset s n?hodne umistenymi hodnotami (1 nebo 0) ve sloupci "pocet" - tedy udela to nahodne mista v array s "rostlinami"
  x <- c((rep(seq(1,1,1),times=8)),(rep(seq(2,2,1),times=8)),(rep(seq(3,3,1),times=8)),(rep(seq(4,4,1),times=8)),(rep(seq(5,5,1),times=8)),(rep(seq(6,6,1),times=8)),(rep(seq(7,7,1),times=8)),(rep(seq(8,8,1),times=8)))
  x
  length(x)
  y<- rep(seq(1,8,1),times=8)
  pocet <- sample(x=c(0,0,0,0,1,1,1),64,replace = T)
  pocet
  id <-seq(1:64)
  datakytky <- data.frame(id=c(id),x=c(x),y=c(y),pocet=c(pocet))
  datakytky
  
    
    #vybere z puvodniho datasetu subdataset, kde je hodnota v sloupci "pocet" ==1 (tedy jen ty mista kde rostliny jsou)
    kdekytkaje <- datakytky[datakytky$pocet==1,]
    kdekytkaje
   
     #nahodne vybere jeden radek ze sloupce "pocet" (tedy 1. rostlinu na kterou opylovac prileti)
    prvnirostlina <- kdekytkaje[kdekytkaje$id==sample(kdekytkaje$id,size=1),]
    prvnirostlina
   
     #vytvori graf array
    plot(datakytky$x,datakytky$y, cex=datakytky$pocet) 
    
    #v array obarvi 1. nahodne vybranou navstivenou rostlinu na cerveno
    points(y=prvnirostlina$y,x=prvnirostlina$x,col="red",cex=prvnirostlina$pocet)
   
     #odsud necht se to opakuje
    
    #vytvori novy sloupec "kdeuzbyl" 
    kdeuzbyl <-rep(x=0,length.out=length(kdekytkaje$id))
    
    #vytvori novy dataset z kdekytkaje a prida do nej sloupec "kdeuzbyl"
    kytkybezprvni <- data.frame(kdeuzbyl=kdeuzbyl,id=kdekytkaje$id,x=kdekytkaje$x,y=kdekytkaje$y,pocet=kdekytkaje$pocet)

    kytkysprvni <- kytkybezprvni[kytkybezprvni$id==prvnirostlina$id,]
    
    #do kytkybezprvni prida do radku prvnirostlina do sloupce "kdeuzbyl" hodnotu 10 - proto?e tam opylovac uz byl 
    kytkybezprvni[kytkybezprvni$id==prvnirostlina$id,"kdeuzbyl"]<-nevracisezpet
   
     #ted musi vybrat druhou rostlinu na zaklade vzdalenosti, jestli na ni jiz byl a popripade na zaklade dalsich promenych
   
     #vytvori dataframe, kde budou odecteny souradnice od "kytkybezprvni" z "prvnirostlina" 
    druhakytka <- data.frame(id=kytkybezprvni$id,x=(kytkybezprvni$x-prvnirostlina$x),y=(kytkybezprvni$y-prvnirostlina$y),pocet=kytkybezprvni$pocet)

    #vypocita vzdalenost v absolutni hodnote od "prvnirostlina"     
    vzdalenost <- data.frame(vzdalenost = (abs(druhakytka$x))+(abs(druhakytka$y)),kdeuzbyl=(kytkybezprvni$kdeuzbyl),id=kytkybezprvni$id,x=kytkybezprvni$x,y=kytkybezprvni$y,pocet=kytkybezprvni$pocet)
    
    #vytvori sloupec "tamnechci" kam secte "vzdalenost" a "kdeuzbyl" popripade zohledni jine parametry
    tamnechci <- data.frame(tamnechci=((vzdalenost$vzdalenost)+(vzdalenost$kdeuzbyl))/(vzdalenost$pocet),vzdalenost = (abs(druhakytka$x))+(abs(druhakytka$y)),kdeuzbyl=(kytkybezprvni$kdeuzbyl),id=kytkybezprvni$id,x=kytkybezprvni$x,y=kytkybezprvni$y,pocet=kytkybezprvni$pocet)
    
    #seradi "tamnechci" podle sloupce "tamnechci" 
    serad <- tamnechci[order(tamnechci$tamnechci),]
    
    #ze "serad" vybere 2.kytku - rostlinu, ktera ma nejmensi hodnotu tam nechci 
    druhakytkaaa <- serad[1,]
    
    #vytvori linii od 1. k 2. rostline na zaklade vyberu opylovace
    lines(x=c(prvnirostlina$x,druhakytkaaa$x),y=c(prvnirostlina$y,druhakytkaaa$y),col="red") 
    #obarvi 2.rostlinu na cerveno
    points(y=druhakytkaaa$y,x=druhakytkaaa$x,col="red")
    
   
    
    tamnechcidalsi <- tamnechci
    x <- 1
    print(prvnirostlina)
    print(druhakytkaaa)
 repeat{
      predchoziserad <-tamnechcidalsi[order(tamnechcidalsi$tamnechci),]
      predchozi <- predchoziserad[1,]
      kytkybezpredchozi <- predchoziserad
      kytkybezpredchozi[kytkybezpredchozi$id==predchozi$id,"kdeuzbyl"]<-kytkybezpredchozi[kytkybezpredchozi$id==predchozi$id,"kdeuzbyl"]+nevracisezpet
      dalsikytka <- data.frame(id=kytkybezpredchozi$id,x=(kytkybezpredchozi$x-predchozi$x),y=(kytkybezpredchozi$y-predchozi$y),pocet=kytkybezpredchozi$pocet)
      vzdalenostdalsi <- data.frame(vzdalenost = (abs(dalsikytka$x))+(abs(dalsikytka$y)),kdeuzbyl=(kytkybezpredchozi$kdeuzbyl),id=kytkybezpredchozi$id,x=kytkybezpredchozi$x,y=kytkybezpredchozi$y,pocet=kytkybezpredchozi$pocet)
      tamnechcidalsi <- data.frame(tamnechci=((vzdalenostdalsi$vzdalenost)+(vzdalenostdalsi$kdeuzbyl))/(vzdalenostdalsi$pocet),vzdalenost = (abs(vzdalenostdalsi$x))+(abs(vzdalenostdalsi$y)),kdeuzbyl=(vzdalenostdalsi$kdeuzbyl),id=vzdalenostdalsi$id,x=vzdalenostdalsi$x,y=vzdalenostdalsi$y,pocet=vzdalenostdalsi$pocet)
      seraddalsi <- tamnechcidalsi[order(tamnechcidalsi$tamnechci),]
      dalsi <- seraddalsi[1,]
      lines(x=c(predchozi$x,dalsi$x),y=c(predchozi$y,dalsi$y),col="red") 
      points(y=dalsi$y,x=dalsi$x,col="red")
      print(prvnirostlina)
      print(dalsi)
      x = x+1
      if (x == maxpocetpreletu){
        break
      }
}
}


opylovac(maxpocetpreletu=10,nevracisezpet=7)  
#konec

    
#odpad    