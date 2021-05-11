library(stringr)
library(gridExtra)
library(grid)
dat = read.csv("newShoplist.csv",header=TRUE)
#categories="groceries","health&beauty","hobbies","transport","entertainment","electronics","clothes&acc","work"
#subcategories="veggies","meat","dairy","readyM","bakery","cupboard","yummy","drinks","cleaning","pets","grooming","makeup","treatment","pharmacy","perfume","craft","plants","restaurant","books","events","phones","pc","media","homeEl","car","public","clothes","shoes","jewelery","acc"
shopc<-as.character(levels(factor(dat$shopclass)))

shopty<-numeric(0)
typeshop<-function(class,type,sclass){
etype<-ifelse(class==sclass,as.character(type),0)
as.character(levels(factor(etype))[-1])
}
for (i in 1:length(shopc)){

a<-which(dat$shopclass==shopc[i])
pty<-as.character(levels(factor(dat$shoptype[a])))
shopty<-c(shopty,pty)

}

allclass<-numeric(length(shopc))
lvlY<-levels(factor(dat$year))
l<-0
alltype<-numeric(length(shopty))
mclass<-numeric(length(shopc)+2)
Mclass<-matrix(,nrow=0,ncol=length(shopc)+2)
colnames(Mclass)<-c("year","month",shopc)
mtype<-numeric(length(shopc)+2)
Mtype<-matrix(,nrow=0,ncol=length(shopty)+2)
colnames(Mtype)<-c("year","month",shopty)

priceCal<- function(type,stype,price){
    priceID<-ifelse(type == stype,1,0)
    sum(price*priceID)
    }
monthCal<-function(year,month,stype,price,levelY,levelM,type){

    sh<-ifelse(levelY==year & levelM==month & type==stype,dat$price,0)
    sum(sh)
    }
 
for (i in 1:length(shopc)){

    allclass[i]=priceCal(shopc[i],dat$shopclass,dat$price)
    
    }
for (i in 1:length(shopty)){

    alltype[i]=priceCal(shopty[i],dat$shoptype,dat$price)
    
    }
for(k in 1:length(lvlY)){
     m<-ifelse(lvlY[k]==dat$year,dat$month,0)
    lvlM<-levels(factor(m))[-1]

for(i in 1:length(lvlM)){
mclass[1]=as.integer(lvlY[k])
mclass[2]=as.integer(lvlM[i])
mtype[1]=as.integer(lvlY[k])
mtype[2]=as.integer(lvlM[i])
for(j in 1:length(shopc)){
 mclass[j+2]=monthCal(dat$year,dat$month,dat$shopclass,dat$price,lvlY[k],lvlM[i],shopc[j])


}

 Mclass<-rbind(Mclass,mclass)

for(l in 1:length(shopty)){
 mtype[l+2]=monthCal(dat$year,dat$month,dat$shoptype,dat$price,lvlY[k],lvlM[i],shopty[l])


}

 Mtype<-rbind(Mtype,mtype)
}
} 
write.csv(Mclass,"spesa_mensile.csv")
write.csv(Mtype,"spesa_mensileSubtipi.csv")
Mtype<-rbind(Mtype,c(0,0,alltype) ) 
Mclass<-rbind(Mclass,c(0,0,allclass) ) 
print(Mclass)
 print(Mtype)
rownames(Mclass)<-NULL
rownames(Mtype)<-NULL
dev.off()

pdf("spesaCategoria.pdf",width = 12, height = 8)
l<-length(Mclass[,1])
par(cex.main=2)
par(mar=c(1,1,1,1))
# Pie Chart with Percentages
for (i in 1:l){
    slices <- Mclass[i,3:10] 
    lbls <- shopc
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    Main<-paste("Expenses per Shop Class",Mclass[i,2],"/",Mclass[i,1])
    Sub<-paste("Total: Euro",sum(slices))
    
    pie(slices,labels = lbls, col=rainbow(length(lbls)),init.angle = 40,main=Main, cex=1.2)
    text(1,1,Sub,cex=1.5)
}


dev.off()
pdf("spesaSubtipo.pdf",width = 8, height = 8)
par(cex.main=2)

for (i in 1:length(shopc)){
    a<-which(dat$shopclass==shopc[i])
   typ<-as.character(levels(factor(dat$shoptype[a])))
    Main=paste(shopc[i],"Euros",Mclass[length(Mtype[,1])-1,i+2])
    indi[1]=which(ifelse(colnames(Mtype)==typ[1],1,0)==1)
    indi[2]=which(ifelse(colnames(Mtype)==typ[length(typ)],1,0)==1)
    bars<-Mtype[length(Mtype[,1])-1,indi[1]:indi[2]]
    bp<-barplot(Mtype[length(Mtype[,1])-1,indi[1]:indi[2]], ylim=c(0.0,max(bars)+10),col=rainbow(bars), beside = TRUE,ylab="Expenditure",las=2,cex.names = 1,main=Main)
    text(x=bp, y=bars, labels=bars, pos=3, xpd=NA,cex = 0.8,las=2)
    
}
dev.off()



