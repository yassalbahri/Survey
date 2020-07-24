library(RSQLite)
library(irr)
library(rel)
final <- dbConnect(SQLite(), "final.sqlite")
table <- dbReadTable(final, "tab")


evaluations <- c("No similarity", "Slight Similarity", "Almost Identical", "	Identical")

set.seed(1234)
myValues <- sample(1:2000, size=26)
names(myValues) <- myLetters

myValues[match("a", names(myValues))]

table <- as.character(table)

tablegroup[tablegroup == "No similarity"] <- 1
tablegroup[tablegroup == "Prefer not to answer"] <- 3
tablegroup[tablegroup == "Slight Similarity"] <- 2
tablegroup[tablegroup == "Slight Similiraty"] <- 2
tablegroup[tablegroup == "Almost Identical"] <- 4
tablegroup[tablegroup == "Identical"] <- 5


table <-table[, -c(1:2)]
table <- t(table)
table <- table[,-11]


kappafle <- kappam.fleiss(table)

table <- table [(3:12),]
sarow <- table [5,]
t(sarow)

kripp.alpha(table, c("ordinal"))



tablegroup <- table


df <- c("1", "1", "1", "1")
kappam.fleiss(t(df))

pie(table(data), names(table(data)))
pie(presults)

barplot(table$What.is.the.step.by.step.guide.to.invest.in.share.market.in.india.,)

tbl <- with(table, table(table$username))


counts <- table(table$What.is.the.step.by.step.guide.to.invest.in.share.market.in.india.)

tbl <- with(table, table(table[,1]))
barplot(tbl, beside = TRUE, legend = TRUE)


counts <- table[,3]


df <- c(0,	0,	0,	0,	14)
df1 <- c(0,	2,	6,	4,	2)
df2 <- c(0,	0,	3,	5,	6)
df3 <- c(0,	3,	9,	2,	0)
df4 <- c(2,	2,	8,	1,	1)
df5 <- c(7,	7,	0,	0,	0)
df6 <- c(3,	2,	6,	3,	0)
df7 <- c(2,	5,	3,	2,	2)
df8 <- c(6,	5,	2,	1,	0)
df9 <- c(0,	2,	2,	3,	7)



tablew = data.frame(df, df1, df2, df3, df4, df5, df6, df7, df8)
tablew <- t(tablew)

kappam.fleiss(tablew)
kripp.alpha(tablew, c("ordinal"))

#careful: use as.matrix to avoid errors with KRIPP ALPHA
#IF THE DATA IS CATEGORICAL kappam.fleiss gives 0.451
#IF THE DATA IS NUMERICAL kappa.fleiss gives 0.425

kappatest1 <- t(kappatest)
kappam.fleiss(kappatest1)

kappatestalpha <- as.matrix(kappatest)
kripp.alpha(kappatestalpha, c("ordinal"))

#Passed the wikipedia example of fleiss kappa in here changing the format
#(raw data instead of distributed) and obtained the same value (0.21)
#kripp (nominal) also gives the same value (0.21) much higher if ordinal (0.538)


#Try kappafleiss and kripp for 1 subject:
subject <- table[10,]
subject <- as.numeric(subject)
kappam.fleiss(t(subject)) #This gives very low value -0.0714
#I think it may be because data is considered nominal (doesn't matter if it is numeric or character)
kripp.alpha(as.matrix(subject), c("nominal")) #this gives -0.0663 (close to -0.0714)
kripp.alpha(as.matrix(subject), c("ordinal")) #this also gives -0.0663 (close to -0.0714)
#it seems when there is 1 subject the values are the same (low), doesn't matter the ratings





devtools::install_github("jmgirard/agreement")
library(agreement)
cat_per_object(as.matrix(subject), categories = c(1, 2, 3, 4, 5), weighting = "linear")


cor(as.matrix(subject), method = "kendall")










#if df are numerical -> not same values as the wikipedia's example (k= 0.0388)


df <- c("0",	"0",	"0",	"0",	"14")
df1 <- c("0",	"2",	"6",	"4",	"2")
df2 <- c("0",	"0",	"3",	"5",	"6")
df3 <- c("0",	"3",	"9",	"2",	"0")
df4 <- c("2",	"2",	"8",	"1",	"1")
df5 <- c("7",	"7",	"0",	"0",	"0")
df6 <- c("3",	"2",	"6",	"3",	"0")
df7 <- c("2",	"5",	"3",	"2",	"2")
df8 <- c("6",	"5",	"2",	"1",	"0")
df9 <- c("0",	"2",	"2",	"3",	"7")


#if df are categorial -> not same values as the wikipedia's example (k= 0.0388 same as numerical)

#weighted kappa only available for 2 raters?
kappam.fleiss(table, exact = FALSE, detail = FALSE)




spi(data = table, weight = c("linear"), conf.level = 0.95)

ckap(data = table, weight = c("linear"),
     std.err = c("Fleiss", "Cohen"), conf.level = 0.95, R = 0)

kripp.alpha(table, c("ordinal"))

library(multilevel)
rwg(subject, subje)


?rwg
data(lq2002)
RWGOUT<-rwg(lq2002$LEAD,lq2002$COMPID)
RWGOUT[1:10,]
summary(RWGOUT)

tablew <- tablew

samegroup <- as.numeric(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

tablegroup <- as.data.table(table)

#--

final <- dbConnect(SQLite(), "final.sqlite")
table <- dbReadTable(final, "tab")

#table <- table[, -c(1:2)]

table <- t(table)
table <- table [(3:12),]
table <- table[, (1:16)]

tablegroup <- table

tablegroup[tablegroup == "No similarity"] <- 1
tablegroup[tablegroup == "Prefer not to answer"] <- 3
tablegroup[tablegroup == "Slight Similarity"] <- 2
tablegroup[tablegroup == "Slight Similiraty"] <- 2
tablegroup[tablegroup == "Almost Identical"] <- 4
tablegroup[tablegroup == "Identical"] <- 5

tablegroup <- as.matrix(tablegroup)
tablegroup <- as.numeric(tablegroup[,3:10])

tablegroup <- (t(tablegroup))
tablegroup$AVG <- rowMeans(TRIAL)


kappam.fleiss(tablegroup, exact = TRUE, detail = FALSE)
#---



tablegroup <- cbind(samegroup, tablegroup)

tablegroup <- as.data.frame(tablegroup)


rwg(tablegroup$AVG, tablegroup$samegroup)



#PARA KAPPA FLEISS ECUACION MANUAL
srow <- tablegroup[as.integer(1),]
sroww <- as.data.frame(table(srow))
table(srow)

PKappa <- sroww$Freq *  sroww$Freq
PKappa1 <- sum(PKappa)

PKappa2 <- (PKappa1) / (nrow(table) * (nrow(table)-1))

PKappa2 <- (PKappa1) / (n_table * (n_table-1))

kripp.alpha(as.matrix(tablegroup), c("ordinal"))
kappa