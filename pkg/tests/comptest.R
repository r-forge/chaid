
library("CHAID")

attach(asNamespace("CHAID"))

##Beispiele in denen der p-Wert nicht simuliert wird

load("orders.Rda")

xtmp <- xtabs(orders$FREQ~ orders$RESP + orders$AGE)
a1 <- logchisq.test(xtmp)[1]
b1 <- log(chisq.test(xtmp, correct = FALSE)$p.value)
stopifnot(isequal(a1, b1))

xtmp <- xtabs(orders$FREQ~ orders$RESP + orders$BUSIVOL)
a2 <- attr(logchisq.test(xtmp), "Chisq")
b2 <- chisq.test(xtmp, correct = FALSE)$statistic
stopifnot(isequal(a2, b2))

load("auto.Rda")

xtmp <- xtabs(~ auto$modell + auto$lease)
a3 <- logchisq.test(xtmp)[1]
b3 <- log(chisq.test(xtmp, correct=FALSE)$p.value)
stopifnot(isequal(a3, b3))


#Daten generieren

set.seed(222)

dat <- data.frame(x = gl(2, 50), y = gl(5, 20))[sample(1:100,75),]
xtmp <- xtabs( ~ dat$x + dat$y)   #teilweise zu geringe Zellhäufigkeit, daher Simulation

set.seed(290875)
a4 <- logchisq.test(xtmp)[1]
set.seed(290875)
b4 <- log(chisq.test(xtmp, correct=FALSE, 
                     simulate.p.value=TRUE, B=9999)$p.value)
stopifnot(isequal(a4, b4))


#########################################################################

# die Funktion mergelevels Ã¼berprÃ¼ft nicht, ob die Variable ordinal 
# oder nominal ist, daher kann man bei ordinalen Variablen auch Kategorien 
# zusammenlegen, die nicht nebeneinander liegen
# ist das mit Absicht so?

# die Fuktion mergex hingegen bricht ab, wenn man versucht bei einer 
# ordinalen Variable Kategorien zusammenzulegen, die nicht nebeneinander liegen

#############################################################################

### Tests with data kredit for
### function mergelevels
### function splitlevels
### function mergex

load("kredit.Rda")

### merge orginal levels 1 and 2 (so index levels 2 and 3)
### (orginal levels 0-4, index levels 1-5)

km <- kredit$moral
km_index <- c(1:length(levels(km)))

km1 <- mergelevels(km_index, c(2, 3))
stopifnot(isequal(km1, c(1,2,2,3,4))) 

### merge first original level with the merged categories
km2 <- mergelevels(km1, c(2, 1))
stopifnot(isequal(km2, c(1,1,1,2,3)))

### remove first orginal level from merged categories
km5 <- splitlevels(km2, 1, 1)
stopifnot(isequal(km5, c(2,1,1,3,4)))

### remove second orginal level from merged categories
km3 <- splitlevels(km2, 1, 2)
stopifnot(isequal(km3, c(1,2,1,3,4)))

### remove third orginal level from merged categories
km4 <- splitlevels(km2, 1, 3)
stopifnot(isequal(km1, c(1,2,2,3,4)))


### actually merge factor levels
# km (ordinal): merge categories 0,1  (the two first)
 
km <- kredit$moral
km_index <- c(1:length(levels(km)))
km_merge_index <-  mergelevels(km_index,c(1,2))

km_merged <- mergex(km, km_merge_index)   
#km_merged

tab_km <- xtabs(~ km + km_merged)
#tab_km

# Kontingenztafel, manuell erstellt nach den HÃ¤ufigkeiten, die in den 
# (zusammengelegten) Kategorien sein mÃ¼ssen
tab_km_manual <- matrix (nrow=5, ncol=4, byrow=TRUE, 
            c(40,0,0,0,  49,0,0,0,  0,530,0,0,  0,0,88,0,  0,0,0,293))
            
stopifnot(isequal(tab_km_manual, tab_km))  

# kl (nominal): merge categories 1,3

kl <- kredit$laufkont
kl_index <- c(1:length(levels(kl)))
kl_merge_index <- mergelevels(kl_index, c(1,3))

kl_merged <- mergex(kl, kl_merge_index)

tab_kl <- xtabs(~ kl + kl_merged)
#tab_kl

tab_kl_manual <- matrix (nrow=4, ncol=3, byrow=TRUE, 
      c(274,0,0,  0,269,0,  63,0,0,  0,0,394))

stopifnot(isequal(tab_kl_manual, tab_kl))


# gibt es eine besser MÃ¶glichkeit zu prÃ¼fen, ob mergex() das richtige tut?
# also nicht Ã¼ber den Vergeich der Kontingenztafeln

# einfaches Beispiel simulieren und dann Datenvektor vergleichen?

### test fuer geordnete Faktoren
x <- ordered(rep(LETTERS[1:4], 4), levels = rev(LETTERS[1:4]))
stopifnot(all.equal(mergex(x, 1:4), x))
mergex(x, c(1, 1, 2, 3))
