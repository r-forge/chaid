
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


set.seed(222)

dat <- data.frame(x = gl(2, 50), y = gl(5, 20))[sample(1:100,75),]
xtmp <- xtabs( ~ dat$x + dat$y)   #teilweise zu geringe Zellhäufigkeit, daher Simulation

set.seed(290875)
a4 <- logchisq.test(xtmp)[1]
set.seed(290875)
b4 <- log(chisq.test(xtmp, correct=FALSE, 
                     simulate.p.value=TRUE, B=9999)$p.value)
stopifnot(isequal(a4, b4))


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



### test fuer geordnete Faktoren
x <- ordered(rep(LETTERS[1:4], 4), levels = rev(LETTERS[1:4]))
stopifnot(all.equal(mergex(x, 1:4), x))
mergex(x, c(1, 1, 2, 3))


### Tests for function logchisq.test()
# Examples where p-value is not simulated

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

# Examples where p-value is simulated

set.seed(222)

dat <- data.frame(x = gl(2, 50), y = gl(5, 20))[sample(1:100,75),]
xtmp <- xtabs( ~ dat$x + dat$y)   

set.seed(290875)
a4 <- logchisq.test(xtmp)[1]
set.seed(290875)
b4 <- log(chisq.test(xtmp, correct=FALSE, 
                     simulate.p.value=TRUE, B=9999)$p.value)
stopifnot(isequal(a4, b4))


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



ctrl <- chaid_control(alpha2 = 0.05, alpha3 = -1, alpha4 = 0.05,
                          minsplit = 20, minbucket = 7, minprob = 0.01)


mlev <- step2(response=kredit$kredit, x=kredit$moral, 
              weights=rep(1,1000), index = 1:nlevels(kredit$moral), ctrl)$mlev
stopifnot(isequal(mlev, c(3,4)))

# response <- kredit$kredit
# x <- kredit$moral
# weights <- rep(1,1000)
# xytab <- xtabs(weights ~ x + response)


mlev <- step2(response=kredit$kredit, x=kredit$verw, 
              weights=rep(1,1000), index = 1:nlevels(kredit$verw), ctrl)$mlev
index <- 1:nlevels(kredit$verw)              
index <- mergelevels(index, mlev)

mlev <- step2(response=kredit$kredit, x=kredit$verw, 
              weights=rep(1,1000), index = index, ctrl)$mlev
index <- mergelevels(index, mlev)             
#index
mlev <- step2(response=kredit$kredit, x=kredit$verw, 
              weights=rep(1,1000), index = index, ctrl)$mlev
index <- mergelevels(index, mlev)
#index
#?# stopifnot(isequal(index, c(1,2,3,4,5,5,6,7,5,6)))
#?# stopifnot(isequal(mlev, c(5,8)))

# return NULL, because there are only two categories in x-Variable
mlev <- step2(response=auto$lease, x=auto$probe, 
                weights=rep(1,(length(auto$modell))), ctrl)$mlev
stopifnot(isequal(mlev, NULL))


#?# warum werden hier die Kategorien 1,2 bzw 3,4 von x nicht zusammengelegt?
# x <- gl(4, 200)
# response <- gl(2,400)
# mlev <- step2(response=response, x=x, 
#               weights=rep(1,1000), index = 1:nlevels(1000), ctrl)


########################################################################
#?# tests for step4internal ??

# Information about the Index from step 2
# mlev <- step2(response=kredit$kredit, x=kredit$verw, 
#               weights=rep(1,1000), index = 1:nlevels(kredit$verw), ctrl)$mlev              
# index <- mergelevels(index, mlev)
# 
# tmp <- step4internal(response=kredit$kredit, x=kredit$verw, 
#               weights=rep(1,1000), index = index)
# 
# tmp_tab <-  xtabs( ~kredit$kredit + mergex(kredit$verw, index))
# tmp_manual <- chisq.test(tmp_tab, correct = FALSE)$statistic 
# 
# stopifnot(isequal(tmp_manual, attr(tmp, "Chisq")))

#?# wie komme ich an den Chisq-Wert aus step4internal tmp[2] funktioniert nicht?


