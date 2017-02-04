library(cwhmisc)

fn.set <- c("+","-","div.prot","*")
term.set <- c("x","samp")

#---------------------------------------------------
#program generating function

node1 <- NA
node2A <- NA
node2B <- NA
termAA <- NA
termAB <- NA
termBA <- NA
termBB <- NA

node1 <- sample(fn.set,1)

node2A.pop <- sample(c("fn.set","term.set"),1)

if(node2A.pop=='fn.set'){
  node2A <- sample(fn.set,1)
  #if node 2A is from the function set then we know the next nodes must
  # be from the terminal set
  termAA <- sample(term.set,1)
  if(termAA=='samp'){termAA=sample(c(-5:5),1)}
  termAB <- sample(term.set,1)
  if(termAB=='samp'){termAB=sample(c(-5:5),1)}
}else{
  node2A=sample(term.set,1)
  if(node2A=='samp'){node2A=sample(c(-5:5),1)}
}

node2B.pop <- sample(c("fn.set","term.set"),1)

if(node2B.pop=='fn.set'){
  node2B <- sample(fn.set,1)
  termBA <- sample(term.set,1)
  if(termBA=='samp'){termBA=sample(c(-5:5),1)}
  termBB <- sample(term.set,1)
  if(termBB=='samp'){termBB=sample(c(-5:5),1)}
}else{
  node2B=sample(term.set,1)
  if(node2B=='samp'){node2B=sample(c(-5:5),1)}
}

#now organize the results as a list tree for visual effects
p <- list(node1,c(node2A,node2B),c(termAA,termAB,termBA,termBB))

#-----------------------------------------------------

targ.fun <- function(z){(z*z)+z+1}

fit.vals <- seq(-1,1,by=0.1)
#I'm just going to change fit values so I don't have to worry about 
# protected division
fit.vals[which(fit.vals==0)] <- 0.001

fit.targ<-unlist(lapply(fit.vals,FUN=targ.fun))

fn1 <- function(x){eval(parse(text="x+1"))}
fn2 <- function(x){eval(parse(text="(x*x)+1"))}
fn3 <- function(x){eval(parse(text="return(2)"))}
fn4 <- function(x){eval(parse(text="x"))}

fitness <- function(prog){
  fit.prog <- unlist(lapply(fit.vals,FUN=prog))
  fit <- sum((fit.targ - fit.prog)^2)
  return(fit)
}

program.fitness <- unlist(lapply(list(fn1,fn2,fn3,fn4),fitness))
probs <- c(.5,.3,.15,.05)

ranks <- rank(program.fitness)

probs.eval <- probs[ranks]

#sample 4 times from the list of programs above but weight the sampling by fitness
sample(1:4,4,probs=probs.eval)

#result = 1,1,1,3

#now for each program selected to make it through probabilistically decide if it 
# should be reproduced, crossed-over, or mutated
sample(c('reproduction','cross-over','mutation'),4,replace=T,prob=c(.5,.25,.25))

#result = reproduction, mutation, reproduction, mutation

#the first program gets reproduced so it's the same as it was last time
fn1.1 <- function(x){eval(parse(text="x+1"))}

#--------------------------------------------------------------------
#the second program through (which is also program 1 btw) gets mutated
# for mutation we need to figure out which node from the program gets 
# mutated and what it gets replaced with

#first we pick which node gets mutated
sample(c(1:3),1)

#in the current example it was 2

#next we have to figure out what the 2nd node gets replaced with
# note this part is really iffy....I know we need to generate a new
# program just as we did for the start but I'm not sure if there are
# any rules about which part of this program we use for the mutation
# That is, the program we generate will have a minimum of 3 nodes (1 parent and 2 children) and
# a maximum of 7 nodes(four terminal nodes that map back to a parent node and each parent node map back to the same
# starting node)...I'm going to assume that once we generate the new program that we can 
# sample any of the nodes from that program to use in our mutation

#running the original function generator I get a new program:

# 4*(-3 div.prot(0)) = 4 because of the protected division operator
#
# this program can be written in tree structure

#         *
#       /  |
#      4   %
#         / |
#       -3  0

# Again, not sure if this is really kosher...but the protected division operator with 0 in 
# the denominator always evaluated to 1 so this program is the same as 4*1

# I'm going to consider this a program with 3 nodes and sample from 1:3

sample(c(1:3),1)

#in this case I get 1 so I'm going to swap this whole program in 


#so the 2nd node from our first program gets replaced with our entire new program, creating

fn2.1 <- function(x){eval(parse(text="return(4)"))}

#note here that the mutation has probably made the fit worse

#--------------------------------------------------------------------


#--------------------------------------------------------------------
#the third program selected for the 1st generation is again
# x+1 and the third random sample from our (reproduction,mutation,cross-over)
# menu was reproduction so this function again just persists to the next 
# generation

fn3.1 <- function(x){eval(parse(text="x+1"))}

#--------------------------------------------------------------------

#--------------------------------------------------------------------
#the final program in generation 1 is the original program number 3
# and the chosen operation is mutation
# ...so the program x gets mutated

#generate a new program and pick a node in that program
# the new program generated was 5-(x+x)

#sample the new program...which has 5 nodes
sample(c(1:5),1)

#we get the result 3 corresponding to the node 

#            +
#          /  \
#         x   x

#the original function evaluated to just x...but the tree structure was a bit more complicated:

#             *
#            / \
#           x   -
#              / \
#             -1  -2

# so the original had 5 nodes. we need to sample again to find out which node get mutated
sample(c(1:5),1)

#the result returned was 2 so the new function is:

#            *
#           / \
#          +  -
#        / \  / \
#      x   x -1  -2


fn4.1 <- function(x){eval(parse(text="(x+x)*(1)"))}
  
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#evaluate fitness
program.fitness <- unlist(lapply(list(fn1.1,fn2.1,fn3.1,fn4.1),fitness))
probs <- c(.5,.3,.15,.05)

ranks <- rank(program.fitness)

probs.eval <- probs[ranks]
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#sample another 4 times from the programs in generation 1 weighting by the sampling by fitness
# in this case we have a tie and I'm not really sure what to do about that
# so I'm just going to change the probability weights by hadn
sample(1:4,4,replace=T,prob=c(.4,0.05,0.4,0.15))

#result = 3,1,3,1

#now for each program selected to make it through probabilistically decide if it 
# should be reproduced, crossed-over, or mutated
sample(c('reproduction','cross-over','mutation'),4,replace=T,prob=c(.5,.25,.25))

#result = cross-over, reproduction, reproduction, reproduction

#````````````````````````````````````````````````````````````````````
#First program gets crossed-over
# Note: I don't really understand cross-over...I'm going to assume that since we've
# already selected a program that the next step is to randomly select another program
# to cross-over with the selected program

sample(c(2:4),1)

#result = 3

#now determine which node in the 1st program gets crossed and which
# node in the second program gets crossed

sample(c(1:5),1)
sample(c(1:5),1)

#result = 4,4

#so we are swapping out the 4th node in the 1st program for the 4th node in the 3rd program...
# since the programs are the same this is actually the same as reproduction on the first function
fn1.2 <- function(x){eval(parse(text="x+1"))}
#``````````````````````````````````````````````````````````````````````````

#``````````````````````````````````````````````````````````````````````````
#the other programs are all reproduced and they are all the same

fn2.2 <- function(x){eval(parse(text="x+1"))}
fn3.2 <- function(x){eval(parse(text="x+1"))}
fn4.2 <- function(x){eval(parse(text="x+1"))}

#``````````````````````````````````````````````````````````````````````````
#--------------------------------------------------------------------

#---------------------------------------------------------------------
# for generation 3 we don't need to sample the programs because they
# are all the same

#we just need to sample the evolutionary operators
sample(c('reproduction','cross-over','mutation'),4,replace=T,prob=c(.5,.25,.25))

#result = mutation, mutation, mutation, mutation

#````````````````````````````````````````````````
#mutate program 1

#run the program generator
#         - 
#        / \
#       +   %
#     / \  / \
#    x  0 x  0

#select the node in population program to mutate
sample(c(1:5),1)

#result = 5

#select the node in new program to replace mutated node with
sample(c(1:5),1)

#result = 5

#new program
#         - 
#        / \
#       +   0
#     / \   
#    x  0   

fn1.3 <- function(x){eval(parse(text="x"))} 

#````````````````````````````````````````````````

#````````````````````````````````````````````````
#mutate program 2

#run the program generator
#         % 
#        / \
#       %   x
#     / \   
#    1  4   

#select the node in population program to mutate
sample(c(1:5),1)

#result = 1...are we allowed to mutate the root node?

#select the node in new program to replace mutated node with
sample(c(1:5),1)

#result = 3

#new program
#         x 


fn2.3 <- function(x){eval(parse(text="x"))} 
#````````````````````````````````````````````````

#````````````````````````````````````````````````
#mutate program 3

#run the program generator
#         - 
#        / \
#       -4   *
#           / \  
#          -1 x

#select the node in population program to mutate
sample(c(1:5),1)

#result = 5

#select the node in new program to replace mutated node with
sample(c(1:5),1)

#result = 1

#new program
#         -
#        / \
#       +  0
#      / \
#     x  -
#       / \
#      -4  *
#         / \
#        -1  x

fn3.3 <- function(x){eval(parse(text="x-4+x"))} 
#````````````````````````````````````````````````

#````````````````````````````````````````````````
#mutate program 3

#run the program generator
#         % 
#        / \
#       x   %
#           / \  
#          x   x

#select the node in population program to mutate
sample(c(1:5),1)

#result = 1

#select the node in new program to replace mutated node with
sample(c(1:5),1)

#result = 1

#new program
#         % 
#        / \
#       x   %
#           / \  
#          x   x

fn4.3 <- function(x){eval(parse(text="x"))} 
#````````````````````````````````````````````````

#evaluate fitness for generation 3:
#evaluate fitness
program.fitness <- unlist(lapply(list(fn1.3,fn2.3,fn3.3,fn4.3),fitness))
probs <- c(.5,.3,.15,.05)

ranks <- rank(program.fitness)

probs.eval <- probs[ranks]
#--------------------------------------------------------------------


#-------------------------------------------------------------------
#generation 4

#here there are three ties so probability weights will just be
# .95/3 for the 3 with the same fitness and 
sample(1:4,4,replace=T,prob=c(.95/3,0.95/3,0.05,0.95/3))

#result = 2,4,3,4

#we just need to sample the evolutionary operators
sample(c('reproduction','cross-over','mutation'),4,replace=T,prob=c(.5,.25,.25))

#result = mutation, reproduction, reproduction, cross-over

#````````````````````````````````````````````````````````
#mutate program 1

#run program generator
#        -
#       / \
#      5   -
#         / \
#        x   x

#pick node from population program to mutate
# program 3 only has 1 node so it has to be 1

#pick node from new program to replace it with
sample(c(1:5),1)

#result = 2

#new program:

#    5

fn1.4 <- function(x){eval(parse(text="return(2)"))}

#````````````````````````````````````````````````````````

#````````````````````````````````````````````````````````
#next two are reproduction and reproduction
fn2.4 <- function(x){eval(parse(text="x"))}
fn3.4 <- function(x){eval(parse(text="x-4+x"))}
#````````````````````````````````````````````````````````

#``````````````````````````````````````````````````````````
#the final one is cross-over of function 4 from last generation

#pick which function to cross it with
sample(c(1,2,3),1)

#result = 3

#pick which node in population function to cross
sample(c(1:5),1)
#result = 5

#pick which node in function 3 to cross with
sample(c(1:9),1)
#result = 7

# new function

#          %
#         / \
#        x   -
#           / \
#          -4  *
#              / \
#             -1  x

fn4.4 <- function(x){eval(parse(text="x/(-4 + x)"))}


#``````````````````````````````````````````````````````````
#--------------------------------------------------------------------

########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
library(rgp)

#Basic building blocks
functionSet1 <- functionSet("+","-","*","/")
inputVariableSet1 <- inputVariableSet("x")
constantFactorySet1 <- constantFactorySet(function() rnorm(1))

#Defining the 'fitness' function
#this part is specific to the evaluation of the function sin(x) 
#   over the interval -pi:pi
xs1 <- seq(from=2*-pi,to=2*pi,by=0.1 )
ys1 <- sin(xs1)

fitnessFunction1 <- function(f){
  fitness <- rmse(Vectorize(f)(xs1),ys1)
  return((if(is.na(fitness)) Inf else fitness))
}

#performing the GP run
#set.seed(1)
gpResults1 <- geneticProgramming(functionSet=functionSet1,
                                 inputVariables=inputVariableSet1,
                                 constantSet=constantFactorySet1,
                                 fitnessFunction=fitnessFunction1,
                                 extinctionPrevention=TRUE,
                                 populationSize=300,
                                 stopCondition=makeTimeStopCondition(2*60))
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
#now let's try to replicate the Koza application:

#here we will be trying to recover the function from MacroEcon:
# P=MV/Q

#we want quarterly data from 1959:1 to 1988:4
#-------------------------------------------------------------------
#read data

#GNP:
gnp <- tbl_df(read.csv("/Users/aaronmamula/Documents/R projects/GP/data/GNP82.txt")) %>%
        mutate(DATE=as.Date(DATE,format='%Y-%m-%d')) %>%   
        filter(DATE>='1959-01-01')

#GDP Deflator
gd <- tbl_df(read.csv("/Users/aaronmamula/Documents/R projects/GP/data/GNPDEF.csv"))
new.dates <- strsplit(as.character(gd$DATE[1:88]),"/")

early.dates <- as.Date(unlist(lapply(new.dates,
                                     function(x)return(paste(x[1],"-",x[2],"-",paste("19",x[3],sep=""),sep="")))),
                       format="%m-%d-%Y")
late.dates <- as.Date(gd$DATE[89:279],format="%m/%d/%y")

gd$date <- c(early.dates,late.dates)

#Money Supply
M2 <- tbl_df(read.csv("/Users/aaronmamula/Documents/R projects/GP/data/M2SL.csv")) 

new.dates <- strsplit(as.character(M2$DATE[1:120]),"/")
early.dates <- as.Date(unlist(lapply(new.dates,
                                     function(x)return(paste(x[1],"-",x[2],"-",paste("19",x[3],sep=""),sep="")))),
                       format="%m-%d-%Y")
late.dates <- as.Date(M2$DATE[121:696],format="%m/%d/%y")

M2$date <- c(early.dates,late.dates)

#average money stock to quarterly values
M2 <- M2 %>% mutate(year=year(date),month=month(date),
                    quarter=ifelse(month %in% c(1,2,3),1,
                                   ifelse(month %in% c(4,5,6),2,
                                          ifelse(month %in% c(7,8,9),3,4)))) %>%
     group_by(year,quarter) %>%
     summarise(M2SL=mean(M2SL,na.rm=T)) 

#fix quarterly dates
M2 <- M2 %>% mutate(month=ifelse(quarter==1,4,
                                ifelse(quarter==2,7,
                                       ifelse(quarter==3,10,1)))) %>%
            mutate(year.new=ifelse(quarter==4,year+1,year)) %>%
            mutate(date=as.Date(paste(year.new,"-",month,"-","01",sep=""),format="%Y-%m-%d"))

#T-bill
tbill <- tbl_df(read.csv("/Users/aaronmamula/Documents/R projects/GP/data/TB3MS.csv")) 

new.dates <- strsplit(as.character(tbill$DATE[1:420]),"/")
early.dates <- as.Date(unlist(lapply(new.dates,
                                     function(x)return(paste(x[1],"-",x[2],"-",paste("19",x[3],sep=""),sep="")))),
                       format="%m-%d-%Y")
late.dates <- as.Date(tbill$DATE[421:997],format="%m/%d/%y")

tbill$date <- c(early.dates,late.dates)

#roll up to quarterly
tbill <- tbill %>% mutate(year=year(date),month=month(date),
                    quarter=ifelse(month %in% c(1,2,3),1,
                                   ifelse(month %in% c(4,5,6),2,
                                          ifelse(month %in% c(7,8,9),3,4)))) %>%
  group_by(year,quarter) %>%
  summarise(TB3MS=mean(TB3MS,na.rm=T)) 


#fix quarterly dates
tbill <- tbill %>% mutate(month=ifelse(quarter==1,4,
                                 ifelse(quarter==2,7,
                                        ifelse(quarter==3,10,1)))) %>%
  mutate(year.new=ifelse(quarter==4,year+1,year)) %>%
  mutate(date=as.Date(paste(year.new,"-",month,"-","01",sep=""),format="%Y-%m-%d"))
#-------------------------------------------------------------------


#fix GDP Deflator...in the Koza study it was pegged to 1982 = 1 but
# we'll just use whatever base year it's already in
names(gnp) <- c('date','GNP')
gnp <- gnp %>% inner_join(gd,by=c('date')) %>% mutate(gnp.real=GNP/(GNPDEF/100))

#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################

#let's start by just estimating our proposed relationship with NLS

df <- data.frame(date=M2$M2SL,m=M2$M2SL,gnp=gnp$GNP,p=gd$GNPDEF,i=tbill$TB3MS)

summary(nls(p~(a*(m*i))/(b*gnp),data=df))

#####################################################################################
#####################################################################################

#in Koza's analysis the price level can be expressed as
# GD (t)= (M2(t)*1.6527)/GNP82(t)


#our first test will be to simulate some data from this type of system 
# and see if the GP can recover something similar

#first make everything the same size
gd <- gd %>% filter(date>'1959-01-01' & date<'1988-10-01')
M2 <- M2 %>% filter(date>'1959-01-01' & date<'1988-10-01')
gnp <- gnp %>% filter(date>'1959-01-01' & date<'1988-10-01')
tbill <- tbill %>% filter(date>'1959-01-01' & date<'1988-10-01')

eps <- rnorm(118)
ysim <- (M2$M2SL * (1.65+eps))/gnp$gnp.real

plot(M2$date,ysim,type="l")


#Basic building blocks
functionSet1 <- functionSet("+","-","*","/")
inputVariableSet1 <- inputVariableSet("m","g")
constantFactorySet1 <- constantFactorySet(function() rnorm(1))

#Defining the 'fitness' function

fitnessFunction1 <- function(f){
  fitness <- rmse(Vectorize(f)(M2$M2SL,gnp$gnp.real),ysim)
  return((if(is.na(fitness)) Inf else fitness))
}

#performing the GP run
#set.seed(1)
gpResults1 <- geneticProgramming(functionSet=functionSet1,
                                 inputVariables=inputVariableSet1,
                                 constantSet=constantFactorySet1,
                                 fitnessFunction=fitnessFunction1,
                                 extinctionPrevention=TRUE,
                                 populationSize=300,
                                 stopCondition=makeTimeStopCondition(2*60))


#plot best fit function with the actual DGP
gpResults1$elite[1]
gp.fit <- M2$M2SL/(gnp$gnp.real/(M2$M2SL*M2$M2SL)/1.723378*(gnp$gnp.real+(gnp$gnp.real+gnp$gnp.real)+M2$M2SL*(M2$M2SL-M2$M2SL/(gnp$gnp.real*0.1959)+-0.375458)))

z <- data.frame(date=M2$date,y=ysim,fit=gp.fit)
ggplot(z,aes(x=date,y=y)) + geom_line() + 
  geom_line(aes(x=date,y=gp.fit),color='red')

########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
#Basic building blocks
functionSet1 <- functionSet("+","-","*","/")
inputVariableSet1 <- inputVariableSet("m","g","i")
constantFactorySet1 <- constantFactorySet(function() rnorm(1))

#Defining the 'fitness' function

fitnessFunction1 <- function(f){
  fitness <- rmse(Vectorize(f)(M2$M2SL,gnp$gnp.real,tbill$TB3MS),gd$GNPDEF)
  return((if(is.na(fitness)) Inf else fitness))
}

#performing the GP run
#set.seed(1)
gpResults1 <- geneticProgramming(functionSet=functionSet1,
                                 inputVariables=inputVariableSet1,
                                 constantSet=constantFactorySet1,
                                 fitnessFunction=fitnessFunction1,
                                 extinctionPrevention=TRUE,
                                 populationSize=300,
                                 stopCondition=makeTimeStopCondition(2*60))

