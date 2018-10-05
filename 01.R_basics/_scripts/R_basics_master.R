# =============================================================================
#### Info #### 
# =============================================================================
# R basics and probability functions
#
# Lei Zhang, UKE, Hamburg, DE
# lei.zhang@uke.de

# =============================================================================
#### Basic Commands #### 
# =============================================================================
getwd()
setwd() # this requires the PATH as input argument
dir()
ls()
print('hello world')
cat("Hello", "World")
paste0('C:/', 'Group1')
? mean
rm(list = ls())
q()

# =============================================================================
#### Data Classes #### 
# =============================================================================

# numeric & integer
a1 <- 5
a2 <- as.integer(a1)

class(a1)
class(a2)

# character
b1 <- 'Hello World!'
b2 <- "Hello World!"
b3 <- 'Hello "World"!'
b4 <- "Hello 'World'!"
b5 <- "Hello Neuroscience\'s World!"
class(b1)
print(b3)
print(b4)
cat(b3)
cat(b4)


## logical
c1 <- T; c2 <- TRUE; c3 <- F; c4 <- FALSE
class(c1)

# factor
f <- factor(letters[c(1, 1, 2, 2, 3:10)])
class(f)

# =============================================================================
#### Data Types ####
# =============================================================================

# vector
v1 <- 1:12
v2 <- c(2,4,1,5,1,6, 13:18)
v3 <- c(rep('aa',4), rep('bb',4), rep('cc',4))
class(v1)
class(v2)
class(v3)

# matrix and array
m1 <- matrix(v1, nrow=3, ncol=4)
m2 <- matrix(v1, nrow=3, ncol=4, byrow = T)
arr <- array(v1, dim=c(2,2,3))
class(m1)
class(arr)

# dataframe
df <- data.frame(v1=v1, v2=v2, v3=v3, f=f)
class(df)
str(df)
class(df$v1)
class(df$v2)
class(df$v3)
class(df$f)

# =============================================================================
#### Control Flow #### 
# =============================================================================

# if-else
t <- runif(1) # random number between 0 and 1
if (t <= 1/3) {
    cat("t =", t, ", t <= 1/3. \n")
} else if (t > 2/3) {
    cat("t =", t, ", t > 2/3. \n")
} else {
    cat("t =", t, ", 1/3 < t <= 2/3. \n")
}

# for-loop
month_name <- format(ISOdate(2018,1:12,1),"%B")
for (j in 1:length(month_name) ) {
    cat("The month is", month_name[j], "\n")
}

# =============================================================================
#### User-defined Function #### 
# =============================================================================
# calculate the meam 
my_mean <- function(x) {
    x_bar <- sum(x) / length(x)
    return(x_bar)
}

tmp <- rnorm(10)
my_mean(tmp)

# sanity check
all.equal(mean(tmp), my_mean(tmp))

# =============================================================================
#### three plotting pkgs #### 
# =============================================================================
x = rnorm(20)
y = x + rnorm(20,0,.8)
plot(x,y)
ggplot2::qplot(x,y)
lattice::xyplot(x,y)

# =============================================================================
#### Probability Functions #### 
# =============================================================================
library(ggplot2)

myconfig <- theme_bw(base_size = 20) +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank() )

## normal distribution
myMean = 0 #0.5
mySD   = 1 #2
myXlim = c(myMean-5, myMean+5)

# dnorm
g1 <- ggplot(data.frame(x = myXlim), aes(x)) + 
     stat_function(fun = dnorm, args = list(mean = myMean, sd = mySD), size = 3, colour = 'black')
g1 <- g1 + myconfig
print(g1)
ggsave(plot = g1, "_plots/dnorm.png", width = 6, height = 4, type = "cairo-png", units = "in")

## stat_function(fun = dnorm, args = list(mean = myMean, sd = mySD), geom='area', size = 3, colour = '#c00000', fill = '#c00000')

# pnorm
g2 <- ggplot(data.frame(x = myXlim), aes(x)) + 
    stat_function(fun = pnorm, args = list(mean = myMean, sd = mySD), size = 3)
g2 <- g2 + myconfig
print(g2)
ggsave(plot = g2, "_plots/pnorm.png", width = 6, height = 4, type = "cairo-png", units = "in")

# qnorm
g3 <- ggplot(data.frame(x = c(0,1)), aes(x)) + 
    stat_function(fun = qnorm, args = list(mean = myMean, sd = mySD), size = 3)
g3 <- g3 + myconfig
print(g3)
ggsave(plot = g3, "_plots/qnorm.png", width = 6, height = 4, type = "cairo-png", units = "in")

# rnorm
set.seed(1494)
df <- data.frame( x = rnorm(2000, mean = myMean, sd = mySD) )
g4 <- ggplot(df, aes(x)) + geom_histogram(binwidth = .3, fill = 'black')
g4 <- g4 + myconfig + ylab("") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank())
print(g4)
ggsave(plot = g4, "_plots/rnorm.png", width = 6, height = 4, type = "cairo-png", units = "in")

