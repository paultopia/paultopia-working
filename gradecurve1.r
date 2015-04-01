# this is a simple r script to handle grading for classes with a curve.  
# right now, all it does is test an existing matrix of student identifiers 
# and scores against the mandatory grading curve imposed on me by the University of Iowa 
# College of Law (where I teach).  
# (specified on pg. 18 of http://law.uiowa.edu/files/law.uiowa.edu/files/2013-14_handbook_web03072014.pdf )

# Subsequent functionality to be added: 
# 1.  Modify raw scores to create scaled scores that match the curve (by making incremental
# changes to boundary scores. 
# 2. Generalise beyond Iowa Law by taking user imput specifying the shape of the curve to be matched. 
# 3. Generalize further by taking raw scores in any format. 

# (C) Paul Gowder, paul-gowder@gmail.com; paul-gowder.com.  Use freely with credit.

# begin by taking a csv 2 columns by n rows of raw final grades named rawscores.csv
# first column unique student id, second column raw score, in 4-point format.

rawscores <- read.csv("rawscores.csv")

# define grading curve based on number of students 
stu <- nrow(rawscores)

aplus.max <- .05*stu
aplus.min <- 0 
a.max <- .1*stu
a.min <- .05*stu 
aminus.max <- .2*stu
aminus.min <- .1*stu
bplus.max <- .3*stu
bplus.min <- .2*stu 
b.max <- .3*stu
b.min <- .2*stu 
bminus.max <- .2*stu
bminus.min <- .1*stu
cplus.max <- .1*stu
cplus.min <- .05*stu 
cbelow.max <- .05*stu
cbelow.min <- 0 

# get counts of students in each bucket


aplus.raw <- length(which(rawscores[,2]>=4.2 & rawscores[,2]<=4.3))
a.raw <- length(which(rawscores[,2]>=3.9 & rawscores[,2]<=4.1))
aminus.raw <- length(which(rawscores[,2]>=3.6 & rawscores[,2]<=3.8))
bplus.raw <- length(which(rawscores[,2]>=3.3 & rawscores[,2]<=3.5))
b.raw <- length(which(rawscores[,2]>=3 & rawscores[,2]<=3.2))
bminus.raw <- length(which(rawscores[,2]>=2.7 & rawscores[,2]<=2.9))
cplus.raw <- length(which(rawscores[,2]>=2.4 & rawscores[,2]<=2.6))
cbelow.raw <- length(which(rawscores[,2]<=2.3))

# compare raw distribution to permissible curve range

if (aplus.raw <= aplus.max & aplus.raw >= aplus.min) 
{ 
aplus.match <- 1 
} else 
{
aplus.match <- 0
}

if (a.raw <= a.max & a.raw >= a.min) 
{ 
a.match <- 1 
} else 
{
a.match <- 0
}
 
if (aminus.raw <= aminus.max & aminus.raw >= aminus.min) 
{ 
aminus.match <- 1 
} else 
{
aminus.match <- 0
}

if (bplus.raw <= bplus.max & bplus.raw >= bplus.min) 
{ 
bplus.match <- 1 
} else
{ 
bplus.match <- 0
}

if (b.raw <= b.max & b.raw >= b.min) 
{ 
b.match <- 1 
} else
{ 
b.match <- 0
}

if (bminus.raw <= bminus.max & bminus.raw >= bminus.min) 
{ 
bminus.match <- 1 
} else
{ 
bminus.match <- 0
}

if (cplus.raw <= cplus.max & cplus.raw >= cplus.min) 
{ 
cplus.match <- 1 
} else
{ 
cplus.match <- 0
}

if (cbelow.raw <= cbelow.max & cbelow.raw >= cbelow.min) 
{ 
cbelow.match <- 1 
} else
{ 
cbellow.match <- 0
}


if (aplus.match == 1 & a.match == 1 & aminus.match == 1 & bplus.match == 1 & b.match == 1 & bminus.match == 1 & cplus.match == 1 & cbelow.match == 1) 
{ 
print("Your grades already match the curve!  Go home, get a drink.") 
} else
{

print("Your grades don't match the curve. Time to rebalance.")


# up to this point, the code works and successfully checks whether or not the grades match the curve.
# Next step: carry out the rebalancing, all w/in this else statement.




}




