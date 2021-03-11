#https://stats.idre.ucla.edu/r/faq/frequently-asked-questions-about-rhow-can-i-subset-a-data-setthe-r-program-as-a-text-file-for-all-the-code-on-this-page-subsetting-is-a-very-important-component/

set.seed(1234)
x <- matrix(rnorm(30, 1), ncol = 5)
y <- c(1, seq(5))

#combining x and y into one matrix
x <- cbind(x, y)

#converting x into a data frame called x.df
x.df <- data.frame(x)
x.df

#Subsetting rows using the subset function
x.sub <- subset(x.df, y > 2)
x.sub

#Subsetting rows using multiple conditional statements
x.sub1 <- subset(x.df, y > 2 & V1 > 0.6)

#Subsetting both rows and columns
x.sub2 <- subset(x.df, y > 2 & V2 > 0.4, select = c(V1, V4))

#only the observations in variables V2-V5 
x.sub3 <- subset(x.df, y > 3, select = V2:V5)

#Subsetting rows using indices
x.sub4 <- x.df[x.df$y == 1, ]

#Subsetting rows selecting on more than one value
#We use the %in% notation when we want to subset on multiple values of y
#variable y are equal to either 1 or 4.
x.sub5 <- x.df[x.df$y %in% c(1, 4), ]

#Subsetting columns using indices
x.sub6 <- x.df[, 1:2]

#Subsetting both rows and columns using indices
x.sub8 <- x.df[c(1, 3), 3:6]