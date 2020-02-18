file2 <- read.csv("~/Rutgers/Spring_2019/DATA-101/Assignments/pc1/M2019btrain.csv")
summary(file2)
boxplot(file2$SCORE~file2$GRADE)

a <- subset(file2, file2$GRADE=="A")
summary(a)
ac <- subset(file2, file2$SCORE>= 75 & file2$SCORE<= 100)
boxplot(ac$SCORE~ac$GRADE)
summary(ac)
acd <- subset(file2, file2$SCORE>78 & file2$SCORE<99)
boxplot(acd$SCORE~acd$GRADE)
summary(acd)
data.frame(acd)
bina <- subset(file2, file2$SCORE>82 & file2$SCORE<98) 
summary(bina)
boxplot(bina$SCORE~bina$GRADE)
data.frame(bina)
a_bina <- subset(bina, bina$GRADE=="A")
b_bina <- subset(bina, bina$GRADE=="B")
c_bina <- subset(bina, bina$GRADE=="C")
summary(a_bina)
summary(b_bina)
summary(c_bina)
cina <- subset(file2, file2$SCORE>=83.7 & file2$SCORE<=84.03)
summary(cina)
data.frame(cina)



b <- subset(file2, file2$GRADE=="B")
summary(b)
bc <- subset(file2, file2$SCORE>=60.93 & file2$SCORE<=87.70)
summary(bc)
boxplot(bc$SCORE~bc$GRADE)
bc1 <- subset(file2, file2$SCORE>=64 & file2$SCORE<=74)
summary(bc1)
boxplot(bc1$SCORE~bc1$GRADE)

c <- subset(file2, file2$GRADE=="C")
summary(c)
cc <- subset(file2, file2$SCORE>=60.93 & file2$SCORE<=87.70)
summary(cc)
boxplot(cc$SCORE~cc$GRADE)
ccs <- subset(cc, cc$SCORE>=74 & cc$SCORE<=82)
summary(ccs)
boxplot(ccs$SCORE~ccs$GRADE)

d <- subset(file2, file2$GRADE=="D")
summary(d)
dc <- subset(file2, file2$SCORE>=46 & file2$SCORE<=73.3)
summary(dc)
boxplot(dc$SCORE~dc$GRADE)
data.frame(dc)
dcb <- subset(dc, dc$SCORE>=59 & dc$SCORE<68)
summary(dcb)
boxplot(dcb$SCORE~dcb$GRADE)

###participation
hist(a$PARTICIPATION)
hist(b$PARTICIPATION)
hist(c$PARTICIPATION)
hist(d$PARTICIPATION)

## by chategorical
summary(file2$PARTICIPATION)
boxplot(file2$PARTICIPATION~file2$GRADE)
boxplot(file2$SCORE~file2$LEAVES_EARLY)

# nn <- subset(file2, file2$ASKS_QUESTIONS=="never"&file2$LEAVES_EARLY=="never")
# summary(nn)
# boxplot(nn$PARTICIPATION~nn$GRADE)


####Traiining
mp <- file2
decision <- rep("F", nrow(mp))
decision[mp$SCORE>40 & mp$PARTICIPATION>.18] <- "D"
decision[mp$SCORE>50 & mp$PARTICIPATION>.2] <- "C"
decision[mp$SCORE>63 & mp$PARTICIPATION>.4] <- "B"
decision[(mp$SCORE>83 & mp$PARTICIPATION>=0.5)] <- "A"

decision[(mp$SCORE>=59 & mp$SCORE<=68)] <- 'D'
decision[(mp$SCORE>=76 & mp$SCORE<=79)|((mp$SCORE>=83 & mp$SCORE<=84.04) & mp$PARTICIPATION<=0.25)] <- 'C'
decision[(mp$SCORE>=66 & mp$SCORE<=69)] <- 'B'
decision[(mp$SCORE>=83 & mp$SCORE<=85) & mp$PARTICIPATION>0.25] <- 'A'

mp$PROJ <- decision
error <- mean(mp$GRADE!=mp$PROJ)
error

# test 
M2019_test_students <- read.csv("~/Rutgers/Spring_2019/DATA-101/Assignments/pc1/M2019_test_students.csv")
mp <- M2019_test_students
decision <- rep("F", nrow(mp))
decision[mp$SCORE>40 & mp$PARTICIPATION>.18] <- "D"
decision[mp$SCORE>50 & mp$PARTICIPATION>.2] <- "C"
decision[mp$SCORE>63 & mp$PARTICIPATION>.4] <- "B"
decision[(mp$SCORE>83 & mp$PARTICIPATION>=0.5)] <- "A"

decision[(mp$SCORE>=59 & mp$SCORE<=68)] <- 'D'
decision[(mp$SCORE>=76 & mp$SCORE<=79)|((mp$SCORE>=83 & mp$SCORE<=84.04) & mp$PARTICIPATION<=0.25)] <- 'C'
decision[(mp$SCORE>=66 & mp$SCORE<=69)] <- 'B'
decision[(mp$SCORE>=83 & mp$SCORE<=85) & mp$PARTICIPATION>0.25] <- 'A'
M2019_test_students$X <- decision

M2019_sample_submission <- read.csv("~/Rutgers/Spring_2019/DATA-101/Assignments/pc1/M2019_sample_submission.csv")
M2019_sample_submission$GRADE <- decision

write.csv(M2019_sample_submission, file = "prediction1.csv", row.names = FALSE)
