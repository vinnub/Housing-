################################
####     BOOTSTRAP ON THE NUMBER OF building/managers
##################################


boot.population <- as.integer(interest)
B = 1000 # the number of bootstrap samples we want
S = 600
boot.sample <- array(dim = c(B, S))
for (i in 1:B) {
  boot.sample[i, ] <- sample(boot.population, size = S, replace = FALSE)
}
high_percent <- function (x) length(which(x == 2))/length(x)
medium_percent <- function (x) length(which(x == 1))/length(x)

boot.high <- apply(X = boot.sample, MARGIN = 1, FUN = high_percent)
boot.medium <- apply(X = boot.sample, MARGIN = 1, FUN = medium_percent)
hist(boot.high, breaks = 25)
mean(boot.high)
boot.sd <- sd(boot.high)
mean(boot.high) + c(-1, 1)*1.96*boot.sd

mean(boot.medium)
boot.medium.sd <- sd(boot.medium)
mean(boot.medium) + c(-1, 1)*1.96*boot.medium.sd
