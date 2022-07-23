



















rs <- function(max){
  purrr::map_dbl(max, ~ sample(1:., 1))
}


n <- 100000

w <- rnorm(n)
u <- rnorm(n)
o <- rnorm(n, 0 + log(8)*u)

z <- rnorm(n, 0 + u*log(2)  + w*log(4))
y <- 1/exp(u*-2)


a.lines <- rnorm(n, z*log(5) + w*log(2))
plogis <- plogis(a.lines, lower.tail = T, log.p = F)
a.lines <- rbinom(n, size = 6, prob = plogis)+1


include <- a.lines!=7

# Update variables

w <- w[include] 
u <- u[include]






LOT <-c(1,1,1,1,2,2,2,3,3,4)
p <- c(rep(0.8, 4), rep(0.05, 6))

d <- data.frame(
  LOT, p
)

d_lot <- d[sample(1:nrow(d), size = 1000, replace=TRUE),]


trial <- d_lot[1:500,]
eca <- d_lot[501-1000,]

sample_trial_lot <- sample(trial$LOT, size=100, p = trial$p)
sample_eca_lot <- sample(trial$LOT, size=100)




