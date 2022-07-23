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


prob <- max(a.lines):1

dat <- 
  data.frame(d.time = rexp(n, y), 
             a.lines,
             w,
             u,
             o,
             prior.lot = a.lines - 1) %>%
  dplyr::mutate(
    lot.trial = sample(1:max(a.lines), n, TRUE),
    inc.trial = lot.trial <= a.lines,
    lot.trial2 = sample(1:max(a.lines), n, TRUE, prob = prob),
    inc.trial2 = lot.trial2 <= a.lines,
    lot.rand = rs(a.lines),
    inc.rand = lot.rand <= a.lines
  )

dat %>% dplyr::filter(inc.trial) %>%
  summarise_all(
    mean
  )

dat %>% dplyr::filter(inc.trial) %>%
  dplyr::select(w, u) %>% cor()


dat %>% dplyr::filter(inc.trial2) %>%
  summarise_all(
    mean
  )

dat %>% dplyr::filter(inc.trial2) %>%
  dplyr::select(w, u) %>% cor()


dat %>% dplyr::filter(inc.rand)%>%
  summarise_all(
    mean
  )

dat %>% dplyr::filter(inc.rand) %>%
  dplyr::select(w, u) %>% cor()


an.dat1 <- rbind(
  dat %>% dplyr::filter(inc.trial) %>%
    dplyr::mutate(trt = 0,
                  n.prior = lot.trial - 1),
  dat %>% dplyr::filter(inc.rand) %>%
    dplyr::mutate(trt = 1, n.prior = lot.rand -1)
) %>% dplyr::mutate(event = 1, time = d.time)

coxph(Surv(time, event) ~ trt, data = an.dat1)
coxph(Surv(time, event) ~ trt + strata(n.prior), data = an.dat1)

coxph(Surv(time, event) ~ trt + strata(n.prior) + u, data = an.dat1)

coxph(Surv(time, event) ~ trt + strata(n.prior) + o, data = an.dat1)



an.dat2 <- rbind(
  dat %>% dplyr::filter(inc.trial) %>%
    dplyr::mutate(trt = 0,
                  n.prior = lot.trial - 1),
  dat %>% dplyr::filter(inc.trial2) %>%
    dplyr::mutate(trt = 1, n.prior = lot.trial2 -1)
) %>% dplyr::mutate(event = 1, time = d.time)

coxph(Surv(time, event) ~ trt, data = an.dat2)
coxph(Surv(time, event) ~ trt + strata(n.prior), data = an.dat2)
coxph(Surv(time, event) ~ trt + strata(n.prior) + u, data = an.dat2)
coxph(Surv(time, event) ~ trt + strata(n.prior) + o, data = an.dat2)
