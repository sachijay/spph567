library(ggplot2)

n <- 500

n_days <- 5
n_weeks <- n/n_days

mu_c <- 1

## Find the relationship between n and T
t1 <- c(5, 10, 30, 100, 300, 1000)
n1 <- c(495, 495, 490, 485, 450, 375)

ggplot(data.frame(t = t1, n = n1),
       aes(x = t, y = n)) +
  geom_point()

n_t_mod <- lm(n1 ~ t1)


#####
get_damp_fac <- function(t, cv_c, k){
  
  # set.seed(1)
  
  sd_c <- cv_c*mu_c
  # sd_c <- exp(sqrt(log(1 + cv_c^2)))
  
  c_ij <- rlnorm(n, 
                 meanlog = mu_c,
                 sdlog = sd_c)
  
  cv_c <- sd(c_ij)/mean(c_ij)
  
  ##
  k_x <- 0.693/t
  
  fac1 <- k*(1-exp(-8*k_x))/k_x
  fac2 <- exp(-24*k_x)
  fac3 <- exp(-72*k_x)
  
  ##
  tmp <- c_ij*fac1
  
  add <- rep(0, times = n)
  
  for (j in 1:n_weeks) {
    
    week_ind <- (j-1)*n_days
    
    if(j == 1) {
      add[week_ind + 1] <- 0
    } else {
      add[week_ind + 1] <- tmp[week_ind]*fac3
    }
    
    for (i in 2:n_days) {
      ind <- week_ind + i
      
      add[ind] <- tmp[ind - 1]*fac2
    }
  }
  
  x_ij <- tmp + add
  
  x_ij_select <- tail(x_ij, n_t_mod$coefficients[1] + n_t_mod$coefficients[2]*t)
  
  cv_x <- sd(x_ij_select)/mean(x_ij_select)
  
  damp_fac <- cv_x/cv_c
  
  # print(cv_x)
  # print(cv_c)
  
  return(damp_fac)
}


get_damp_fac(t = 5,
             cv_c = 0.2,
             k = 1)

t_seq <- 1:1000
damp_fac0.1 <- sapply(t_seq, get_damp_fac, cv_c = 0.1, k = 1)
damp_fac0.2 <- sapply(t_seq, get_damp_fac, cv_c = 0.2, k = 1)
damp_fac0.3 <- sapply(t_seq, get_damp_fac, cv_c = 0.3, k = 1)
damp_fac0.5 <- sapply(t_seq, get_damp_fac, cv_c = 0.5, k = 1)
damp_fac2.5 <- sapply(t_seq, get_damp_fac, cv_c = 2.5, k = 1)


plot_df <- data.frame(t = t_seq,
           damp_fac0.2 = damp_fac0.2,
           damp_fac0.5 = damp_fac0.5,
           damp_fac2.5 = damp_fac2.5)

ggplot(plot_df, aes(x = t)) + 
  # geom_smooth(aes(y = damp_fac0.2), se = FALSE, method = "gam", formula = y ~ s(x, bs = "tp")) +
  geom_line(aes(y = damp_fac2.5), col = "green") +
  geom_line(aes(y = damp_fac0.5), col = "red") +
  # geom_line(aes(y = damp_fac0.3), col = "blue") +
  geom_line(aes(y = damp_fac0.2)) +
  # geom_line(aes(y = damp_fac0.1), col = "steelblue") +
  scale_x_continuous(trans = 'log10') + 
  labs(x = "T (hr)",
       y = "1/A")
