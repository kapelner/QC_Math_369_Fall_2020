pacman::p_load(ggplot2)

Nsim = 1000
nsim = 1

n = 17
theta = 369

thetahathat_mm_s = array(NA, Nsim)
thetahathat_mle_s = array(NA, Nsim)

for (nsim in 1 : Nsim){
  xs = runif(n, 0, theta)
  thetahathat_mm_s[nsim] = 2 * mean(xs)
  thetahathat_mle_s[nsim] = max(xs)
  ggplot(data.frame(x = xs, y = 0)) + aes(x = x, y = y) + 
    geom_point() +  
    xlim(0, theta * 1.5) + 
    theme(
      panel.grid = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) + 
    geom_vline(xintercept = theta, col = "black") + 
    geom_vline(xintercept = thetahathat_mm_s[nsim], col = "red") + 
    geom_vline(xintercept = thetahathat_mle_s[nsim], col = "green")
}

all_res = data.frame(
  thetahathat_mm_s = thetahathat_mm_s, 
  thetahathat_mle_s = thetahathat_mle_s,
  thetahathat_mm_sqd_losses = (thetahathat_mm_s - theta)^2,
  thetahathat_mle_sqd_losses = (thetahathat_mle_s - theta)^2
)

ggplot(all_res) +
  geom_density(aes(x = thetahathat_mm_s), fill = "red", col = "red", alpha = .8) + 
  geom_density(aes(x = thetahathat_mle_s), fill = "green", col = "green", alpha = .8) + 
  geom_vline(xintercept = theta, col = "black")

ggplot(all_res) +
  geom_density(aes(x = thetahathat_mm_sqd_losses), fill = "red", col = "red", alpha = .8) + 
  geom_density(aes(x = thetahathat_mle_sqd_losses), fill = "green", col = "green", alpha = .8) + 
  geom_vline(xintercept = mean(all_res$thetahathat_mm_sqd_losses), col = "red", lwd = 1) + 
  geom_vline(xintercept = mean(all_res$thetahathat_mle_sqd_losses), col = "green", lwd = 1) +
  xlim(0, quantile(all_res$thetahathat_mm_sqd_losses, 0.95))

mean(all_res$thetahathat_mm_sqd_losses)
mean(all_res$thetahathat_mle_sqd_losses)
mean(all_res$thetahathat_mm_sqd_losses) / mean(all_res$thetahathat_mle_sqd_losses)
