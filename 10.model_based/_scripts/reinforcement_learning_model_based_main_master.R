# =============================================================================
#### Info #### 
# =============================================================================
# Hierachical simple reinforcement learning model
#
# true parameters: lr  = rnorm(10, mean=0.6, sd=0.12); tau = rnorm(10, mean=1.5, sd=0.2)
#
# Lei Zhang, UKE, Hamburg, DE
# lei.zhang@uke.de

# =============================================================================
#### Construct Data #### 
# =============================================================================
# clear workspace
rm(list=ls(all=TRUE))
library(rstan)
library(ggplot2)

load('_data/rl_mp.RData')
sz <- dim(rl_mp)
nSubjects <- sz[1]
nTrials   <- sz[2]

dataList <- list(nSubjects=nSubjects,
                 nTrials=nTrials, 
                 choice=rl_mp[,,1], 
                 reward=rl_mp[,,2])


# =============================================================================
#### Running Stan #### 
# =============================================================================
rstan_options(auto_write = TRUE)
options(mc.cores = 2)

modelFile <- '_scripts/reinforcement_learning_model_based.stan'

nIter     <- 2000
nChains   <- 4 
nWarmup   <- floor(nIter/2)
nThin     <- 1

cat("Estimating", modelFile, "model... \n")
startTime = Sys.time(); print(startTime)
cat("Calling", nChains, "simulations in Stan... \n")

fit_rl <- stan(modelFile, 
               data    = dataList, 
               chains  = nChains,
               iter    = nIter,
               warmup  = nWarmup,
               thin    = nThin,
               init    = "random",
               seed    = 1450154185
)

cat("Finishing", modelFile, "model simulation ... \n")
endTime = Sys.time(); print(endTime)  
cat("It took",as.character.Date(endTime - startTime), "\n")

saveRDS(fit_rl, file='_outputs/fit_rl.RData')

# =============================================================================
#### Model Summary, extract internal model variables #### 
# =============================================================================
fit_rl <- readRDS('_outputs/fit_rl.RData')

print(fit_rl, pars = c('lr_mu', 'tau_mu', 'lr', 'tau', 'log_lik'))

dec_var <- get_posterior_mean(fit_rl, pars=c('vc', 'pe', 'v'))[,5]
vc <- dec_var[1:(nSubjects*nTrials)]
pe <- dec_var[(1:(nSubjects*nTrials)) + nSubjects*nTrials]
vc <- matrix(vc, nrow = nSubjects, ncol = nTrials, byrow = T)
pe <- matrix(pe, nrow = nSubjects, ncol = nTrials, byrow = T)

#### take one participants as an example, subj = 1
vc_sub1 <- vc[1,]
pe_sub1 <- pe[1,]
ch_sub1 <- dataList$choice[1,]
rw_sub1 <- dataList$reward[1,]

df_sub1 <- data.frame(trial  = 1:nTrials,
                      choice = ch_sub1,
                      reward = rw_sub1,
                      value  = vc_sub1,
                      pe     = pe_sub1)


#### make plots of choice, reward, v(chn), and pe
library(ggplot2)
myconfig <- theme_bw(base_size = 20) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank() )

g1 <- ggplot(df_sub1, aes(x=trial, y=value)) 
g1 <- g1 + geom_line(size = 2, color = 'black') + geom_point(size = 3, shape = 21, fill='black')
g1 <- g1 + myconfig + labs(x = 'Trial', y = 'Chosen Value')
g1

g2 <- ggplot(df_sub1, aes(x=trial, y=pe)) 
g2 <- g2 + geom_line(size = 2, color = 'black') + geom_point(size = 3, shape = 21, fill='black')
g2 <- g2 + myconfig + labs(x = 'Trial', y = 'Prediction Error')
g2

ggsave(plot = g1, "_plots/sub1_value.png", width = 10, height = 4, type = "cairo-png", units = "in")
ggsave(plot = g2, "_plots/sub1_pe.png", width = 10, height = 4, type = "cairo-png", units = "in")


#### correlation matrix of PE, value and outcome
a = df_sub1[, c('pe', 'reward', 'value')]
colnames(a) = c('PE', 'Reward', 'Value')
r = corrr::rplot(shave(correlate(a)), shape=15, colors = c('indianred2','white','chartreuse3'))
r = r + theme(axis.text   = element_text(size=20), legend.text = element_text(size=15))
r
ggsave(plot = r, "_plots/PE_V_R_corr_mat.png", width = 5, height = 3, type = "cairo-png", units = "in")


#### EXERCISE: extract and plot the value of option2 ####
v <- dec_var[((nSubjects*nTrials)*2+1):length(dec_var)]
v <- matrix(v, nrow = 2, ncol = length(v)/2) # 1st row for option1, 2nd row for option2

v_op1 <- v[1,]
v_op1 <- matrix(v_op1, nrow = nSubjects, ncol = nTrials+1, byrow = T)
v_op1 <- v_op1[1:nSubjects, 1:nTrials]  # remove the 101th trial
v_op1_sub1 <- v_op1[1,]

v_op2 <- v[2,]
v_op2 <- matrix(v_op2, nrow = nSubjects, ncol = nTrials+1, byrow = T)
v_op2 <- v_op2[1:nSubjects, 1:nTrials]  # remove the 101th trial
v_op2_sub1 <- v_op2[1,]

df2_sub1 <- data.frame(trial  = 1:nTrials,
                       value1 = v_op1_sub1,
                       value2 = v_op2_sub1)

g3 <- ggplot(df2_sub1, aes(x=trial, y=value1)) 
g3 <- g3 + geom_line(size = 2, color = 'black') + geom_point(size = 3, shape = 21, fill='black')
g3 <- g3 + myconfig + labs(x = 'Trial', y = 'value of option 1')
g3

g4 <- ggplot(df2_sub1, aes(x=trial, y=value2)) 
g4 <- g4 + geom_line(size = 2, color = 'black') + geom_point(size = 3, shape = 21, fill='black')
g4 <- g4 + myconfig + labs(x = 'Trial', y = 'value of option 2')
g4

ggsave(plot = g3, "_plots/sub1_value_opt1.png", width = 10, height = 4, type = "cairo-png", units = "in")
ggsave(plot = g4, "_plots/sub1_value_opt2.png", width = 10, height = 4, type = "cairo-png", units = "in")
