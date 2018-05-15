library(zoo)      
library(ggplot2)
library(forecast)
library(cowplot)
library(rugarch)
library(gridExtra)
library(MTS)
library(FinTS)
library(tseries)

theme_set(theme_classic())

company_name = 'magnit'
  
ROOT = 'path/to/data'
ROOT_PLOTS = 'plots'

window = 500
refit.every = 20

cat('\nFitting model for', company_name, '\n')
  
# Paths to files

TS_PATH = paste(ROOT, '/magnit_price.csv', sep = '')
FINAM_PATH = paste(ROOT, '/magnit_finam_news.csv', sep = '')
ENTIT_PATH = paste(ROOT, '/magnit_entit_news.csv', sep = '')
TRIPLES_PATH = paste(ROOT, '/magnit_triples_news.csv', sep = '')

# Load data 

stock_data <- load_stock_data(TS_PATH)

finam <- read.zoo(FINAM_PATH, header = TRUE, sep = ",", format = "%Y-%m-%d") 
stock_data <- merge(stock_data, finam)

entit <- read.zoo(ENTIT_PATH, header = TRUE, sep = ",", format = "%Y-%m-%d") 
stock_data <- merge(stock_data, entit)

triples <- read.zoo(TRIPLES_PATH, header = TRUE, sep = ",", format = "%Y-%m-%d") 
stock_data <- merge(stock_data, triples)

stock_data <- na.fill(stock_data, 0)

# Mean and variance tests

t.test(as.vector(stock_data$returns), mu=0)
var.test(as.vector(stock_data$returns), rnorm(1816), alternative = "two.sided")
# 1816 is the number of observations

# Arch tests

returns <- as.numeric(stock_data$returns)
ArchTest(returns, lags=2)
ArchTest(returns, lags=5)
ArchTest(returns, lags=10)


# Base specification

mean = list(armaOrder = c(0,0), include.mean = FALSE)
fit.control = list(rec.init = 0.7)

garchOrder <- c(1,1)

base_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = garchOrder, external.regressors = NULL), 
  mean.model = mean, 
  distribution.model = "norm")

# News specifications

finam_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = garchOrder, 
                        external.regressors = as.matrix(stock_data[, 'finam'])), 
  mean.model = mean,
  distribution.model = "norm")

entit_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = garchOrder, 
                        external.regressors = as.matrix(stock_data[, 'entit'])), 
  mean.model = mean,
  distribution.model = "norm")

triples_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = garchOrder, 
                        external.regressors = as.matrix(stock_data[, 'triples'])), 
  mean.model = mean,
  distribution.model = "norm")

base_fit <- ugarchfit(base_spec, data = stock_data$returns, fit.control = fit.control) 
finam_fit <- ugarchfit(finam_spec, data = stock_data$returns, fit.control = fit.control) 
entit_fit <- ugarchfit(entit_spec, data = stock_data$returns, fit.control = fit.control) 
triples_fit <- ugarchfit(triples_spec, data = stock_data$returns, fit.control = fit.control) 

# Properties of standardized residuals

base_res = residuals(base_fit) / sigma(base_fit) 
finam_res = residuals(finam_fit) / sigma(finam_fit) 
entit_res = residuals(entit_fit) / sigma(entit_fit) 
triples_res = residuals(triples_fit) / sigma(triples_fit) 

ArchTest(base_res, 1) 
ArchTest(base_res, 2) 
ArchTest(base_res, 5) 
ArchTest(base_res, 10) 

ArchTest(finam_res, 1) 
ArchTest(finam_res, 2) 
ArchTest(finam_res, 5) 
ArchTest(finam_res, 10) 

ArchTest(entit_res, 1) 
ArchTest(entit_res, 2) 
ArchTest(entit_res, 5) 
ArchTest(entit_res, 10) 

ArchTest(triples_res, 1) 
ArchTest(triples_res, 2) 
ArchTest(triples_res, 5) 
ArchTest(triples_res, 10) 

jarque.bera.test(base_res)
jarque.bera.test(finam_res)
jarque.bera.test(entit_res)
jarque.bera.test(triples_res)

t.test(as.vector(base_res), mu=0)
t.test(as.vector(finam_res), mu=0)
t.test(as.vector(entit_res), mu=0)
t.test(as.vector(triples_res), mu=0)

var.test(as.vector(base_res), rnorm(1816), alternative = "two.sided")
var.test(as.vector(finam_res), rnorm(1816), alternative = "two.sided")
var.test(as.vector(entit_res), rnorm(1816), alternative = "two.sided")
var.test(as.vector(triples_res), rnorm(1816), alternative = "two.sided")

# QQ-plots

plot(base_fit, which=9)
plot(finam_fit, which=9)
plot(entit_fit, which=9)
plot(triples_fit, which=9)


# Plots of fitted models

png(paste(ROOT_PLOTS, '/', company_name, '_conditional_sd', '_base.png', sep = ''), 500, 300)
par(col.main='white') 
plot(base_fit, which = 1)
par(col.main='black') 
title('GARCH(1,1)')
dev.off()

png(paste(ROOT_PLOTS, '/', company_name, '_conditional_sd', '_finam.png', sep = ''), 500, 300)
par(col.main='white') 
plot(finam_fit, which = 1)
par(col.main='black') 
title('human preprocessed news')
dev.off()

png(paste(ROOT_PLOTS, '/', company_name, '_conditional_sd', '_entit.png', sep = ''), 500, 300)
par(col.main='white') 
plot(entit_fit, which = 1)
par(col.main='black') 
title('named entities')
dev.off()

png(paste(ROOT_PLOTS, '/', company_name, '_conditional_sd', '_triples.png', sep = ''), 500, 300)
par(col.main='white') 
plot(triples_fit, which = 1)
par(col.main='black') 
title('SOV-triples')
dev.off()

# Infocriteria

infocriteria(base_fit)
cat('\nLikelihood base:', likelihood(base_fit))

infocriteria(finam_fit)
cat('\nLikelihood finam:', likelihood(finam_fit))
print('\n')

infocriteria(entit_fit)
cat('\nLikelihood entit:', likelihood(entit_fit))
print('\n')

infocriteria(triples_fit)
cat('\nLikelihood triples:', likelihood(triples_fit))
print('\n')

# Coefs 

cat('\nBase coef\n')
printvec(coef(base_fit))
cat('\nfinam coef\n')
printvec(coef(finam_fit))
cat('\n')
print(confint(finam_fit))
cat('\n')
cat('\nentit coef\n')
printvec(coef(entit_fit))
cat('\n')
print(confint(entit_fit))
cat('\n')
cat('\ntriples coef\n')
printvec(coef(triples_fit))
cat('\n')
print(confint(triples_fit))
cat('\n')

# Fit using all data from the beginning

base_roll = fit_roll(base_spec, stock_data)
finam_roll = fit_roll(finam_spec, stock_data)
entit_roll = fit_roll(entit_spec, stock_data)
triples_roll = fit_roll(triples_spec, stock_data)

base_coefs <- get_coefs(base_roll)
png(paste(ROOT_PLOTS, '/', company_name, '_exp_coefs', '_base.png', sep = ''), 1200, 400)
plot_garch_coefs(base_coefs, 'GARCH(1,1)')
dev.off()

finam_coefs <- get_coefs(finam_roll, include_external = TRUE, external = c('vxreg1'))
png(paste(ROOT_PLOTS, '/', company_name, '_exp_coefs', '_finam.png', sep = ''), 1200, 400)
plot_garch_coefs(finam_coefs, 'human-preprocessed news')
dev.off()

entit_coefs <- get_coefs(entit_roll, include_external = TRUE, external = c('vxreg1'))
png(paste(ROOT_PLOTS, '/', company_name, '_exp_coefs', '_entit.png', sep = ''), 1200, 400)
plot_garch_coefs(entit_coefs, 'named entities')
dev.off()

triples_coefs <- get_coefs(triples_roll, include_external = TRUE, external = c('vxreg1'))
png(paste(ROOT_PLOTS, '/', company_name, '_exp_coefs', '_triples.png', sep = ''), 1200, 400)
plot_garch_coefs(triples_coefs, 'SOV-triples')
dev.off()

print(fpm(base_roll))
print(fpm(finam_roll))
print(fpm(entit_roll))
print(fpm(triples_roll))

png(paste(ROOT_PLOTS, '/', company_name, '_exp_vxreg', '_finam.png', sep = ''), 700, 500)
  plot_coef(finam_coefs, 
            finam_coefs$vxreg1, 
            finam_coefs$vxreg1_std, 'human-preprocessed news', ylims = c(0, 0.0003))
dev.off()

png(paste(ROOT_PLOTS, '/', company_name, '_exp_vxreg', '_entit.png', sep = ''), 700, 500)
plot_coef(entit_coefs, 
          entit_coefs$vxreg1, 
          entit_coefs$vxreg1_std, 'named entities', ylims = c(0, 0.0003))
dev.off()

png(paste(ROOT_PLOTS, '/', company_name, '_exp_vxreg', '_triples.png', sep = ''), 700, 500)
plot_coef(triples_coefs, 
          triples_coefs$vxreg1, 
          triples_coefs$vxreg1_std, 'SOV-triples', ylims = c(0, 0.0003))
dev.off()



# Fit using sliding window

base_roll = fit_roll(base_spec, stock_data, refit.window = 'moving')
finam_roll = fit_roll(finam_spec, stock_data, refit.window = 'moving')
entit_roll = fit_roll(entit_spec, stock_data, refit.window = 'moving')
triples_roll = fit_roll(triples_spec, stock_data, refit.window = 'moving')

base_coefs <- get_coefs(base_roll)
png(paste(ROOT_PLOTS, '/', company_name, '_sl_coefs', '_base.png', sep = ''), 1200, 400)
plot_garch_coefs(base_coefs, 'GARCH(1,1)')
dev.off()

finam_coefs <- get_coefs(finam_roll, include_external = TRUE, external = c('vxreg1'))
png(paste(ROOT_PLOTS, '/', company_name, '_sl_coefs', '_finam.png', sep = ''), 1200, 400)
plot_garch_coefs(finam_coefs, 'human-preprocessed news')
dev.off()

entit_coefs <- get_coefs(entit_roll, include_external = TRUE, external = c('vxreg1'))
png(paste(ROOT_PLOTS, '/', company_name, '_sl_coefs', '_entit.png', sep = ''), 1200, 400)
plot_garch_coefs(entit_coefs,  'named entities')
dev.off()

triples_coefs <- get_coefs(triples_roll, include_external = TRUE, external = c('vxreg1'))
png(paste(ROOT_PLOTS, '/', company_name, '_sl_coefs', '_triples.png', sep = ''), 1200, 400)
plot_garch_coefs(triples_coefs, 'SOV-triples')
dev.off()


png(paste(ROOT_PLOTS, '/', company_name, '_sl_vxreg', '_finam.png', sep = ''), 700, 500)
plot_coef(finam_coefs, 
          finam_coefs$vxreg1, 
          finam_coefs$vxreg1_std, 'human-preprocessed news', ylims = c(0, 0.0003))
dev.off()

png(paste(ROOT_PLOTS, '/', company_name, '_sl_vxreg', '_entit.png', sep = ''), 700, 500)
plot_coef(entit_coefs, 
          entit_coefs$vxreg1, 
          entit_coefs$vxreg1_std, 'named entities', ylims = c(0, 0.0003))
dev.off()

png(paste(ROOT_PLOTS, '/', company_name, '_sl_vxreg', '_triples.png', sep = ''), 700, 500)
plot_coef(triples_coefs, 
          triples_coefs$vxreg1, 
          triples_coefs$vxreg1_std, 'SOV-triples', ylims = c(0, 0.0003))
dev.off()

print(fpm(base_roll))
print(fpm(finam_roll))
print(fpm(entit_roll))
print(fpm(triples_roll))

# Criteria from preprint

R_h <- data.frame(
  index = double(),
  base = double(),
  finam = double(),
  entit = double(),
  triples = double()
)


for (h in seq(1, 50)){
  
  base <- get_R_h(spec = base_spec, data = stock_data[,'returns'], h = h) 
  finam <- get_R_h(spec = finam_spec, data = stock_data[,'returns'], h = h, 
                       external.forecasts = stock_data[,'finam'])
  triples <- get_R_h(spec = entit_spec, data = stock_data[,'returns'], h = h, 
                       external.forecasts = stock_data[,'triples'])
  entit <- get_R_h(spec = triples_spec, data = stock_data[,'returns'], h = h, 
                         external.forecasts = stock_data[,'entit'])
  R_h[h, 'index'] = h
  
  R_h[h, 'base'] = sum(base) / length(base)
  R_h[h, 'finam'] = sum(finam) / length(finam)
  R_h[h, 'entit'] = sum(entit) / length(entit)
  R_h[h, 'triples'] = sum(triples) / length(triples)
  
  R_h[h, 'base_finam_equal'] <- dm.test(base$value, finam$value, h = h)$p.value
  R_h[h, 'base_entit_equal'] <- dm.test(base$value , entit$value, h = h)$p.value
  R_h[h, 'base_triples_equal'] <- dm.test(base$value , triples$value, h = h)$p.value

}

R_h$finam_base = R_h$finam / R_h$base 
R_h$returns_base = R_h$returns / R_h$base 
R_h$entit_base = R_h$entit / R_h$base 
R_h$triples_base = R_h$triples / R_h$base

# R(h) plot

png(paste(ROOT_PLOTS, '/', company_name, '_r_h_1', '.png', sep = ''), 700, 500)

ggplot(R_h, aes(index, finam_base, color='black')) +
  geom_line() +
  geom_line(aes(index, entit_base, color = "blue")) +
  geom_line(aes(index, triples_base, color = "red")) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  xlab('h') + ylab('R(h) ratio') +
  scale_colour_manual(name = 'R(h) ratio', 
                      values = c('black'='black', 'red'='red', 'blue'='blue'), 
                      labels = c('finam/base', 'triples/base', 'entities/base'))

dev.off()

# Hypothesis of estimates equality plot

png(paste(ROOT_PLOTS, '/', company_name, '_r_h_eq', '.png', sep = ''), 700, 500)

ggplot(R_h, aes(index, base_finam_equal, color='black')) +
  geom_line() +
  geom_line(aes(index, base_entit_equal, color = "blue")) +
  geom_line(aes(index, base_triples_equal, color = "red")) +
  geom_hline(yintercept = 0.05, linetype = "dotted") +
  xlab('h') + ylab('p-value') +
  scale_colour_manual(name = 'DM test', 
                      values = c('black'='black', 'red'='red', 'blue'='blue'), 
                      labels = c('base finam equal', 'base triples equal', 'base entities equal')
                      )

dev.off()


