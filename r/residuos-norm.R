# ------------------------------------------------- #
#        Análisis de Residuos Studentizados         #
# ------------------------------------------------- #

library(ggplot2)

diagnorm = function(fit.model){
  # fit.model: objeto con el resultado del modelo normal 
  # lineal homocedástico obtenido a través de la función "lm"
  
  if (!is(fit.model, "lm")) stop("fit.model debe ser un objeto retornado por la función lm.")

  X = model.matrix(fit.model)
  n = nrow(X)
  p = ncol(X)
  H = X%*%solve(t(X)%*%X)%*%t(X)
  h = diag(H)
  lms = summary(fit.model)
  s = lms$sigma
  r = resid(lms)
  ts = r/(s*sqrt(1-h))
  di = (1/p)*(h/(1-h))*(ts^2)
  si = lm.influence(fit.model)$sigma
  tsi = r/(si*sqrt(1-h))
  a = max(tsi)
  b = min(tsi)
  #
  dados2 = data.frame(x=1:length(tsi), tsi, fit=fitted(fit.model))
  f1 = ggplot(dados2, aes(x=x, y=tsi)) + geom_point() +
    geom_hline(yintercept=c(-2, 0, 2), linetype="dashed", color="red") + 
    labs(x="Índice", y="Residuos Studentizados") + theme_bw()
  #
  f2 = ggplot(dados2, aes(x=fit, y=tsi)) + geom_point() +
    geom_hline(yintercept=c(-2, 0, 2), linetype="dashed", color="red") + 
    labs(x="Valores ajustados", y="Residuos Studentizados") + theme_bw()
  #
  f3 = ggplot(dados2, aes(x=tsi, y=after_stat(density))) + geom_histogram(bins=11, fill="grey", color="black") +
    labs(x="Residuos Studentizados", y="Densidad") + theme_bw()
  #
  ident = diag(n)
  epsilon = matrix(0,n,100)
  e  = matrix(0,n,100)
  e1 = numeric(n)
  e2 = numeric(n)
  #
  for(i in 1:100){
    epsilon[,i] = rnorm(n,0,1)
    e[,i] = (ident - H)%*%epsilon[,i]
    u = diag(ident - H)
    e[,i] = e[,i]/sqrt(u)
    e[,i] = sort(e[,i]) }
  #
  for(i in 1:n){
    eo = sort(e[i,])
    e1[i] = (eo[2]+eo[3])/2
    e2[i] = (eo[97]+eo[98])/2 }
  #
  med = apply(e,1,mean)
  faixa = range(tsi,e1,e2)
  #
  dados3 = data.frame(e1, e2, med)
  q1 = data.frame(qqnorm(tsi, plot.it=FALSE))
  q2 = data.frame(qqnorm(e1, plot.it=FALSE))
  q3 = data.frame(qqnorm(e2, plot.it=FALSE))
  q4 = data.frame(qqnorm(med, plot.it=FALSE))
  
  f4 = ggplot() + geom_line(data=q2, aes(x=x, y=y)) + ylim(faixa) +
    geom_line(data=q3, aes(x=x, y=y)) + geom_line(data=q4, aes(x=x, y=y), color="red", linetype="dashed") +
    geom_point(data=q1, aes(x=x, y=y), size=0.6) + labs(x="Percentiles de la N(0,1)", y="Residuos Studentizados") + theme_bw()
  
  gridExtra::grid.arrange(f1, f2, f3, f4, nrow=2)
  
  return(tsi)
}
