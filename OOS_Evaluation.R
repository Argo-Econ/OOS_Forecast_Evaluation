# ------------------ -----------------------------------------------------------/
# Evaluación de pronóstico ----
# OOS - Out-of-Sample Evaluation
# Arturo Yesid González Peña
# ------------------ -----------------------------------------------------------/



## Carga de librerias ----

pacman::p_load(readxl, forecast, TSstudio, janitor, glue, Metrics, tsoutliers
               ,lmtest, tsoutliers, GGally, xts, ggplot2, dplyr, tseries)


## Definición rutas ----
path_in  <- "Datos_ent/"
path_out <- "Datos_Sal/"

## Importar datos ----
list.files(path = path_in)
Datos_ent <- read_xlsx(path = glue("{path_in}Base_Modelos.xlsx")
                       ,sheet = "Base",range = "a2:g300",col_names = T)
tail(Datos_ent)


# Transformaciones -----
## metodo 1 ----
numeric_col <- names(Datos_ent)[sapply(Datos_ent, is.numeric)]
Datos_YoY1  <- Datos_ent |>  mutate_at(vars(all_of(numeric_col)), function(x) (x/lag(x,n = 12) - 1)) |> 
                select(-Fecha) |> xts(x = , order.by = as.Date(Datos_ent$Fecha))

## metodo 2 ----
Datos_YoY2 <- Datos_ent |> mutate(across(where(is.numeric), function(x) (x/lag(x,n = 12)) -1 )) |> 
              select(-Fecha) |> xts(order.by = as.Date(Datos_ent$Fecha))

## metodo 3 ----
## preservando las variables originales
Datos_YoY3 <- Datos_ent |> mutate(across(where(is.numeric), 
                                         list(Var_YoY = function(x) (x/lag(x, n = 12)) - 1),
                                         .names = "{.col}_var_yoy"
                                          )) |> select(-Fecha) |> 
              xts(,order.by = as.Date(Datos_ent$Fecha))

## metodo 4 ----
## Humm!!!
Datos_YoY4 <- (Datos_ent[13:nrow(Datos_ent),-1]/Datos_ent[1:(nrow(Datos_ent) - 12),-1]) - 1
Datos_YoY4 <- ts(Datos_YoY4,start = c(2001,1),frequency = 12)
tail(Datos_YoY4)

## Graficos ----
GGally::ggpairs(as.data.frame(Datos_YoY2))

windows()
plot(Datos_YoY4[,1])

windows()
Datos_YoY4 |> autoplot(facets = T) + xlab("Fecha") + ylab("Valor")

windows()
tsdisplay(Datos_YoY3$IPC_var_yoy,main = "Inflación Colombia"
          ,xlab = "Fecha",ylab = "% anual")

ts_plot(Datos_YoY3,type = "single")
ts_plot(Datos_YoY2,type = "multiple")

ts_seasonal(Datos_YoY1$IPC)
ts_seasonal(Datos_YoY1$IPC,type = "all")
ts_seasonal(Datos_YoY3$IPC_var_yoy,type = "all")
ts_surface(Datos_YoY3$IPC_var_yoy)

ts_lags(Datos_YoY3$IPC_var_yoy)
ts_cor(na.omit(Datos_YoY4[,1]))

# Identificación ----
test <- adf.test(na.omit(Datos_YoY4[,1]))
test$p.value

## test estacionariedad ----

## dickey-fuller -> H0: serie no estacionaria
apply(na.omit(Datos_YoY4), 2, function(x) adf.test(x)$p.value)
## Philliph Penrron -> H0: serie no estacionaria
apply(na.omit(Datos_YoY4), 2, function(x) pp.test(x)$p.value)
## Kwiatkowski-Phillips-Schmidt-Shin (KPSS) -> H0: serie estacionaria
apply(na.omit(Datos_YoY4), 2, function(x) kpss.test(x)$p.value)

# Transformación log ----

Datos_lx  <- Datos_ent |> mutate( across(where(is.numeric), function(x) log(x)) ) |> 
             select(-Fecha) |> ts(start = c(2000,1), frequency = 12) |> na.omit()
class(Datos_lx)

Datos_dlx <- Datos_ent |> mutate( across(where(is.numeric), function(x) log(x) - log(lag(x,1)) )) |> 
             select(-Fecha) |> xts(order.by = as.Date(Datos_ent$Fecha))

Datos_slx <- Datos_ent |> mutate( across(where(is.numeric), function(x) log(x) - log(lag(x,12)) )) |> 
             select(-Fecha) |> xts(order.by = as.Date(Datos_ent$Fecha))

Datos_dlx1 <- Datos_ent |> mutate( across(where(is.numeric), function(x) log(x) )) |> 
              select(-Fecha) |> xts(order.by = as.Date(Datos_ent$Fecha)) |> diff.xts(lag = 1,differences = 1)

Datos_slx1 <- Datos_ent |> mutate( across(where(is.numeric), function(x) log(x) )) |> 
              select(-Fecha) |> xts(order.by = as.Date(Datos_ent$Fecha)) |> diff.xts(lag = 12,differences = 1)

## dickey-fuller -> H0: serie no estacionaria
apply(na.omit(Datos_dlx1), 2, function(x) adf.test(x)$p.value)
## Philliph Penrron -> H0: serie no estacionaria
apply(na.omit(Datos_dlx1), 2, function(x) pp.test(x)$p.value)
## Kwiatkowski-Phillips-Schmidt-Shin (KPSS) -> H0: serie estacionaria
apply(na.omit(Datos_dlx1), 2, function(x) kpss.test(x)$p.value)

## Definición de estructura -----
TSA::eacf(na.omit(Datos_dlx$IPC),ar.max = 10,ma.max = 10)
# Candidatos posibles

# Modelo SARIMAX --------------------------------------------------------------
# -----------------------------------------------------------------------------/

## mod1 ----
mod1 <- Arima(y = Datos_lx[,1] ,order = c(2,1,2),seasonal = c(1,0,0),xreg = Datos_lx[,-1]
              ,method = "CSS")
summary(mod1)
lmtest::coeftest(mod1) 
checkresiduals(mod1)

# -----------------------------------------------------------------------------/
# -----------------------------------------------------------------------------/
# Ajuste por outliers -----
# Detección de outliers
# tipos de outliers
# -----------------------------------------------------------------------------/

# - Additive outliers (AO)      - función pulso
# - Level Shift       (LS)
# - Transient change  (TC)      - Cambio de nivel
# - Innovation Ouliers (IO)     - Cambio progresivo
# - Seasonal level Shifts (SLS)

ts_plot(Datos_lx[,1])

outliers_IPC_lx <- tso(Datos_lx[,1], types = c("TC", "AO", "LS", "IO") )
windows()
plot(outliers_IPC_lx)


Datos_lx <- Datos_lx |> as.data.frame() |> cbind(as.data.frame(outliers_IPC_lx$yadj)) |> 
            rename(IPC_adj=x) |> relocate(IPC_adj) |> ts(start = c(2000,1), frequency = 12)
            
tail(Datos_lx)
# --------------------------------------------------------------/





# mod2 ----
tail(Datos_lx)
tictoc::tic()
mod2 <- auto.arima(y = Datos_lx[,1] ,max.order = 8
                   ,stationary = F, stepwise = T
                   ,trace = T
                   ,xreg = Datos_lx[,-c(1,2)]
                   ,method = "CSS-ML")
tictoc::toc()
summary(mod2)
lmtest::coeftest(mod2) 
checkresiduals(mod2)


# mod3 ----
mod3 <- Arima(y = Datos_lx[,2],order = c(1,1,0),seasonal = c(1,0,0))
summary(mod3)
lmtest::coeftest(mod3) 
checkresiduals(mod3)


# -----------------------------------------------------------------------------/
# Eval fuera de muestra, OOS ----
# -----------------------------------------------------------------------------#
# mod1 -> Modelo SARIMAX con exogenas serie original IPC en log
# mod2 -> Modelo ARIMAX con exogenas serie ajustada IPC en log (auto)
# mod3 -> modelo ARIMA sin exogenas IPC original

# -----------------------------------------------------------------------------#

## 1. Matriz de errores -----
tail(Datos_lx)

# datos de entrada
base_in <- na.omit(Datos_lx)
exogenas <- base_in[,-c(1,2)]

## Recorte es para 3 años (36 datos de muestra 1 paso adelante, 35 a dos pasos adelante-
## 34 a tres pasos adelante, etc.

recorte_fm <- (nrow(base_in) - 36)/nrow(base_in)   # 88% de los datos
horizonte  <- 24       # maximo nivel para el horizonte de pronostico
pronostico <- 36       # número de simulaciones par el calculo de erores OOS

rmse_err_mod1  <- rmse_err_mod2 <- rmse_err_mod3 <- matrix(NA, pronostico,horizonte)

colnames(rmse_err_mod1) <- sprintf(fmt = "RMSE_f_%d",seq(1,horizonte))
rownames(rmse_err_mod1) <- paste("Simul",seq(1:pronostico),sep = "_")
View(rmse_err_mod1)


## 2. Bucle RMSE mod1 ----
# -----------------------------------------------------------------------------#
head(Datos_lx)

# mod1 -> ARIMA(2,1,2)(1,0,0)[12]

j <- i <- 1 

### bucle para cambio del horizonte de pronostico
tictoc::tic()
for(j in 1:horizonte){
  ### reseteo de base para simulaciones
  base_train <- base_in[(1:round(nrow(base_in)*recorte_fm)),] |> 
                ts(start = c(2000,1),frequency = 12)
  dim(base_train)
  base_train_exo <- exogenas[(1:round(nrow(exogenas)*recorte_fm)),] |> 
                    ts(start = c(2000,1),frequency = 12)
  dim(base_train_exo)
  
  ### bucle para recorrer toda la serie de evaluación
  for(i in 1:(pronostico-j+1)) {
    
    mod1_sim <- Arima(base_train[,2], order = c(2,1,2),seasonal = c(1,0,0)
                      , xreg = base_train_exo
                      , method="CSS")  
    pronos_ind <- exogenas[(nrow(base_train)+1):(nrow(base_train)+j),]
    
    ### pronóstico puntual
    fore_mod1_sim <- if(j==1){forecast(mod1_sim, xreg=t(pronos_ind))} else{forecast(mod1_sim, xreg= pronos_ind)} 
    
    ### Calculo del error OOS 
    rmse_err  <- rmse(actual = base_in[(nrow(base_train)+1):(nrow(base_train)+j),2]
                      ,predicted = fore_mod1_sim$mean)
    
    ### ubicar resultado RMSE en lugar de la matriz
    rmse_err_mod1[i,j] <- rmse_err
    
    ### ampliación de un nuevo dato para reiniciar la estimación y pronóstico
    base_train <- rbind(base_train
                        ,base_in[(nrow(base_train)+1):(nrow(base_train)+1) ,2])
    base_train_exo <- rbind(base_train_exo
                            ,exogenas[(nrow(base_train_exo)+1):(nrow(base_train_exo)+1),])
    
  }
  ### imprime la base de erorres fuera de muestra cada vez que culmina todas las simulaciones
  ### a un horizonte de pronóstico dado
  print(rmse_err_mod1)
}
tictoc::toc()

View(rmse_err_mod1)

## 2.1 Exportar resutltados ----
xlsx::write.xlsx(rmse_err_mod1,glue("{path_out}Out-of-Sample_mod1.xlsx"))
write.csv(rmse_err_mod1,glue("{path_out}Out-of-Sample_mod1.csv"))
# -----------------------------------------------------------------------------/


## 3. Bucle RMSE mod2 ----
# -----------------------------------------------------------------------------#
colnames(rmse_err_mod2) <- sprintf(fmt = "RMSE_f_%d",seq(1,horizonte))
rownames(rmse_err_mod2) <- paste("Simul",seq(1:pronostico),sep = "_")

# mod2 -> ARIMA(2,1,2)(1,0,0)[12] con serie ajustada


### bucle para cambio del horizonte de pronostico
tictoc::tic()
for(j in 1:horizonte){
  ### reseteo de base para simulaciones
  base_train <- base_in[(1:round(nrow(base_in)*recorte_fm)),] |> 
    ts(start = c(2000,1),frequency = 12)
  dim(base_train)
  base_train_exo <- exogenas[(1:round(nrow(exogenas)*recorte_fm)),] |> 
    ts(start = c(2000,1),frequency = 12)
  dim(base_train_exo)
  
  ### bucle para recorrer toda la serie de evaluación
  for(i in 1:(pronostico-j+1)) {
    
    
    mod2_sim <- auto.arima(base_train[,1],stationary = F
                      , stepwise = T,trace = F
                      , xreg = base_train_exo
                      , method="CSS-ML")  
    
    pronos_ind <- exogenas[(nrow(base_train)+1):(nrow(base_train)+j),]
    
    ### pronóstico puntual
    fore_mod2_sim <- if(j==1){forecast(mod2_sim, xreg=t(pronos_ind))} else{forecast(mod2_sim, xreg= pronos_ind)} 
    
    ### Calculo del error OOS 
    rmse_err  <- rmse(actual = base_in[(nrow(base_train)+1):(nrow(base_train)+j),1]
                      ,predicted = fore_mod2_sim$mean)
    
    ### ubicar resultado RMSE en lugar de la matriz
    rmse_err_mod2[i,j] <- rmse_err
    
    ### ampliación de un nuevo dato para reiniciar la estimación y pronóstico
    base_train <- rbind(base_train
                        ,base_in[(nrow(base_train)+1):(nrow(base_train)+1) ,1])
    base_train_exo <- rbind(base_train_exo
                            ,exogenas[(nrow(base_train_exo)+1):(nrow(base_train_exo)+1),])
    
  }
  ### imprime la base de erorres fuera de muestra cada vez que culmina todas las simulaciones
  ### a un horizonte de pronóstico dado
  print(rmse_err_mod2)
}
tictoc::toc()

View(rmse_err_mod2)

## 3.1 Exportar resutltados ----
xlsx::write.xlsx(rmse_err_mod2,glue("{path_out}Out-of-Sample_mod2.xlsx"))
write.csv(rmse_err_mod2,glue("{path_out}Out-of-Sample_mod2.csv"))
# -----------------------------------------------------------------------------/


## 4. Bucle RMSE mod3 ----
# -----------------------------------------------------------------------------#
colnames(rmse_err_mod3) <- sprintf(fmt = "RMSE_f_%d",seq(1,horizonte))
rownames(rmse_err_mod3) <- paste("Simul",seq(1:pronostico),sep = "_")

head(Datos_lx)
# mod3 -> ARIMA(2,1,2)(1,0,0)[12] con serie original sin exogenas


### bucle para cambio del horizonte de pronostico
tictoc::tic()
for(j in 1:horizonte){
  ### reseteo de base para simulaciones
  base_train <- base_in[(1:round(nrow(base_in)*recorte_fm)),] |> 
    ts(start = c(2000,1),frequency = 12)
  dim(base_train)

  
  ### bucle para recorrer toda la serie de evaluación
  for(i in 1:(pronostico-j+1)) {
    
    mod3_sim <- Arima(y = base_train[,2],order = c(1,1,0),seasonal = c(1,0,0))
      
    
    ### pronóstico puntual
    fore_mod3_sim <- if(j==1){forecast(mod3_sim,h=j)} else{forecast(mod3_sim,h=j)} 
    
    ### Calculo del error OOS 
    rmse_err  <- rmse(actual = base_in[(nrow(base_train)+1):(nrow(base_train)+j),2]
                      ,predicted = fore_mod1_sim$mean)
    
    ### ubicar resultado RMSE en lugar de la matriz
    rmse_err_mod3[i,j] <- rmse_err
    
    ### ampliación de un nuevo dato para reiniciar la estimación y pronóstico
    base_train <- rbind(base_train
                        ,base_in[(nrow(base_train)+1):(nrow(base_train)+1) ,2])
    
  }
  ### imprime la base de erorres fuera de muestra cada vez que culmina todas las simulaciones
  ### a un horizonte de pronóstico dado
  print(rmse_err_mod3)
}
tictoc::toc()

View(rmse_err_mod3)

## 4.1 Exportar resutltados ----
openxlsx::write.xlsx(rmse_err_mod3,glue("{path_out}Out-of-Sample_mod3.xlsx"))
write.csv(rmse_err_mod3,glue("{path_out}Out-of-Sample_mod3.csv"))
# -----------------------------------------------------------------------------/





# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#
# Evaluacion de la estructura de errores fuera de muestra de cada modelo
#
# se crea un vector con el estadistico y el p-valor de la prueba para cada hori-
# zonte de tiempo, desde 1 paso adelante, hasta 12 pasos adelante
# -----------------------------------------------------------------------------#

# Pruebas de Dieblod & Mariano ----
## mod1 vs mod2 ----
salidaDM_1vs2 <- NULL

i<-1

for(i in 1:horizonte){
  salida <-  cbind("esta. prueba" = dm.test(e1 = na.omit(rmse_err_mod1[,i])
                                          ,e2 = na.omit(rmse_err_mod2[,i])
                                          ,h = i
                                          ,alternative=c("less")
                                          ,varestimator = "bartlett")$statistic
                   ,"p-valor" = dm.test(e1 = na.omit(rmse_err_mod1[,i])
                                        ,e2 = na.omit(rmse_err_mod2[,i])
                                        ,h = i
                                        ,alternative=c("less")
                                        ,varestimator = "bartlett")$p.value
                   )
  salidaDM_1vs2 <- rbind(salidaDM_1vs2,salida)
}
rownames(salidaDM_1vs2) <- paste("DMtest",seq(1:horizonte),sep = "_")


## salida prueba -----
openxlsx::write.xlsx(salidaDM_1vs2,glue("{path_out}DMtest_mod1_vs_mod2.xlsx"))
View(salidaDM_1vs2)


# Pruebas de Dieblod & Mariano -
## mod2 vs mod3 ----
salidaDM_2vs3 <- NULL

i<-1

for(i in 1:horizonte){
  salida <-  cbind("esta. prueba" = dm.test(e1 = na.omit(rmse_err_mod2[,i])
                                            ,e2 = na.omit(rmse_err_mod3[,i])
                                            ,h = i
                                            ,alternative=c("less")
                                            ,varestimator = "bartlett")$statistic
                   ,"p-valor" = dm.test(e1 = na.omit(rmse_err_mod2[,i])
                                        ,e2 = na.omit(rmse_err_mod3[,i])
                                        ,h = i
                                        ,alternative=c("less")
                                        ,varestimator = "bartlett")$p.value
  )
  salidaDM_2vs3 <- rbind(salidaDM_2vs3,salida)
}
rownames(salidaDM_2vs3) <- paste("DMtest",seq(1:horizonte),sep = "_")


## salida prueba -----
openxlsx::write.xlsx(salidaDM_2vs3,glue("{path_out}DMtest_mod2_vs_mod3.xlsx"))
View(salidaDM_2vs3)



# Pruebas de Dieblod & Mariano -
## mod1 vs mod3 ----
salidaDM_1vs3 <- NULL

i<-1

for(i in 1:horizonte){
  salida <-  cbind("esta. prueba" = dm.test(e1 = na.omit(rmse_err_mod1[,i])
                                            ,e2 = na.omit(rmse_err_mod3[,i])
                                            ,h = i
                                            ,alternative=c("less")
                                            ,varestimator = "bartlett")$statistic
                   ,"p-valor" = dm.test(e1 = na.omit(rmse_err_mod1[,i])
                                        ,e2 = na.omit(rmse_err_mod3[,i])
                                        ,h = i
                                        ,alternative=c("less")
                                        ,varestimator = "bartlett")$p.value
  )
  salidaDM_1vs3 <- rbind(salidaDM_1vs3,salida)
}
rownames(salidaDM_1vs3) <- paste("DMtest",seq(1:horizonte),sep = "_")


## salida prueba -----
openxlsx::write.xlsx(salidaDM_1vs3,glue("{path_out}DMtest_mod1_vs_mod3.xlsx"))
View(salidaDM_1vs3)


# Para el tema de varianza negativa en la prueba de diebold / Mariano ver
# https://www.sciencedirect.com/science/article/abs/pii/S0169207017300559

# Fin eval fuera de muestra ----
# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#





