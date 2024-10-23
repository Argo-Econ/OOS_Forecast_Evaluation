# ------------------ -----------------------------------------------------------/
# Evaluación de pronóstico ----
# OOS - Out-of-Sample Evaluation
# Arturo Yesid González Peña
# ------------------ -----------------------------------------------------------/



## Carga de librerias ----

pacman::p_load(readxl, forecast, TSstudio, janitor, glue, Metrics
               ,lmtest, tsoutliers, GGally, xts, ggplot2, dplyr, tseries)


## Definición rutas ----
path_in  <- "Datos_ent/"
path_out >- "DAtos_Sal/"

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
## humm!!!
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

Datos_dlx <- Datos_ent |> mutate( across(where(is.numeric), function(x) log(x) - log(lag(x,1)) )) |> 
             select(-Fecha) |> xts(order.by = as.Date(Datos_ent$Fecha))

Datos_slx <- Datos_ent |> mutate( across(where(is.numeric), function(x) log(x) - log(lag(x,12)) )) |> 
             select(-Fecha) |> xts(order.by = as.Date(Datos_ent$Fecha))

Datos_dlx1 <- Datos_ent |> mutate( across(where(is.numeric), function(x) log(x) )) |> 
              select(-Fecha) |> xts(order.by = as.Date(Datos_ent$Fecha)) |> diff.xts(lag = 1,differences = 1)

Datos_slx1 <- Datos_ent |> mutate( across(where(is.numeric), function(x) log(x) )) |> 
              select(-Fecha) |> xts(order.by = as.Date(Datos_ent$Fecha)) |> diff.xts(lag = 12,differences = 1)

