# install.packages("pacman")
pacman::p_load(tidyverse,
               haven,
               janitor,
               openxlsx)


## dataframe direccion 1 ##
# 1) seleccionamos las variables que tienen relación a la primera direccion. 
#    Sin embargo, eliminamos un grupo que no necesitamos

dir1 <- df %>% 
  select(SbjNum, 31:85) %>%             # seleccion de variables.
  select(-starts_with(c("I_",           # no consideramos las vars 
                        "p_seleccion",  # que comienzan con (-starts_with) 
                        "esta_",        # estos nombres.
                        "T_agenda", 
                        "acepta_", 
                        "k_in", 
                        "proxima_hora"))) %>% 
  clean_names()                   # estandarizamos los nombres de las vars



# 2) convertimos todas las variables de la data en  a character 
#    para no tener conlfictos de formato

dir1 <- setNames(data.frame(
  lapply(dir1,     # seleccionamos nuestro df
         as.character)),  # asignacion de clase (puede cambiarse)
  colnames(dir1))  # aplicar a funcion a los nombres de las variables

# con "lapply" le decimos a R que aplique esa funcion a cada elemento de dir1



# 3) quitar guiones de los nombres de las variables. 
#    Con gsub podemos "buscar y reemplazar

colnames(dir1) <- 
  gsub("_", "",        # buscamos todos los guiones y los cambiamos por "nada"
       colnames(dir1)) # aplicar a funcion a los nombres de las variable



# 4) quitamos los numeros de las variables
colnames(dir1) <- 
  gsub("[1-5]", "",    # cambiamos todos los numeros de 1 a 5 presentes en los nombres de las variables
       colnames(dir1)) # aplicar a funcion a los nombres de las variables



# 5) cambiamos el nombre de la variable que identifica otro tipo de vivienda
names(dir1)[6] <- "D1otrotipovivienda"
# names(nuestro_df)[variable posicion 6] <- "nuevo_nombre_de_variable"



## 6) agregar identificador de direccion y visita ##
# ".*^" significa "antes de comenzar el nombre".
# entonces, agregamos los valores X_ antes de los nombres de las variables

# Ej: colnames(nuestro_df)[c(variable 2 a variable 6)] <-
# gsub("buscamos el inicio del nombre", "reemplazamos por un 1_",
# colnames(nuestro_df[c(variable 2 a variable 6)]))

colnames(dir1)[c(2:6)] <- gsub(".*^", "1_", colnames(dir1[2:6]))        
colnames(dir1)[c(7:11)] <- gsub(".*^", "1_", colnames(dir1[7:11]))   
colnames(dir1)[c(12:16)] <- gsub(".*^", "2_", colnames(dir1[12:16])) 
colnames(dir1)[c(17:20)] <- gsub(".*^", "3_", colnames(dir1[17:20]))



# 7) transformar la base de datos dir1 a formato "larga" y luego a "ancha", 
#    es decir, primero convertimos columnas a filas, y luego filas a columnas

dir1 <- dir1 %>%
  tidyr::pivot_longer(                 # funcion transforma a "larga"
    cols = c(2:6),                     # variables que convertiremos a fila
    names_to = c("I_dir", "variable"), # nombre de variables nuevas
    names_sep = "_",                   # explicacion  detallada más abajo
    values_to = "value",               # nueva columna con los datos de las variables 2:6 (calles, numero, etc)
    values_ptypes = list(value = character())) %>% # coercionamos datos de "value" a character
  pivot_wider(               # funcion transforma a "ancha"
    names_from  = variable,  # convertimos los datos de "variable" a columnas
    values_from = value)     # indicamos que los datos de estas nuevas variables se encuentran en la columna "value"

# De esta manera solo el indicador de la direccion queda en las filas,
# y mantiene el resto de variables en las columnas

## EXPLICACION "NAMES_SEP": si recuerdan, mas arriba asignamos identificadores a las variables según direccion. En este caso es "1_variable" (direccion1_variable), y como se observa, esta se encuentra separada con un guion. Entonces, con names_sep le decimos a R que este será nuestro parametro de distinsion, de esta forma se crean las variables "I_dir" y "variable". R entiende que de "1_variable","1" será el valor de "I_dir" y "variable" (direccion, dirnumero, gse, etc) serán los valores de la columna "variable".

dir2 <- df %>% 
  select(SbjNum, 86:140) %>% 
  select(-starts_with(c("I_", "p_seleccion", "esta_", "T_agenda", "acepta_", "k_in", "proxima_hora"))) %>% 
  clean_names()

# variables a character
dir2 <- setNames(data.frame(lapply(dir2, as.character)), 
                 colnames(dir2))

# quitar guiones de las variables
colnames(dir2) <- gsub("_", "", colnames(dir2))

# quitamos los 1, 2 y 3 de las variables
colnames(dir2) <- gsub("[1-5]", "", colnames(dir2))

# cambiamos el nombre de la variable que identifica otro tipo de vivienda
names(dir2)[6] <- "D2otrotipovivienda"

### agregar identificador de visita
# direccion 1
colnames(dir2)[c(2:6)] <- gsub(".*^", "2_", colnames(dir2[2:6]))     
colnames(dir2)[c(7:11)] <- gsub(".*^", "1_", colnames(dir2[7:11]))   
colnames(dir2)[c(12:16)] <- gsub(".*^", "2_", colnames(dir2[12:16])) 
colnames(dir2)[c(17:20)] <- gsub(".*^", "3_", colnames(dir2[17:20]))

# pivot direccion
dir2 <- dir2 %>%
  tidyr::pivot_longer(cols = c(2:6),
                      names_to     = c("I_dir", "variable"),
                      names_sep = "_",
                      values_to = "value",
                      values_ptypes = list(value = character())) %>% 
  pivot_wider(names_from  = variable,
              values_from = value)



dir3 <- df %>% 
  select(SbjNum, 141:195) %>% 
  select(-starts_with(c("I_", "p_seleccion", "esta_", "T_agenda", "acepta_", "k_in", "proxima_hora"))) %>% 
  clean_names()

# variables a character
dir3 <- setNames(data.frame(lapply(dir3, as.character)), 
                 colnames(dir3))

# quitar guiones de las variables
colnames(dir3) <- gsub("_", "", colnames(dir3))

# quitamos los 1, 2 y 3 de las variables
colnames(dir3) <- gsub("[1-5]", "", colnames(dir3))

# cambiamos el nombre de la variable que identifica otro tipo de vivienda
names(dir3)[6] <- "D3otrotipovivienda"

### agregar identificador de visita
# direccion 1
colnames(dir3)[c(2:6)] <- gsub(".*^", "3_", colnames(dir3[2:6]))     # 0
colnames(dir3)[c(7:11)] <- gsub(".*^", "1_", colnames(dir3[7:11]))   # 1
colnames(dir3)[c(12:16)] <- gsub(".*^", "2_", colnames(dir3[12:16])) # 2
colnames(dir3)[c(17:20)] <- gsub(".*^", "3_", colnames(dir3[17:20]))

# pivot direccion
dir3 <- dir3 %>%
  tidyr::pivot_longer(cols = c(2:6),
                      names_to     = c("I_dir", "variable"),
                      names_sep = "_",
                      values_to = "value",
                      values_ptypes = list(value = character())) %>% 
  pivot_wider(names_from  = variable,
              values_from = value)



dir4 <- df %>% 
  select(SbjNum, 196:250) %>% 
  select(-starts_with(c("I_", "p_seleccion", "esta_", "T_agenda", "acepta_", "k_in", "proxima_hora"))) %>% 
  clean_names()

# variables a character
dir4 <- setNames(data.frame(lapply(dir4, as.character)), 
                 colnames(dir4))

# quitar guiones de las variables
colnames(dir4) <- gsub("_", "", colnames(dir4))

# quitamos los 1, 2 y 3 de las variables
colnames(dir4) <- gsub("[1-5]", "", colnames(dir4))

# cambiamos el nombre de la variable que identifica otro tipo de vivienda
names(dir4)[6] <- "D4otrotipovivienda"

### agregar identificador de visita
# direccion 1
colnames(dir4)[c(2:6)] <- gsub(".*^", "4_", colnames(dir4[2:6]))     # 0
colnames(dir4)[c(7:11)] <- gsub(".*^", "1_", colnames(dir4[7:11]))   # 1
colnames(dir4)[c(12:16)] <- gsub(".*^", "2_", colnames(dir4[12:16])) # 2
colnames(dir4)[c(17:20)] <- gsub(".*^", "3_", colnames(dir4[17:20]))

# pivot direccion
dir4 <- dir4 %>%
  tidyr::pivot_longer(cols = c(2:6),
                      names_to     = c("I_dir", "variable"),
                      names_sep = "_",
                      values_to = "value",
                      values_ptypes = list(value = character())) %>% 
  pivot_wider(names_from  = variable,
              values_from = value)



dir5 <- df %>% 
  select(SbjNum, 251:305) %>% 
  select(-starts_with(c("I_", "p_seleccion", "esta_", "T_agenda", "acepta_", "k_in", "proxima_hora"))) %>% 
  clean_names()

# variables a character
dir5 <- setNames(data.frame(lapply(dir5, as.character)), 
                 colnames(dir5))

# quitar guiones de las variables
colnames(dir5) <- gsub("_", "", colnames(dir5))

# quitamos los 1, 2 y 3 de las variables
colnames(dir5) <- gsub("[1-5]", "", colnames(dir5))

# cambiamos el nombre de la variable que identifica otro tipo de vivienda
names(dir5)[6] <- "D5otrotipovivienda"

### agregar identificador de visita
# direccion 1
colnames(dir5)[c(2:6)] <- gsub(".*^", "5_", colnames(dir5[2:6]))     # 0
colnames(dir5)[c(7:11)] <- gsub(".*^", "1_", colnames(dir5[7:11]))   # 1
colnames(dir5)[c(12:16)] <- gsub(".*^", "2_", colnames(dir5[12:16])) # 2
colnames(dir5)[c(17:20)] <- gsub(".*^", "3_", colnames(dir5[17:20]))

# pivot direccion
dir5 <- dir5 %>%
  tidyr::pivot_longer(cols = c(2:6),
                      names_to     = c("I_dir", "variable"),
                      names_sep = "_",
                      values_to = "value",
                      values_ptypes = list(value = character())) %>% 
  pivot_wider(names_from  = variable,
              values_from = value)


## ordenar variables de dataframes por posicion
## las variables deben ser vectores nombrados, 
## FORMA DE UTILIZAR LA FUNCION; c("nombre.variable.A"= 1,
##                                 "nombre.variable.B" = 12,
##                                 "nombre.variable.C = X)

arrange.vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}


# unimos todas los df de direcciones en una sola base de datos
# utilizamos full_join para unir todos los datos presentes en los df

a <- full_join(dir1, dir2)  # unimos dir1 y dir2 en el objeto "a"
b <- full_join(a, dir3)     # unimos "a" (dir1,dir2) en el objeto "b"
c <- full_join(b, dir4)     # unimos "b" (dir1,dir2,dir3) en el objeto "c"

df_viviendas <-       # nombre de bd final
  full_join(c, dir5)  # unimos "c"(dir1,dir2,dir3, dir4) con dir5 
# en el objeto "df_viviendas"



# ordenamos nuestra base final según folio e indicador de direccion
df_viviendas <- df_viviendas %>% 
  arrange(sbjnum, I_dir)



# mantenemos solo las filas que contengan datos en fecha 1
df_viviendas <- df_viviendas %>%
  filter(df_viviendas$`1_fecha`!=0)



# Para este caso tenemos dos variables que indican lo mismo,
# pero estan codificadas de diferente manera
# usamos coalesce() para unir estas dos vars en una

# a) dirnumero
df_viviendas$dirnumero <- 
  coalesce(df_viviendas$dirnumero, # variable destino
           df_viviendas$numero)    # variable de donde sacamos los datos

# b) codigodisposicion
df_viviendas$`1_codigodisposicion` <- 
  coalesce(df_viviendas$`1_codigodisposicion`, # variable destino
           df_viviendas$`1_codigodispsicion`)  # variable de donde sacamos los datos

# eliminamos las variables que ya no usaremos
df_viviendas$numero <- NULL
df_viviendas$`1_codigodispsicion` <- NULL



# ordenamos las columnas con un orden especifico,
# utilizando a funcion personalizada cargada anteriormente
df_viviendas <- arrange.vars(df_viviendas, c("I_dir" = 2, 
                                             "direccion" = 3,
                                             "dirnumero" = 4,
                                             "gse" = 5,
                                             "tipovivienda" = 6,
                                             "D1otrotipovivienda" = 7,
                                             "D2otrotipovivienda" = 8,
                                             "D3otrotipovivienda" = 9,
                                             "D4otrotipovivienda" = 10,
                                             "D5otrotipovivienda" = 11))

# eliminamos otras variables "comentario"
df_viviendas[26:29] <- NULL


# creamos una nueva variable que contenga las incidencias finales
# el valor "120" indica "cambiar fecha", entonces, para este caso
# CDF_FINAL solo contendrá los valores de la última fecha registrada
df_viviendas <- 
  df_viviendas %>% 
  mutate(CDF_FINAL = case_when(
    .$`1_codigodisposicion` != 120 ~ .$`1_codigodisposicion`,
    .$`2_codigodisposicion` != 120 ~ .$`2_codigodisposicion`,
    .$`3_codigodisposicion` != 120 ~ .$`3_codigodisposicion`
  ))



# limpiamos los nombres de las variables para no tener conflictos al
# exportar la base en formato SPSS
df_viviendas <- clean_names(df_viviendas)


# exportar base de datos (quitar comentario para cada caso requerido)
# funcion(df_viviendas, "nombre_BBDD.x)

# write_sav(df_viviendas, "BBDD_viviendas.sav")    # formato SPSS
# write.csv(df_viviendas, "BBDD_viviendas.csv")    # formato csv
# write.xlsx(df_viviendas, "BBDD_viviendas.xlsx")  # formato xlsx


