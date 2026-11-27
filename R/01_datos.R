# =============================================================================
# 01_datos.R. Los datos del post, comparando los primeros meses de Boric
# (11-03-2022 al 25-11-2022) y de Kast (11-03-2026 al 25-11-2026).
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, después de correr
# contingencia_legislativa para los dos gobiernos:
#   Rscript post_primeros_meses/R/01_datos.R
#
# LEE (solo lectura)
#   contingencia_legislativa/datos/{boric,kast}/legisladores.rds
#   contingencia_legislativa/resultados/{boric,kast}/*.rds
#   Ahí están las decisiones de cada medida (ver su docs/estado.md). Lo que
#   este script agrega son los cruces entre los dos períodos.
#
# ESCRIBE
#   post_primeros_meses/datos/datos.js, lo único que lee la página. Lleva
#   agregados, la posición B-Call de cada diputado y las palabras de las
#   tablas, no las bases.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(jsonlite)
})

CONT <- "contingencia_legislativa"
REPO <- "post_primeros_meses/datos"
dir.create(REPO, showWarnings = FALSE, recursive = TRUE)
GOB <- c(boric = "Boric", kast = "Kast")
PRES <- c(boric = "15383-05", kast = "18702-05")
leer <- function(g, f, carpeta = "resultados") readRDS(file.path(CONT, carpeta, g, paste0(f, ".rds")))
por_gob <- function(f, ...) bind_rows(lapply(names(GOB), function(g) f(g, ...) %>% mutate(g = GOB[[g]], .before = 1)))
r3 <- function(x) round(x, 3)

# --- Escaños --------------------------------------------------------------------
escanos <- por_gob(function(g) leer(g, "legisladores", "datos") %>% count(cam = camara, coal = coalicion))

# --- Posiciones B-Call en la Cámara, con todas las votaciones -------------------
puntos <- por_gob(function(g) leer(g, "puntos_ideales") %>%
  filter(version == "todas", camara == "D", !pocos_votos) %>%
  transmute(n = nombre, pa = partido, co = coalicion, d1 = r3(d1), d2 = r3(d2)))

# --- Votaciones: divididas, derrotas y Rice ------------------------------------
ind <- por_gob(function(g) {
  v <- leer(g, "votacion_resumen") %>% mutate(ambito = ifelse(boletin == PRES[[g]], "presupuesto", "resto"))
  resumir <- function(d) d %>% summarise(n = n(), div = mean(dividida, na.rm = TRUE),
                                         of_der = mean(oficialismo_derrotado, na.rm = TRUE),
                                         op_der = mean(oposicion_derrotada, na.rm = TRUE),
                                         n_div = sum(dividida %in% TRUE),
                                         gana_of_div = mean(!oficialismo_derrotado[dividida %in% TRUE], na.rm = TRUE),
                                         rice_of = mean(rice_oficialismo, na.rm = TRUE),
                                         rice_op = mean(rice_oposicion, na.rm = TRUE), .groups = "drop")
  bind_rows(v %>% group_by(cam = camara, ambito) %>% resumir(),
            v %>% group_by(cam = camara) %>% resumir() %>% mutate(ambito = "todas"))
}) %>% mutate(across(c(div, of_der, op_der, gana_of_div, rice_of, rice_op), r3))

# --- El primer presupuesto -------------------------------------------------------
# La votación en general en la Cámara: la primera de tipo General del
# presupuesto. Quienes cambian de bloque: B-Call solo con las votaciones del
# presupuesto contra el de las demás, sin quienes votan en menos del 10%.
pres <- por_gob(function(g) {
  v <- leer(g, "votacion_resumen") %>% filter(boletin == PRES[[g]])
  gen <- v %>% filter(camara == "D", tipo_votacion %in% c("General", "General y particular")) %>%
    arrange(fecha, id_votacion) %>% slice(1)
  cam <- leer(g, "presupuesto_bcall") %>% filter(!pocos_votos, bloque_pres != bloque_resto)
  tibble(boletin = PRES[[g]], fecha_general = gen$fecha, si = gen$si, no = gen$no, abst = gen$abstencion,
         cambian_d = sum(cam$camara == "D"), cambian_s = sum(cam$camara == "S"),
         cambian = paste(sprintf("%s (%s)", cam$nombre[cam$camara == "D"], cam$partido[cam$camara == "D"]), collapse = "; "))
})
# El diputado del oficialismo que más votó contra su coalición en el presupuesto
discolo <- por_gob(function(g) leer(g, "contra_coalicion") %>%
  filter(camara == "D", ambito == "presupuesto", coalicion == "oficialismo") %>%
  slice_max(tasa, n = 1, with_ties = FALSE) %>% transmute(nombre, partido, tasa = r3(tasa), n))

# --- Agenda del Ejecutivo --------------------------------------------------------
agenda <- por_gob(function(g) {
  ing <- leer(g, "actividad_proyectos") %>% group_by(iniciativa) %>% summarise(n = sum(n), .groups = "drop")
  urg <- leer(g, "actividad_urgencias") %>% group_by(tipo) %>% summarise(n = sum(n), .groups = "drop")
  ley <- leer(g, "leyes_publicadas")
  tibble(mensajes = ing$n[ing$iniciativa == "Mensaje"], mociones = ing$n[ing$iniciativa == "Moción"],
         urgencias = sum(urg$n), discusion_inmediata = urg$n[urg$tipo == "Discusión inmediata"],
         suma = urg$n[urg$tipo == "Suma"], simple = urg$n[urg$tipo == "Simple"],
         leyes = nrow(ley), leyes_mensaje_propio = sum(ley$iniciativa == "Mensaje" & ley$ingresado_en == "este gobierno"))
})
# Urgencias acumuladas por mes de gobierno (marzo es el mes 1)
urg <- por_gob(function(g) leer(g, "actividad_urgencias") %>%
  group_by(mes) %>% summarise(n = sum(n), .groups = "drop") %>% arrange(mes) %>%
  transmute(m = row_number(), mes = substr(mes, 6, 7), n, acum = cumsum(n)))

# --- Palabras de cada bloque en la Cámara ----------------------------------------
# z positivo es oficialismo. En 2022 el oficialismo es la izquierda y en 2026
# la derecha, así que se pasa a izquierda y derecha. Persiste es estar entre
# las 50 más típicas del mismo bloque en el otro período.
pal <- bind_rows(
  leer("boric", "discursos_palabras") %>% filter(camara == "D") %>% transmute(g = "Boric", palabra, z_izq = z),
  leer("kast", "discursos_palabras") %>% filter(camara == "D") %>% transmute(g = "Kast", palabra, z_izq = -z))
comunes <- pal %>% count(palabra) %>% filter(n == 2) %>% pull(palabra)
top <- function(d, lado, k) d %>% filter(palabra %in% comunes) %>%
  arrange(if (lado == "izquierda") desc(z_izq) else z_izq) %>% head(k) %>% pull(palabra)
palabras <- bind_rows(lapply(c("Boric", "Kast"), function(gg) bind_rows(lapply(c("izquierda", "derecha"), function(lado) {
  otro <- top(pal %>% filter(g != gg), lado, 50)
  d <- pal %>% filter(g == gg, palabra %in% comunes)
  tibble(g = gg, lado = lado, palabra = top(d, lado, 12)) %>%
    mutate(persiste = palabra %in% otro, orden = row_number())
}))))
anchos <- pal %>% filter(palabra %in% comunes) %>% pivot_wider(names_from = g, values_from = z_izq)
solape <- bind_rows(lapply(c("izquierda", "derecha"), function(lado) tibble(
  lado = lado,
  mismo_bloque = length(intersect(top(pal %>% filter(g == "Boric"), lado, 50), top(pal %>% filter(g == "Kast"), lado, 50))),
  otro_bloque = length(intersect(top(pal %>% filter(g == "Boric"), lado, 50),
                                 top(pal %>% filter(g == "Kast"), setdiff(c("izquierda", "derecha"), lado), 50))))))
texto <- list(palabras_comunes = length(comunes),
              spearman = r3(cor(anchos$Boric, anchos$Kast, method = "spearman")),
              solape = solape,
              discursos = por_gob(function(g) leer(g, "discursos_resumen")$cobertura %>%
                                    transmute(cam, discursos, oradores, desde = format(desde), hasta = format(hasta))))

# --- Escribir ---------------------------------------------------------------------
datos <- list(escanos = escanos, puntos = puntos, ind = ind, pres = pres, discolo = discolo,
              agenda = agenda, urg = urg, palabras = palabras, texto = texto)
writeLines(c("// Generado por R/01_datos.R. No editar a mano.",
             paste0("window.PM = ", toJSON(datos, auto_unbox = TRUE, dataframe = "rows", na = "null", Date = "ISO8601"), ";")),
           file.path(REPO, "datos.js"), useBytes = TRUE)
cat("escrito", file.path(REPO, "datos.js"), round(file.size(file.path(REPO, "datos.js")) / 1024), "KB\n\n")

# Para el texto
options(width = 200)
print(as.data.frame(escanos %>% pivot_wider(names_from = coal, values_from = n)))
print(as.data.frame(ind))
print(as.data.frame(pres %>% select(-cambian))); cat(pres$cambian, sep = "\n")
print(as.data.frame(discolo)); print(as.data.frame(agenda))
print(as.data.frame(urg %>% select(g, m, acum) %>% pivot_wider(names_from = g, values_from = acum)))
print(as.data.frame(palabras %>% group_by(g, lado) %>% summarise(p = paste0(palabra, ifelse(persiste, "*", ""), collapse = " "), .groups = "drop")))
str(texto[c("palabras_comunes", "spearman")]); print(as.data.frame(solape)); print(as.data.frame(texto$discursos))
