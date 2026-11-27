# =============================================================================
# 02_datos.R. Lo que lee la página, en datos/datos.js.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, después de
# R/01_texto.R:
#   Rscript post_primeros_meses/R/02_datos.R
#
# LEE (solo lectura)
#   contingencia_legislativa/resultados/{boric,kast}/puntos_ideales.rds, B-Call
#     con todas las votaciones de proyectos de ley del 11-03 al 25-11.
#   contingencia_legislativa/resultados/{boric,kast}/discursos_ca_palabras.rds,
#     la coordenada de cada palabra en la posición desde el texto.
#   outputs/data/post_primeros_meses/texto.rds, de R/01_texto.R.
#
# ESCRIBE
#   post_primeros_meses/datos/datos.js. Lleva la posición B-Call y la del
#   texto de cada diputado, las palabras que separan a los bloques en al
#   menos un período, las redes, las citas y los polos del texto. No lleva
#   los discursos.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(jsonlite)
})

CONT <- "contingencia_legislativa"
REPO <- "post_primeros_meses/datos"
dir.create(REPO, showWarnings = FALSE, recursive = TRUE)
GOB <- c(boric = "Boric", kast = "Kast")
CRITICO <- 10.83
t <- readRDS("outputs/data/post_primeros_meses/texto.rds")
r1 <- function(x) round(x, 1); r3 <- function(x) round(x, 3)
lado <- function(g, co) case_when(co == "otro" ~ "otro", (g == "Boric") == (co == "oficialismo") ~ "izquierda", TRUE ~ "derecha")

# --- Posiciones B-Call en la Cámara ---------------------------------------------
puntos <- bind_rows(lapply(names(GOB), function(g) readRDS(file.path(CONT, "resultados", g, "puntos_ideales.rds")) %>%
  filter(version == "todas", camara == "D", !pocos_votos) %>%
  transmute(g = GOB[[g]], n = nombre, pa = partido, co = coalicion, d1 = r3(d1), d2 = r3(d2))))

# --- Palabras: las que separan a los bloques en al menos un período ----------------
pal <- t$ambos %>%
  filter(abs(g2_Boric) > CRITICO | abs(g2_Kast) > CRITICO) %>%
  transmute(t = str_replace_all(termino, "_", " "), x = r1(g2_Boric), y = r1(g2_Kast), c = clase,
            n22 = izq_Boric + der_Boric, n26 = izq_Kast + der_Kast, f = r1(firmeza))
resumen <- list(
  terminos = nrow(t$ambos),
  solo_2022 = sum(abs(t$ambos$g2_Boric) > CRITICO & abs(t$ambos$g2_Kast) <= CRITICO),
  solo_2026 = sum(abs(t$ambos$g2_Boric) <= CRITICO & abs(t$ambos$g2_Kast) > CRITICO),
  los_dos = sum(abs(t$ambos$g2_Boric) > CRITICO & abs(t$ambos$g2_Kast) > CRITICO),
  spearman = r3(cor(t$ambos$g2_Boric, t$ambos$g2_Kast, method = "spearman")),
  clases = t$ambos %>% filter(!is.na(clase)) %>% count(clase),
  acuerdo = t$acuerdo,
  discursos = t$n_discursos)

# --- Redes ------------------------------------------------------------------------------
redes <- lapply(t$redes, function(r) list(
  nodos = r$nodos %>% transmute(t = str_replace_all(termino, "_", " "), x = r3(x), y = r3(y), n, c = clase),
  aristas = r$aristas %>% transmute(de = str_replace_all(de, "_", " "), a = str_replace_all(a, "_", " "), p = peso)))

# --- Citas ------------------------------------------------------------------------------
# Las palabras de kwic vienen separadas por espacios; se juntan con la
# puntuación como en el texto.
juntar <- function(x) x %>%
  str_replace_all("\\s+([,.;:!?)\\]”»])", "\\1") %>%
  str_replace_all("([(\\[“«¿¡])\\s+", "\\1") %>%
  str_squish()
citas <- t$citas %>%
  transmute(clase, g, bloque, nombre, partido, fecha = format(fecha),
            texto = juntar(paste(antes, palabra, despues)), palabra = str_replace_all(termino, "_", " "))

# --- Texto y voto -------------------------------------------------------------------------
oradores <- t$oradores %>%
  transmute(g, n = nombre, pa = partido, l = lado(g, coalicion), d1 = r3(d1), t1 = r3(texto1), disc = discursos)
polos <- bind_rows(lapply(names(GOB), function(g) {
  w <- readRDS(file.path(CONT, "resultados", g, "discursos_ca_palabras.rds")) %>%
    filter(cam == "D") %>% filter(masa > median(masa))
  tibble(g = GOB[[g]],
         izquierda = paste(head(w$palabra[order(w$texto1)], 12), collapse = ", "),
         derecha = paste(head(w$palabra[order(-w$texto1)], 12), collapse = ", "))
}))
cruzados <- t$cruzados %>%
  transmute(g, l = bloque_coal, n = nombre, pa = partido, d1 = r3(d1), t1 = r3(texto1), terminos)

# --- Escribir ----------------------------------------------------------------------------
datos <- list(puntos = puntos, pal = pal, resumen = resumen, redes = redes, citas = citas,
              oradores = oradores, corr = t$corr %>% mutate(r = r3(r)), polos = polos, cruzados = cruzados)
writeLines(c("// Generado por R/02_datos.R. No editar a mano.",
             paste0("window.PM = ", toJSON(datos, auto_unbox = TRUE, dataframe = "rows", na = "null"), ";")),
           file.path(REPO, "datos.js"), useBytes = TRUE)
cat("escrito", file.path(REPO, "datos.js"), round(file.size(file.path(REPO, "datos.js")) / 1024), "KB\n\n")

options(width = 220)
str(resumen[c("terminos", "solo_2022", "solo_2026", "los_dos", "spearman")])
print(as.data.frame(resumen$clases))
print(as.data.frame(citas %>% select(clase, g, nombre, fecha, texto)))
print(as.data.frame(polos))
print(as.data.frame(t$corr))
