# =============================================================================
# 01_texto.R. Los discursos en la Sala de la Cámara en los primeros meses de
# Boric (2022) y de Kast (2026), con las medidas estándar del análisis de
# texto, y su relación con la posición B-Call.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, después de correr
# contingencia_legislativa/R/05_discursos.R para los dos gobiernos:
#   Rscript post_primeros_meses/R/01_texto.R
#
# LEE (solo lectura)
#   contingencia_legislativa/datos/{boric,kast}/discursos.rds, los discursos
#     de diputados en ejercicio entre el 11-03 y el 09-09, sin quien preside,
#     con el texto original y el limpio (reglas de ideologia_texto).
#   contingencia_legislativa/resultados/{boric,kast}/discursos_oradores.rds,
#     la posición desde el texto (análisis de correspondencias, fijado en
#     contingencia antes de compararlo con B-Call) y d1 de cada diputado.
#   ideologia_texto/diccionarios, las palabras vacías.
#   outputs/data/post_boric_kast/textos_medidas.rds, las expresiones de dos
#     palabras aprendidas con la Cámara 2002-2022 ("derechos humanos"), que
#     se unen en un término. No se aprenden de nuevo con estos discursos.
#
# BLOQUES Y ROLES
#   Bloque es el lado de la coalición del diputado: en 2022 el oficialismo es
#   la izquierda y la oposición la derecha, en 2026 al revés. Los diputados
#   de fuera de las coaliciones no entran en la keyness ni en las redes.
#
# MEDIDAS (las de post_boric_kast/R/08_texto_medidas.R)
#   Keyness de la izquierda frente a la derecha en cada período, con
#   quanteda.textstats::textstat_keyness: G² (measure = "lr") y chi²
#   (measure = "chi2"), con sus correcciones por defecto, y el log-odds con
#   previa de Dirichlet informativa, con los conteos de toda la Cámara del
#   período como previa (alfa0 = 10.000). Términos usados al menos 8 veces,
#   en al menos 4 discursos y por al menos 10 diputados del período.
#   Clase de cada término, con el valor crítico de G² para p < 0,001 (10,83)
#   en los dos períodos:
#     bloque izquierda  más de la izquierda en 2022 y en 2026
#     bloque derecha    más de la derecha en 2022 y en 2026
#     gobierno          más de la izquierda en 2022 y de la derecha en 2026
#     oposición         más de la derecha en 2022 y de la izquierda en 2026
#   Redes de coocurrencia (quanteda::fcm, ventana de 5 palabras, con las
#   vacías como hueco para que la distancia sea la del texto) entre los 30
#   términos con mayor G² de cada bloque y período, con las 60 aristas de más
#   peso.
#   Concordancias (quanteda::kwic) del término más firme de cada clase, el de
#   mayor G² mínimo entre los dos períodos. Una cita por período, sacada al
#   azar con semilla fija entre las apariciones en los discursos del bloque
#   que la usa más, con 12 palabras a cada lado, del texto original.
#   TF-IDF por diputado (dfm_tfidf, frecuencia relativa por la inversa de la
#   frecuencia de documentos), para describir a los diputados cuyo texto queda
#   más cerca del otro bloque.
#
# ESCRIBE
#   outputs/data/post_primeros_meses/texto.rds  todo lo anterior, para revisar
#   y R/02_datos.R pasa lo que usa la página a datos/datos.js
#   (ese script escribe el archivo)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(quanteda)
  library(quanteda.textstats)
  library(igraph)
})
quanteda_options(threads = 4)

CONT <- "contingencia_legislativa"
OUT  <- "outputs/data/post_primeros_meses"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
SEMILLA <- 20261127
CRITICO <- 10.83
GOB <- c(boric = "Boric", kast = "Kast")

# --- Discursos de la Cámara ------------------------------------------------------
d <- bind_rows(lapply(names(GOB), function(g)
  readRDS(file.path(CONT, "datos", g, "discursos.rds")) %>% mutate(g = GOB[[g]]))) %>%
  filter(cam == "D") %>%
  mutate(docid = paste(g, id_discurso),
         bloque = case_when(coalicion == "otro" ~ NA_character_,
                            (g == "Boric") == (coalicion == "oficialismo") ~ "izquierda",
                            TRUE ~ "derecha"),
         rol = case_when(coalicion == "oficialismo" ~ "gobierno", coalicion == "oposicion" ~ "oposición"))
stopifnot(!anyDuplicated(d$docid))
print(d %>% count(g, bloque, rol) %>% as.data.frame())

leer <- function(f) { x <- readLines(f, encoding = "UTF-8", warn = FALSE); x[nzchar(trimws(x))] }
vacias <- c(leer("ideologia_texto/diccionarios/vacias_nltk_es.txt"), leer("ideologia_texto/diccionarios/procedimentales.txt"))
expresiones <- readRDS("outputs/data/post_boric_kast/textos_medidas.rds")$expresiones$expresion

corp <- corpus(d %>% select(docid, limpio, g, bloque, rol, id_leg), docid_field = "docid", text_field = "limpio")
toks0 <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_compound(phrase(expresiones), concatenator = "_")
toks <- toks0 %>% tokens_select("^[[:alpha:]_]{3,}$", valuetype = "regex") %>% tokens_remove(vacias)
x <- dfm(toks)
cat("términos:", nfeat(x), " discursos:", ndoc(x), "\n")

# --- Keyness por período ---------------------------------------------------------
keyness <- function(gg, alfa0 = 10000, min_n = 8, min_doc = 4, min_dip = 10) {
  sel <- d$g == gg & !is.na(d$bloque)
  m <- x[sel, ]
  # textstat_keyness() da "Invalid DFM object" con estos metadatos de
  # documento; sin ellos calcula igual, porque el objetivo va como vector
  docvars(m) <- NULL
  obj <- d$bloque[sel] == "izquierda"
  lr <- textstat_keyness(m, target = obj, measure = "lr", sort = FALSE)
  ch <- textstat_keyness(m, target = obj, measure = "chi2", sort = FALSE)
  stopifnot(identical(lr$feature, featnames(m)), identical(ch$feature, featnames(m)))
  dip <- colSums(dfm_group(dfm_weight(m, scheme = "boolean"), groups = d$id_leg[sel]) > 0)
  f <- colSums(x[d$g == gg, ])
  na <- sum(lr$n_target); nb <- sum(lr$n_reference)
  tibble(g = gg, termino = lr$feature, izq = as.numeric(lr$n_target), der = as.numeric(lr$n_reference),
         discursos = as.numeric(colSums(m > 0)), diputados = as.numeric(dip), fondo = as.numeric(f[lr$feature]),
         g2 = lr$G2, chi2 = ch$chi2) %>%
    filter(izq + der >= min_n, discursos >= min_doc, diputados >= min_dip) %>%
    mutate(alfa = alfa0 * fondo / sum(f),
           logodds = log((izq + alfa) / (na + alfa0 - izq - alfa)) - log((der + alfa) / (nb + alfa0 - der - alfa)),
           z = logodds / sqrt(1 / (izq + alfa) + 1 / (der + alfa))) %>%
    filter(alfa > 0) %>%
    select(g, termino, izq, der, diputados, g2, chi2, z)
}
key <- bind_rows(keyness("Boric"), keyness("Kast"))

# Acuerdo entre medidas: de los 20 términos con más peso de cada lado, cuántos
# comparten G², chi² y log-odds
acuerdo <- key %>% mutate(lado = ifelse(g2 > 0, "izquierda", "derecha")) %>%
  group_by(g, lado) %>%
  group_modify(function(t, k) {
    top <- function(v) t$termino[order(-abs(t[[v]]))][1:20]
    tibble(g2_chi2 = length(intersect(top("g2"), top("chi2"))), g2_logodds = length(intersect(top("g2"), top("z"))),
           las_tres = length(Reduce(intersect, list(top("g2"), top("chi2"), top("z")))),
           spearman_g2_z = round(cor(abs(t$g2), abs(t$z), method = "spearman"), 2))
  }) %>% ungroup()
print(as.data.frame(acuerdo))

# --- Bloque o rol ------------------------------------------------------------------
ambos <- key %>% select(g, termino, g2, izq, der) %>%
  pivot_wider(names_from = g, values_from = c(g2, izq, der)) %>%
  filter(!is.na(g2_Boric), !is.na(g2_Kast)) %>%
  mutate(clase = case_when(g2_Boric > CRITICO & g2_Kast > CRITICO ~ "bloque izquierda",
                           g2_Boric < -CRITICO & g2_Kast < -CRITICO ~ "bloque derecha",
                           g2_Boric > CRITICO & g2_Kast < -CRITICO ~ "gobierno",
                           g2_Boric < -CRITICO & g2_Kast > CRITICO ~ "oposición",
                           TRUE ~ NA_character_),
         firmeza = pmin(abs(g2_Boric), abs(g2_Kast)))
cat("\ntérminos en los dos períodos:", nrow(ambos), " Spearman de G² con signo:",
    round(cor(ambos$g2_Boric, ambos$g2_Kast, method = "spearman"), 3), "\n")
print(ambos %>% count(clase) %>% as.data.frame())
print(ambos %>% filter(!is.na(clase)) %>% group_by(clase) %>% arrange(desc(firmeza), .by_group = TRUE) %>%
        summarise(terminos = paste(head(termino, 15), collapse = ", "), .groups = "drop") %>% as.data.frame())
# Significativos en un período y no en el otro
solo_uno <- ambos %>% mutate(s22 = abs(g2_Boric) > CRITICO, s26 = abs(g2_Kast) > CRITICO) %>% count(s22, s26)
print(as.data.frame(solo_uno))

# --- Redes de coocurrencia ------------------------------------------------------------
toks_pad <- toks0 %>% tokens_select("^[[:alpha:]_]{3,}$", valuetype = "regex", padding = TRUE) %>%
  tokens_remove(vacias, padding = TRUE)
red <- function(gg, bl, n_nodos = 30, n_aristas = 60) {
  k <- key %>% filter(g == gg) %>% mutate(propio = if (bl == "izquierda") g2 else -g2) %>%
    filter(propio > CRITICO) %>% slice_max(propio, n = n_nodos, with_ties = FALSE)
  tq <- tokens_subset(toks_pad, g == gg & !is.na(bloque) & bloque == bl)
  m <- fcm(tq, context = "window", window = 5, tri = TRUE) %>% fcm_select(pattern = k$termino, valuetype = "fixed")
  mm <- as.matrix(m); mm[lower.tri(mm, diag = TRUE)] <- 0
  ij <- which(mm > 0, arr.ind = TRUE)
  ar <- tibble(de = rownames(mm)[ij[, 1]], a = colnames(mm)[ij[, 2]], peso = mm[ij]) %>%
    filter(de != a) %>% slice_max(peso, n = n_aristas, with_ties = FALSE)
  gr <- graph_from_data_frame(ar, directed = FALSE, vertices = tibble(name = k$termino))
  set.seed(SEMILLA)
  xy <- layout_with_fr(gr, weights = log1p(E(gr)$peso))
  list(nodos = tibble(termino = k$termino, g2 = k$propio, n = if (bl == "izquierda") k$izq else k$der,
                      x = xy[, 1], y = xy[, 2]) %>%
         left_join(ambos %>% select(termino, clase), by = "termino"),
       aristas = ar)
}
redes <- list()
for (gg in GOB) for (bl in c("izquierda", "derecha")) redes[[paste(gg, bl)]] <- red(gg, bl)

# --- Concordancias -------------------------------------------------------------------
# El término más firme de cada clase y, en cada período, el bloque que lo usa más
toks_raw <- tokens(corpus(d %>% mutate(texto = str_squish(gsub("\\\\n", " ", texto))) %>%
                            select(docid, texto, g, bloque),
                          docid_field = "docid", text_field = "texto"))
elegidos <- ambos %>% filter(!is.na(clase)) %>% group_by(clase) %>% slice_max(firmeza, n = 1, with_ties = FALSE) %>% ungroup()
quien_usa <- function(clase, gg) {
  switch(clase, "bloque izquierda" = "izquierda", "bloque derecha" = "derecha",
         "gobierno" = if (gg == "Boric") "izquierda" else "derecha",
         "oposición" = if (gg == "Boric") "derecha" else "izquierda")
}
citas <- bind_rows(lapply(seq_len(nrow(elegidos)), function(i) bind_rows(lapply(GOB, function(gg) {
  bl <- quien_usa(elegidos$clase[i], gg)
  k <- kwic(tokens_subset(toks_raw, g == gg & !is.na(bloque) & bloque == bl),
            pattern = phrase(gsub("_", " ", elegidos$termino[i])), window = 12, case_insensitive = TRUE)
  set.seed(SEMILLA)
  as_tibble(k) %>% slice_sample(n = 1) %>%
    transmute(clase = elegidos$clase[i], termino = elegidos$termino[i], g = gg, bloque = bl,
              docid = docname, antes = pre, palabra = keyword, despues = post)
})))) %>%
  left_join(d %>% select(docid, nombre, partido, fecha), by = "docid")
print(as.data.frame(citas %>% select(clase, g, nombre, partido, fecha, antes, palabra, despues)))

# --- Texto y voto ---------------------------------------------------------------------
# La posición desde el texto y d1, de contingencia. Para describir a quienes
# hablan más parecido al otro bloque, sus términos con mayor TF-IDF frente al
# resto de la Cámara del período.
oradores <- bind_rows(lapply(names(GOB), function(g)
  readRDS(file.path(CONT, "resultados", g, "discursos_oradores.rds")) %>% mutate(g = GOB[[g]]))) %>%
  filter(cam == "D", !is.na(texto1), !is.na(d1)) %>%
  mutate(texto1 = as.numeric(texto1),
         bloque_coal = case_when(coalicion == "otro" ~ "otro",
                                 (g == "Boric") == (coalicion == "oficialismo") ~ "izquierda", TRUE ~ "derecha"))
corr <- oradores %>% group_by(g) %>%
  group_modify(function(t, k) bind_rows(
    tibble(grupo = "toda la Cámara", n = nrow(t), r = cor(t$d1, t$texto1)),
    t %>% filter(bloque_coal != "otro") %>% group_by(grupo = bloque_coal) %>%
      summarise(n = n(), r = cor(d1, texto1), .groups = "drop"))) %>% ungroup()
print(as.data.frame(corr))
# Dentro de cada bloque, el diputado cuyo texto queda más hacia el otro lado
cruzados <- oradores %>% filter(bloque_coal != "otro") %>% group_by(g, bloque_coal) %>%
  slice_max(if_else(bloque_coal == "derecha", -texto1, texto1), n = 2, with_ties = FALSE) %>% ungroup()
tf <- bind_rows(lapply(seq_len(nrow(cruzados)), function(i) {
  sel <- d$g == cruzados$g[i]
  m <- dfm_group(x[sel, ], groups = d$id_leg[sel])
  t <- dfm_tfidf(m, scheme_tf = "prop", scheme_df = "inverse")
  v <- as.numeric(t[docnames(t) == cruzados$id_leg[i], ])
  o <- order(-v)[1:10]
  tibble(g = cruzados$g[i], id_leg = cruzados$id_leg[i], terminos = paste(featnames(t)[o], collapse = ", "))
}))
cruzados <- cruzados %>% left_join(tf, by = c("g", "id_leg"))
print(as.data.frame(cruzados %>% select(g, bloque_coal, nombre, partido, discursos, d1, texto1, terminos)))

saveRDS(list(key = key, acuerdo = acuerdo, ambos = ambos, redes = redes, citas = citas,
             oradores = oradores, corr = corr, cruzados = cruzados, n_discursos = d %>% count(g, bloque)),
        file.path(OUT, "texto.rds"))
cat("\nescrito", file.path(OUT, "texto.rds"), "\n")
