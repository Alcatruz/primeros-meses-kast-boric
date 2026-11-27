# Dos arranques

Qué se dijo en la Sala de la Cámara de Diputados de Chile en los primeros meses de Boric (2022) y de Kast (2026), y cómo se relaciona con la manera en que votó cada diputado. Keyness, redes de coocurrencia, concordancias y una posición desde el texto, comparada con la posición B-Call.

**📖 [Leer el análisis →](https://alcatruz.github.io/primeros-meses-kast-boric/)**

El post está en `index.html` y se publica con GitHub Pages.

## Contenido

| Ruta | Qué es |
|---|---|
| `index.html` | El post, con gráficos interactivos (Plotly) |
| `R/01_texto.R` | Keyness (G², chi² y log-odds) de izquierda y derecha en cada período, palabras del bloque y del rol, redes de coocurrencia, citas y TF-IDF de los diputados que se apartan |
| `R/02_datos.R` | Escribe `datos/datos.js` |
| `datos/datos.js` | Lo único que lee la página: la posición B-Call y la del texto de cada diputado, las palabras que separan a los bloques, las redes y las citas. No lleva los discursos |

## Datos y decisiones

- Discursos en la Sala de Labor Parlamentaria (BCN), del 11 de marzo al 9 de septiembre de cada año, y votaciones de proyectos de ley de la Cámara (datos abiertos de la Cámara), del 11 de marzo al 25 de noviembre.
- Las estimaciones y las medidas salen de un proyecto local que guarda los datos ya bajados y corre el mismo análisis para cada gobierno. Esos archivos no están en este repositorio. Los scripts de `R/` se corren desde la raíz de ese proyecto, en orden.

## Referencia

Toro-Maureira, S., Reutter, J., Valenzuela, L., Alcatruz, D. y Valenzuela, M. (2025). B-Call: integrating ideological position and voting cohesion in legislative behavior. *Frontiers in Political Science, 7*, 1670089. https://doi.org/10.3389/fpos.2025.1670089
