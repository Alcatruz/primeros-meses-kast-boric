# Dos arranques

Los primeros ocho meses del gobierno de Kast en el Congreso de Chile, comparados con el mismo tramo del gobierno de Boric. Del 11 de marzo al 25 de noviembre de 2022 y de 2026, con B-Call, las votaciones divididas, el primer presupuesto de cada gobierno, las urgencias y los discursos en la Sala.

El post está en `index.html` y se publica con GitHub Pages.

## Contenido

| Ruta | Qué es |
|---|---|
| `index.html` | El post, con gráficos interactivos (Plotly) |
| `R/01_datos.R` | Cruza los resultados de los dos períodos y escribe `datos/datos.js` |
| `datos/datos.js` | Lo único que lee la página: agregados, la posición B-Call de cada diputado y las palabras de las tablas |

## Datos y decisiones

- Votaciones de proyectos de ley de la Cámara (datos abiertos de la Cámara) y del Senado (API del Senado), proyectos, urgencias y leyes de la ficha de tramitación, y discursos en la Sala de Labor Parlamentaria (BCN).
- Las estimaciones y las medidas salen de un proyecto local que guarda los datos ya bajados y corre el mismo análisis para cada gobierno. Esos archivos no están en este repositorio. `R/01_datos.R` se corre desde la raíz de ese proyecto.

## Referencia

Toro-Maureira, S., Reutter, J., Valenzuela, L., Alcatruz, D. y Valenzuela, M. (2025). B-Call: integrating ideological position and voting cohesion in legislative behavior. *Frontiers in Political Science, 7*, 1670089. https://doi.org/10.3389/fpos.2025.1670089
