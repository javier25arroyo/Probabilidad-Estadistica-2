# Práctica 3 - Pruebas de Hipótesis

Contenido en un solo directorio:
- `practica_3_hipotesis.R`: resolución completa de los 5 problemas en script R.
- `practica_3_hipotesis.Rmd`: versión para generar PDF.
- `README.md`: instrucciones.

## Ejecutar script R

En terminal, desde la raíz del repositorio:

```bash
Rscript Practica-3/practica_3_hipotesis.R
```

## Generar PDF desde R Markdown

1. Abrir `Practica-3/practica_3_hipotesis.Rmd`.
2. Knit a PDF en RStudio, o ejecutar:

```r
rmarkdown::render("Practica-3/practica_3_hipotesis.Rmd")
```

Nota: para PDF puede requerirse una distribución LaTeX (`tinytex`, `texlive`, etc.).

## Entregable

Se cumple la instrucción de tener el desarrollo en un solo archivo (script `.R` o `.Rmd` para PDF), ambos dentro de la carpeta `Practica-3`.