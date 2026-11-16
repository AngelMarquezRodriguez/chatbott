FROM rocker/r-ver:4.3.2

# 1. Instalar librerías del sistema requeridas por plumber (libsodium) y RPostgres (libpq)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    libsodium-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# 2. Instalar paquetes de R desde binarios precompilados (rápido y verificando que plumber cargue)
RUN R -q -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest')); \
    install.packages(c('plumber', 'DBI', 'RPostgres', 'stringr', 'stringi', 'httr', 'jsonlite', 'dplyr')); \
    library(plumber); library(RPostgres)"

# 3. Copiar los archivos del proyecto
COPY . /app

# 4. CRÍTICO: Borrar .Rprofile y renv dentro del contenedor para que NO oculten los paquetes instalados
RUN rm -rf /app/.Rprofile /app/renv /app/renv.lock

EXPOSE 8000

# 5. Ejecutar main.R con --vanilla para ignorar cualquier perfil local residual
CMD ["Rscript", "--vanilla", "main.R"]
