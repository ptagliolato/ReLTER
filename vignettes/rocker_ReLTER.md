---
title: "Run ReLTER in a Docker container"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Run ReLTER in a Docker container}
  %\VignetteEngine{knitr::knitr}
  %\usepackage[UTF-8]{inputenc}
author: "Paolo Tagliolato"
---

# rocker_ReLTER
docker image for ReLTER package (https://github.com/ropensci/ReLTER)

## About
Run RStudio with preinstalled ReLTER package through Docker.

## Install a different branch of ReLTER package
Edit the addReLTER.sh script and modify the last line, using the name of your preferred branch as the ref= parameter value:

    R -e "devtools::install_github('https://github.com/ropensci/ReLTER',ref = '<branchName>',dependencies = FALSE)"
  
## Build the image

    docker build . -t rocker_relter

## Usage
Run locally on port 8080 (change port as you like).
You can change "yourpasswordW with the password you prefer.

    docker run -d -e PASSWORD=yourpassword -p 8080:8787 rocker_relter

Open browser at localhost:8080 and login with

    user: rstudio
    password: youpassword

## Preserve your work (use a docker volume)
When you stop a docker container, the files created within it are lost. In order to preserve your work across 
different runs, link a local volume (in this example, the current working directory, $(pwd)) to the container:

    docker run -d -v $(pwd):/home/rstudio -e PASSWORD=yourpassword -p 8080:8787 rocker_relter

## Precompiled image on docker hub
You can also use the precompiled image from docker hub:

    docker pull ptagliolato/rocker_relter
    docker run -d -e PASSWORD=yourpassword -p 8080:8787 ptagliolato/rocker_relter
