#### Chargement des packages ####

library(tidyverse)
library(readxl)
library(rmarkdown)
library(sf)

#### Chargement du tableau des fiches projet ####

Tab_proj <- janitor::clean_names(read_xlsx("Tableau_Projets.xlsx"))[-c(1:3), ] %>%
    separate(col = "empl", into = c("latitude", "longitude"), sep = ";", remove = FALSE) %>%
    mutate(
        longitude = as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", longitude))),
        latitude = as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", latitude)))
    ) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), "", .))) %>%
    mutate(file_name = paste0("Fiche_projet_", row_number())) %>%
    rename(
        project_name = titre_du_projet,
        responsable = responsable_projet,
        partenaire = partenaires,
        client = clients_concernes,
        valorisation = livrables_et_valorisations
    ) %>%
    select(-empl) %>%
    # COUPER ICI POUR GENERER LES FICHES PROJETS, CONSERVER ET SAUVEGARDER POUR LE SCRIPT QUI GENERE LA CARTE

    mutate(
        date_clean = str_trim(date),
        year_list = map(date_clean, function(x) {
            if (str_detect(x, "et")) {
                as.integer(str_extract_all(x, "\\d{4}")[[1]])
            } else if (str_detect(x, "-")) {
                years <- as.integer(str_extract_all(x, "\\d{4}")[[1]])
                seq(min(years), max(years))
            } else {
                as.integer(x)
            }
        })
    ) %>%
    unnest(year_list) %>%
    mutate(date_carte = as.character(year_list)) %>%
    select(-date_clean, -year_list) %>%
    mutate(
        date_clean = str_trim(date),
        year_list = map(date_clean, function(x) {
            if (str_detect(x, "et")) {
                as.integer(str_extract_all(x, "\\d{4}")[[1]])
            } else if (str_detect(x, "-")) {
                years <- as.integer(str_extract_all(x, "\\d{4}")[[1]])
                seq(min(years), max(years))
            } else {
                as.integer(x)
            }
        })
    ) %>%
    unnest(year_list) %>%
    mutate(date_carte = as.character(year_list)) %>%
    select(-date_clean, -year_list) %>%
    separate_rows(domaine, sep = "\\s*(;|,)\\s*") %>%
    separate_rows(service, sep = "\\s*(;|,)\\s*")

save(Tab_proj, file = "Tab_proj_formatted.RData")


#### Fonction d'automatisation des fiches projet a réaliser avec la partie au dessus de la mention  COUPER ICI POUR GENERER LES FICHES PROJETS, CONSERVER ET SAUVEGARDER POUR LE SCRIPT QUI GENERE LA CARTE ####

generate_project_sheets <- function(data, template = "Fiche_Projet.Rmd") {
    for (i in 1:nrow(data)) {
        render(
            input = template,
            output_file = paste0(data$file_name[i], ".html"),
            params = list(
                project_name = data$project_name[i],
                date = data$date[i],
                responsable = data$responsable[i],
                partenaire = data$partenaire[i],
                client = data$client[i],
                description = data$description[i],
                valorisation = data$valorisation[i]
            ),
            envir = new.env()
        )
    }
}


generate_project_sheets(Tab_proj)
