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
    select(-empl)

save(Tab_proj, file = "Tab_proj_formatted.RData")


#### Fonction d'automatisation des fiches projet ####

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
