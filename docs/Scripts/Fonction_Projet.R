
#### Chargement des packages ####

library(tidyverse)
library(readxl)
library(rmarkdown)

#### Chargement du tableau des fiches projet ####

Tab_proj <- tibble(janitor::clean_names(read_xlsx("Data/Tableau_Projets.xlsx")))[-3,] # Supression de la colonne masquée

#### Creation du dataframe des projets ####

projects_df <- data.frame(
    file_name = c("Fiche_projet_1", "Fiche_projet_2"), project_name = c("Projet A", "Projet B"),
    date = c("01/01/2025", "15/06/2024"), location = c("Nice", "Guyane"), responsable = c("Alice", "Bob"),
    partenaire = c("Partner 1", "Partner 2"), client = c("Client A", "Client B"), organisation = c("Org A", "Org B"),
    description = c("1", "2"), valorisation = c("Valorisation 1", "Valorisation 2"),
    stringsAsFactors = FALSE
)


#### Fonction d'automatisation des fiches projet ####

generate_project_sheets <- function(data, template = "Fiche_Projet.Rmd", output_dir = "Output") {
    if (!dir.exists(output_dir)) dir.create(output_dir)

    for (i in 1:nrow(data)) {
        render(
            input = template,
            output_file = paste0(data$file_name[i], ".html"),
            output_dir = output_dir,
            params = list(
                project_name = data$project_name[i],
                date = data$date[i],
                location = data$location[i],
                responsable = data$responsable[i],
                partenaire = data$partenaire[i],
                client = data$client[i],
                organisation = data$organisation[i],
                description = data$description[i],
                valorisation = data$valorisation[i]
            ),
            envir = new.env()
        )
    }
}


generate_project_sheets(projects_df)
