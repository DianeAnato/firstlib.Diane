

#' Filtre les anomalies non nulles
#'
#' @param trajet A data.frame
#'
#' @returns A data.frame
#' @export
#'
#' @examples
#' data(df_small)
#'filtre_anomalie(df_small)
#'
filtre_anomalie <- function(trajet){
  trajet |>
    dplyr::filter(is.na(`Probabilité de présence d'anomalies`))
}


#' Compte le nombre de trajet
#'
#' @param trajet A data.frame
#'
#' @returns A data.frame
#' @export
#'
#' @examples
#'  data(df_small)
#'compter_nombre_trajets(df_small)
#'
compter_nombre_trajets <- function(trajet){
  trajet |>
    dplyr::pull(Total) |>
    sum()
}

#' Compte le nombre de boucle
#'
#' @param trajet A data.frame contenant une colonne 'Numéro de boucle'
#'
#' @returns Un entier correspondant au nombre de boucles distinctes
#' @export
#'
#' @examples
#' data(df_small)
#' compter_nombre_boucle(df_small)
compter_nombre_boucle <- function(trajet){
  trajet |>
    dplyr::pull(`Numéro de boucle`) |>
    dplyr::n_distinct()
}

#' Trouver le jour de plus de trajets
#'
#' @param trajet A data.frame
#'
#' @returns A data.frame
#' @export
#'
#' @examples
#' data(df_small)
#' trouver_trajet_max(df_small)
trouver_trajet_max <- function(trajet){
  trajet |>
    dplyr::slice_max(Total) |>
    dplyr::select(`Boucle de comptage`, Jour, Total)
}


#' Les trajets par semaine
#'
#' @param trajet A data.frame
#'
#' @returns A data.frame
#' @export
#'
#' @examples
#'  data(df_small)
#' calcul_distribution_semaine(df_small)
#'
calcul_distribution_semaine <- function(trajet, filtre=TRUE){
  if (filtre== TRUE) {
    trajet <- trajet |>
      filtre_anomalie()
  }

  trajet |>
    dplyr::count(`Jour de la semaine`, wt = Total, sort = TRUE, name = "trajets")
}

#' Graphique
#'
#' @param trajet A data.frame
#'
#' @returns A data.frame
#' @export
#'
#' @examples
#' data(df_small)
#' plot_distribution_semaine(df_small)
#'
plot_distribution_semaine <- function(trajet) {
  trajet_weekday <- trajet |>
    filtre_anomalie() |>
    calcul_distribution_semaine() |>
    dplyr::mutate(
      jour = forcats::fct_recode(
        factor(`Jour de la semaine`),
        "lundi" = "1",
        "mardi" = "2",
        "mercredi" = "3",
        "jeudi" = "4",
        "vendredi" = "5",
        "samedi" = "6",
        "dimanche" = "7"
      )
    )

  ggplot2::ggplot(trajet_weekday) +
    ggplot2::aes(x = jour, y = trajets) +
    ggplot2::geom_col()
}



#' Petit jeu de données de trajets vélo
#'
#' Un échantillon de données utilisé pour illustrer les fonctions du package.
#'
#' @format A data.frame with variables:
#' \describe{
#'   \item{Numéro de boucle}{Identifiant de la boucle}
#'   \item{Jour de la semaine}{Jour de circulation}
#'   \item{Total}{Nombre total de trajets}
#'   \item{Probabilité de présence d'anomalies}{Indicateur d'anomalie}
#' }
"df_small"


#' Jeu de données de trajets vélo sur les vacances de Toussaint
#'
#' La base dont on aura besoin dans le package.
#'
#' @format A data.frame with variables:
#' \describe{
#'   \item{Numéro de boucle}{Identifiant de la boucle}
#'   \item{Jour de la semaine}{Jour de circulation}
#'   \item{Total}{Nombre total de trajets}
#'   \item{Probabilité de présence d'anomalies}{Indicateur d'anomalie}
#' }
"df_velo"


#' Filtrer les trajets par numéro de boucle
#'
#' Cette fonction permet de sélectionner uniquement les lignes
#' correspondant à une ou plusieurs boucles dans un jeu de données.
#'
#' @param trajet Un data.frame contenant les données de trajets.
#' @param boucle Un vecteur de numéros de boucle à conserver.
#'
#' @return Un data.frame filtré sur les boucles sélectionnées.
#' @export
#'
#' @examples
#' filtrer_trajet(trajet = df_velo, boucle = c("880", "881"))
filtrer_trajet <- function(trajet, boucle) {
  if (is.null(boucle)) { return(trajet) }
  trajet |>
    dplyr::filter(`Numéro de boucle` %in% boucle)
}

