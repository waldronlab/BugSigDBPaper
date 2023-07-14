plotHeat <- function()
{
    

}


#' @export
plotStatsPerBodySite <- function(bbs,
                                 org = c("human", "mouse", "rat"),
                                 sex = c("male", "female"),    
                                 mode = c("fine", "coarse"))
{
   coarse.classes <-  c("circulatory system", 
                        "digestive system", 
                        "nervous system", 
                        "reproductive system",
                        "respiratory system")

    org <- match.arg(org)
    sex <- match.arg(sex)
    mode <- match.arg(mode)   

    bbs <- .transformBBS(bbs)
    organs <- sort(union(gganatogram::hgMale_key$organ, 
                         gganatogram::hgFemale_key$organ))
    stopifnot(all(names(bbs) %in% organs))

    df <- if(sex == "male") gganatogram::hgMale_key else gganatogram::hgFemale_key
    df <- df[df$organ %in% names(bbs),]
    df$value <- unname(bbs[df$organ]) 

    gganatogram::gganatogram(df, 
                             organism = org,
                             sex = sex, 
                             fill = "value") + 
        ggplot2::theme_void() +
        viridis::scale_fill_viridis(name = "nr.papers", trans = "log2")

}

.transformBBS <- function(bbs)
{
    names(bbs) <- tolower(names(bbs))
    bbs <- bbs[!(names(bbs) %in% c("", "surgical tissue", "hiv"))]
    
    bf <- c("blood", "feces")
    bbs[bf] <- bbs[bf] + bbs["blood,feces"]
    bbs <- bbs[names(bbs) != "blood,feces"]
    
    bf <- c("bronchus", "nose")
    if(!("nose" %in% names(bbs))) bbs <- c(bbs, nose = 1)
    bbs[bf] <- bbs[bf] + bbs["bronchus,nose"]
    bbs <- bbs[names(bbs) != "bronchus,nose"]

    bf <- c("nose", "skin of cheek")
    bbs[bf] <- bbs[bf] + bbs["nose,skin of cheek"]
    bbs <- bbs[names(bbs) != "nose,skin of cheek"]

    bf <- c("feces", "mouth")
    bbs[bf] <- bbs[bf] + bbs["feces,mouth"]
    bbs <- bbs[names(bbs) != "feces,mouth"]

    names(bbs)[names(bbs) %in% c("feces", "meconium", 
                                 "colonic mucosa", 
                                 "feces,mucosa of small intestine",
                                 "mucosa of small intestine,feces",   
                                 "digestive system,digestive tract,lower digestive tract")] <- "colon"
    names(bbs)[names(bbs) %in% c("oral gland", 
                                 "saliva",
                                 "saliva-secreting gland",
                                 "major salivary gland")] <- "salivary_gland"
    names(bbs)[names(bbs) %in% c("mouth", 
                                 "mouth,skin of cheek,tongue,gingiva,oropharynx",
                                 "dental plaque",
                                 "gingiva",
                                 "subgingival dental plaque",    
                                 "buccal mucosa", 
                                 "buccal mucosa,lower lip")] <- "tongue"
    names(bbs)[names(bbs) %in% c("oropharynx", "hypopharynx")] <- "throat"
    names(bbs)[names(bbs) %in% c("skin of body", 
                                 "skin of cheek",
                                 "skin of forearm",   
                                 "skin of sole of pes,interdigital space")] <- "skin"
    names(bbs)[names(bbs) %in% c("blood")] <- "aorta"
    names(bbs)[names(bbs) %in% c("nasopharynx", "nasopharynx,oropharynx")] <- "nasal_pharynx"
    names(bbs)[names(bbs) %in% c("posterior fornix of vagina",
                                 "lower part of vagina",
                                 "genitals",
                                 "semen",
                                 "cervicovaginal secretion,posterior fornix of vagina",
                                 "vaginal fluid")] <- "vagina"
    names(bbs)[names(bbs) %in% c("mesenteric lymph node")] <- "lymph node"
    names(bbs)[names(bbs) %in% c("mucosa of stomach", "mucosa")] <- "stomach"
    names(bbs)[names(bbs) %in% c("nasal cavity")] <- "nasal_septum"
    names(bbs)[names(bbs) %in% c("prostate gland secretion")] <- "prostate"
    names(bbs)[names(bbs) %in% c("urine")] <- "kidney"
    names(bbs)[names(bbs) %in% c("uterovesical pouch")] <- "uterus"
    names(bbs)[names(bbs) %in% c("ectocervix", "endocervix")] <- "uterine_cervix"
    names(bbs)[names(bbs) %in% c("pair of lungs")] <- "lung"

    names(bbs) <- gsub(" ", "_", names(bbs))
    bbs <- split(bbs, names(bbs))
    vapply(bbs, sum, numeric(1))
}

#' @export
plotStatsPerCountry <- function(ppc, fixed = TRUE)
{
    world <-  ggplot2::map_data("world")
    ppc <- ppc[names(ppc) != ""]
    
    fgukl <- list(c("Finland", "Germany", "United Kingdom"),
                 c("Estonia", "Sweden"),
                 c("Norway", "Sweden"),
                 c("Canada", "United States of America"),
                 c("Cyprus", "Estonia", "Germany", "Hungary", "Sweden"),
                 c("Austria", "Canada", "China", "France", "Italy", "United States of America"),
                 c("Austria", "China", "France", "Germany", "Italy", "Japan", "United States of America"),
                 c("Bangladesh", "China", "United Kingdom", "United States of America"))           

    for(p in seq_along(fgukl))
    {
        fguk <- fgukl[[p]]
        afguk <- paste(fguk, collapse = ",") 
        for(i in fguk) ppc[i] <- sum(ppc[i], ppc[afguk], na.rm = TRUE)
        ppc <- ppc[names(ppc) != afguk]   
    }    

    um <- c("United States of America", "Mexico")
    ppc[um] <- ppc[um] + ppc["United States-Mexico Border"]
    ppc <- ppc[names(ppc) != "United States-Mexico Border"]    

    names(ppc)[names(ppc) == "United States of America"] <- "USA"
    names(ppc)[names(ppc) == "United Kingdom"] <- "UK"
    names(ppc)[names(ppc) == "Russian Federation"] <- "Russia"
    names(ppc)[names(ppc) == "United Republic of Tanzania"] <- "Tanzania"
    names(ppc)[names(ppc) == "Trinidad and Tobago"] <- "Trinidad"

    stopifnot(all(names(ppc) %in% world$region))

    world$nr.papers <- as.vector(ppc[world$region])
    ggp <- ggplot2::ggplot() + 
        ggplot2::geom_polygon(data = world, 
            ggplot2::aes(x = long, y = lat, fill = nr.papers, group = group), 
            color = "white") + 
        viridis::scale_fill_viridis(trans = "log10")
    if(fixed) ggp <- ggp + ggplot2::coord_fixed(1.3)
    ggp 
}


