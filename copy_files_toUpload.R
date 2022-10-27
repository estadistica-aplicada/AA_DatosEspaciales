d <- 'AA_GAB_2022'
dir.create(d)
knitr::purl(
  input = "Ej01_AA.qmd",
  output = "AA_GAB_2022/Ejemplo_01.R", 
  documentation = 1L
)

knitr::purl(
  input = "Ej02_rinde.qmd",
  output = "AA_GAB_2022/Ejemplo_02.R", 
  documentation = 1L
)


datos <- list.files("data/", full.names = TRUE)
dir.create('AA_GAB_2022/data')
file.copy(from = datos,
          to = paste0('AA_GAB_2022/', datos))


dir.create('AA_GAB_2022/src')
scripts_auxiliares <- list.files("src/", full.names = TRUE)
scripts_auxiliares <- scripts_auxiliares[!agrepl("sim_grid.R", scripts_auxiliares, fixed = FALSE)]
file.copy(from = scripts_auxiliares,
          to = paste0('AA_GAB_2022/', scripts_auxiliares))

file.copy(from = "GAB2022_AnalisisInSitu.Rproj",
          to = paste0('AA_GAB_2022/', "GAB2022_AnalisisInSitu.Rproj"))


files <- list.files(d , recursive = TRUE)
zip("myarchive.zip",
    files = paste(d, files, sep = "/"))
