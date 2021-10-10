
library(dsDesigner)
library(magick)
library(tidyverse)

#"#da1c95"

bg <- image_read("inst/certificado.png")
bg <- image_read_svg("inst/certificado.svg")

texts <- read_csv("inst/textos2.csv", col_types = cols(.default = "c"))

texts <- texts %>%
  select(1:4) %>%
  mutate(nombre_asistente = mop::totitle(nombre_asistente),
         filename = paste0(id,"-",mop::create_slug(nombre_asistente)),
         date_text = paste0(makeup::makeup_dat(fecha, locale = "es_CO",
                                               format = "%d de %B de %Y"),
                            " con una duración de 2 horas"))



# x <- texts %>% sample_n(1)
# img <- img_annotate(img, text = x$nombre_asistente, gravity = "North",
#              size = 58,
#              color = "#293845",
#              font = "IBM Plex Sans",
#              weight = 700,
#              y = 15)
# img

textos_list <- transpose(texts)


config <- read_csv("inst/textos-config.csv")
config_list <- transpose(config) %>% set_names(config$tipo_texto)
i <- 1
imgs <- map(textos_list, function(txts){

  #txts <- textos_list[[1]]
  opts <- config_list[[names(txts)[2]]]
  img <- img_annotate(bg, text = txts$nombre_asistente, gravity = "North",
               size = opts$size,
               color = opts$color,
               font = opts$font,
               weight = opts$weight %||% 400,
               y = opts$y)
  opts <- config_list[[names(txts)[3]]]
  img <- img_annotate(img, text = txts$curso, gravity = "North",
                      size = opts$size,
                      color = opts$color,
                      font = opts$font,
                      weight = opts$weight %||% 400,
                      y = opts$y)
  opts <- config_list[[names(txts)[6]]]
  img <- img_annotate(img, text = txts$date_text, gravity = "North",
                      size = opts$size,
                      color = opts$color,
                      font = opts$font,
                      weight = opts$weight %||% 400,
                      y = opts$y)
  img
  img <- image_convert(img, format = "pdf")
  img %>% image_write(paste0("tmp/",txts$filename, ".pdf"))
  i <<- i + 1
})

imgs[[1]]

turn::image_into_image



