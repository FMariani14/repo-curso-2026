library(tidyverse)
library(palmerpenguins)
library(ggthemes)

dim(penguins)
nrow(penguins)
ncol(penguins)

?penguins

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point()

#relacion dispersa 

ggplot(penguins, aes(x = species, y = bill_depth_mm)) +
  geom_point()

ggplot(penguins, aes(x = species, y = bill_depth_mm)) +
  geom_boxplot()

#error
ggplot(data = penguins) +
  geom_point()

ggplot(data = penguins) +
  geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g))
#codigo correcto, relacon positiva 

?geom_point

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(na.rm = TRUE)

#agrega descripcion al grafico
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(na.rm = TRUE) +
  labs(caption = "Data come from the palmerpenguins package.")


#color y estetica de grafico
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = bill_depth_mm)) +
  geom_smooth()

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = bill_depth_mm)) +
  geom_point() +
  geom_smooth()

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE)

#datos global
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()

#datos local repetidos
ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )

#grafioc de barras horizontal
ggplot(penguins, aes(y = species)) +
  geom_bar()

#grafico de barras vertical 
ggplot(penguins, aes(x = species)) +
  geom_bar()

#color por borde
ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")

#relleno de color
ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")

?geom_histogram

#histograma
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(bins = 30)

#menos cantidad de barras, regulo con bins
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(bins = 10)

glimpse(diamonds)

#histograma
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.5)

#barras mas finas, binwidth regulo el ancho
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.1)
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

?mpg

#para distinguir categoricas de numericas int o chr
glimpse(mpg)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

#scatterplot con color 
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = cyl))

#regulo el ancho de los puntos 
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(size = cyl))

#doy formas a los puntos 
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(shape = class))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(linewidth = cyl))

#grafica con formas y colores
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv, shape = drv))

#solo colores 
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv))

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point()

#genera tres paneles, uno por cada especie y dentro de eso cada scatterp
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  facet_wrap(~species)

ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm,
    color = species, shape = species
  )
) +
  geom_point() +
  labs(color = "Species")

#mismo scatterp tres categorias distinguidas con una forma y un color cu
ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm,
    color = species, shape = species
  )
) +
  geom_point() +
  labs(color = "Species", shape = "Species")

#stacked bar plots, cada color separa del total de cada especie 
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")

ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")

#para guardar el ultimo grafico
ggplot(mpg, aes(x = class)) +
  geom_bar()
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()
ggsave("mpg-plot.png")

#guardar como pdf
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()
ggsave("mpg-plot.pdf")

?ggsave
