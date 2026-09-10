library(tidyverse)
library(nycflights13)

flights
glimpse(flights)

#limpio datos q quiero ver
flights |>
  filter(arr_delay >= 120)

flights |>
  filter(dest == "IAH" | dest == "HOU")

flights |>
  filter(dest %in% c("IAH", "HOU"))

flights |>
  filter(carrier %in% c("UA", "AA", "DL"))

flights |>
  filter(month %in% c(7, 8, 9))

flights |>
  filter(arr_delay > 120 & dep_delay <= 0)

flights |>
  filter(dep_delay >= 60 & (dep_delay - arr_delay) > 30)


#ordenar datos
flights |>
  arrange(desc(dep_delay))

flights |>
  arrange(dep_time)

#agrego formula
flights |>
  mutate(speed = distance / air_time * 60) |>
  arrange(desc(speed))

flights |>
  mutate(speed = distance / air_time * 60, .before = 1) |>
  arrange(desc(speed))

#pido datos especificos
flights |>
  distinct(month, day)

flights |>
  distinct(month, day) |>
  nrow()

flights |>
  arrange(desc(distance))

flights |>
  arrange(distance)

flights |>
  arrange(distance) |>
  select(origin, dest, distance) |>
  head(1)

#filtro por datos y dsps ordeno
flights |>
  filter(dest == "IAH") |>
  arrange(desc(dep_delay))

#ordeno y dsps filtro 
flights |>
  arrange(desc(dep_delay)) |>
  filter(dest == "IAH")

flights |>
  select(dep_time, sched_dep_time, dep_delay) |>
  head(10)

#forms de seleccionar columnas
flights |>
  select(dep_time, dep_delay, arr_time, arr_delay)

flights |>
  select(starts_with("dep_"), starts_with("arr_"))

flights |>
  select(contains("time") & (contains("dep") | contains("arr")))

flights |>
  select(matches("^(dep|arr)_(time|delay)$"))

#trae una sola columna
flights |>
  select(dep_time, dep_time)

#trae dos veces la misma
flights |>
  select(dep_time, arr_time, dep_time, dest)

variables <- c("year", "month", "day", "dep_delay", "arr_delay")

flights |>
  select(any_of(variables))

variables_con_error <- c("year", "month", "day", "dep_delay", "arr_delay", "no_existe")

flights |>
  select(variables_con_error)

#selecciona las columnas q existen y si alguna no la ignora
flights |>
  select(any_of(variables_con_error))

flights |>
  select(contains("TIME"))

#encuentra al columna por mas q este en minuscula
flights |>
  select(contains("TIME", ignore.case = FALSE))

#renombrar o reubicar
flights |>
  rename(air_time_min = air_time) |>
  relocate(air_time_min)

flights |>
  relocate(air_time_min = air_time)

flights |>
  select(tailnum) |>
  arrange(arr_delay)

flights |>
  arrange(arr_delay) |>
  select(tailnum)

#agrupar
flights |>
  group_by(carrier) |>
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) |>
  arrange(desc(avg_delay))

flights |>
  group_by(carrier, dest) |>
  summarize(n = n())

flights |>
  group_by(dest) |>
  slice_max(dep_delay, n = 1) |>
  relocate(dest)

#agrupo por maximo
flights |>
  group_by(dest) |>
  slice_max(dep_delay, n = 1) |>
  relocate(dest)

#grafico de lineas
flights |>
  group_by(hour) |>
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) |>
  ggplot(aes(x = hour, y = avg_delay)) +
  geom_line() +
  geom_point()

flights |>
  group_by(dest) |>
  slice_min(arr_delay, n = 1) |>
  relocate(dest)

flights |>
  group_by(dest) |>
  slice_min(arr_delay, n = -1) |>
  relocate(dest)

#nro de filas
flights |>
  group_by(dest) |>
  slice_min(arr_delay, n = 1) |>
  nrow()

flights |>
  group_by(dest) |>
  slice_min(arr_delay, n = -1) |>
  nrow()

#agrupar y contar las variables
flights |>
  count(dest)

flights |>
  group_by(dest) |>
  summarize(n = n())

flights |>
  count(dest, sort = TRUE)

flights |>
  group_by(dest) |>
  summarize(n = n()) |>
  arrange(desc(n))

#genera dataframe para trabajar 
df <- tibble(
  x = 1:5,
  y = c("a", "b", "a", "a", "b"),
  z = c("K", "K", "L", "L", "K")
)
df

df |>
  group_by(y)

#reordena filas segun el valor de y
df |>
  arrange(y)

#agrupa por y y calcula el promedio 
df |>
  group_by(y) |>
  summarize(mean_x = mean(x))

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))

#elimina agrupacion al final
df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x), .groups = "drop")

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))

df |>
  group_by(y, z) |>
  mutate(mean_x = mean(x))

##19.2.4

glimpse(weather)
glimpse(airports)

weather |>
  count(year, month, day, hour, origin) |>
  filter(n > 1)

weather |>
  filter(origin == "EWR", month == 11, day == 3, hour == 1)

special_days <- tibble(
  year = c(2013, 2013),
  month = c(12, 12),
  day = c(24, 25),
  holiday = c("Christmas Eve", "Christmas Day")
)
special_days

flights |>
  left_join(special_days, join_by(year, month, day))

install.packages("Lahman")
library(Lahman)

glimpse(Lahman::People)
glimpse(Lahman::Batting)
glimpse(Lahman::Salaries)

##19.3.4

worst_hours <- flights |>
  mutate(hour = time_hour) |>
  group_by(hour) |>
  summarize(total_delay = sum(dep_delay, na.rm = TRUE)) |>
  arrange(desc(total_delay)) |>
  head(48)

worst_hours |>
  left_join(weather, join_by(hour == time_hour))

top_dest <- flights |>
  count(dest, sort = TRUE) |>
  head(10)
top_dest

flights |>
  semi_join(top_dest, join_by(dest))

flights |>
  filter(dest %in% top_dest$dest)

flights |>
  anti_join(weather, join_by(origin, time_hour))

flights |>
  anti_join(weather, join_by(origin, time_hour)) |>
  nrow()

nrow(flights)

flights |>
  anti_join(planes, join_by(tailnum)) |>
  count(carrier, sort = TRUE)

flights |>
  count(carrier, sort = TRUE) |>
  head(10)

carriers_per_plane <- flights |>
  distinct(tailnum, carrier) |>
  group_by(tailnum) |>
  summarize(carriers = paste(carrier, collapse = ", "))
carriers_per_plane

planes |>
  left_join(carriers_per_plane, join_by(tailnum))

flights |>
  distinct(tailnum, carrier) |>
  count(tailnum, sort = TRUE) |>
  filter(n > 1)

flights |>
  left_join(
    airports |> select(faa, lat_origin = lat, lon_origin = lon),
    join_by(origin == faa)
  )

flights |>
  left_join(
    airports |> select(faa, lat_origin = lat, lon_origin = lon),
    join_by(origin == faa)
  ) |>
  left_join(
    airports |> select(faa, lat_dest = lat, lon_dest = lon),
    join_by(dest == faa)
  )

delays_by_dest <- flights |>
  group_by(dest) |>
  summarize(avg_delay = mean(arr_delay, na.rm = TRUE))
delays_by_dest

delays_by_dest <- delays_by_dest |>
  left_join(airports, join_by(dest == faa))
delays_by_dest

install.packages("maps")
delays_by_dest |>
  ggplot(aes(x = lon, y = lat, color = avg_delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

delays_june13 <- flights |>
  filter(year == 2013, month == 6, day == 13) |>
  group_by(dest) |>
  summarize(avg_delay = mean(arr_delay, na.rm = TRUE))
delays_june13

delays_june13 <- delays_june13 |>
  left_join(airports, join_by(dest == faa))

delays_june13 |>
  ggplot(aes(x = lon, y = lat, color = avg_delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

weather |>
  filter(month == 6, day == 13) |>
  arrange(desc(precip))

