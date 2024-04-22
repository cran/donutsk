## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(donutsk)
library(dplyr)

GDP1 <- filter(GDP, date %in% c(2001, 2022)) |> 
  group_by(date) |>
  mutate(country = if_else(GDP > quantile(GDP, .9), country, "Other")) |> 
  group_by(date, region, region_ISO, country) |> 
  summarise(GDP = sum(GDP), .groups = "drop")

GDP1

## -----------------------------------------------------------------------------
GDP2_1 <- arrange(GDP1, date, region_ISO)

GDP2_2 <- nest_by(GDP1, date) |> 
  mutate(data = list(packing(data, GDP, region_ISO))) |> 
  tidyr::unnest(cols = "data")

GDP2 <- bind_rows(`arrange()`= GDP2_1, `packing()`= GDP2_2, .id = "Arrange type")

## ---- fig.height=10, fig.width=10---------------------------------------------
ggplot(GDP2, aes(value = GDP, fill = region)) + 
  # Internal donat represents regions
  geom_donut_int(r_int = .25, col="white", linewidth=.1) +
  # External donat represents countries
  geom_donut_ext(aes(opacity = country), col="white", linewidth=.1, show.legend = F) +
  # Text annotations for internal donut
  geom_text_int(aes(label = "{scales::percent(.prc)}", col = region), 
                size=3, r = 1.25, show.legend = F) + 
  # Label annotations for internal donut
  geom_label_ext(aes(col=region, label=paste0(country, "-{scales::percent(.prc, .01)}")), 
                 size=3, col = "white", layout = eye(), show.legend = F,
                 label.padding=unit(0.1, "lines")) + 
  # Link label annotations to specific country GDP segment
  geom_pin(aes(col = region),  
           size=.5, linewidth=.1, show.legend = F, cut=0, layout = eye(), r = 2) + 
  # Adjust colors schema with palette
  scale_fill_viridis_d(option = "mako", begin = .1, end = .8) +
  scale_color_viridis_d(option = "mako", begin = .1, end = .8) +
  coord_radial(theta = "y", expand = F) +
  # Splitting data to 4 subsets with different combinations Arrange type ~ Year
  facet_grid(`Arrange type`~date) +
  xlim(0, 5) +
  theme(legend.position = "inside", axis.text=element_blank(), 
        axis.ticks=element_blank(), panel.grid=element_blank(), 
        legend.position.inside=c(.5, .5), legend.direction = "horizontal") +
  labs(title = "GDP, PPP (current international $)")

## ---- fig.width=7, fig.height=13----------------------------------------------
ggplot(GDP2_1, aes(value = GDP, fill = region)) + 
  # Internal donat represents regions
  geom_donut_int(r_int = .25, col="white", linewidth=.1) +
  # External donat represents countries
  geom_donut_ext(aes(opacity = country), col="white", linewidth=.1, show.legend = F) +
  # Text annotations for internal donut
  geom_text_int(aes(label = "{scales::percent(.prc)}", col = region), 
                size=3, r = 1.25, show.legend = F) + 
  # Label annotations for internal donut
  geom_label_ext(aes(col=region, label=paste0(country, "-{scales::percent(.prc, .01)}")), 
                 size=3, col = "white", layout = eye(), show.legend = F) + 
  # Link label annotations to specific country GDP segment
  geom_pin(aes(col = region),  
           size=.5, linewidth=.1, show.legend = F, cut=0, layout = eye(), r = 2) + 
  # Adjust colors schema with palette
  scale_fill_viridis_d(option = "mako", begin = .1, end = .8) +
  scale_color_viridis_d(option = "mako", begin = .1, end = .8) +
  coord_radial(theta = "y", expand = F) +
  # Splitting data to 4 subsets with different combinations Arrange type ~ Year
  facet_grid(date~., switch = "x") +
  xlim(0, 4.5) +
  theme(legend.position = "inside", axis.text=element_blank(), 
        axis.ticks=element_blank(), panel.grid=element_blank(), 
        legend.position.inside=c(.5, .5), legend.direction = "horizontal") +
  labs(title = "GDP, PPP (current international $)", fill="")

## ---- fig.height=10, fig.width=10---------------------------------------------
# Prepare data using more strict threshold 
GDP4 <- filter(GDP, date %in% c(2001, 2022)) |> 
  group_by(date) |>
  mutate(country = if_else(GDP > quantile(GDP, .95), country, paste0("Other\n", region_ISO))) |> 
  group_by(date, region, region_ISO, country) |> 
  summarise(GDP = sum(GDP), .groups = "drop")

# Prepare arranged data alphabethically 
GDP5_1 <- arrange(GDP4, date, region_ISO)

# Utilize packing() for data ordering
GDP5_2 <- nest_by(GDP4, date) |> 
  mutate(data = list(packing(data, GDP, region_ISO))) |> 
  tidyr::unnest(cols = "data")

# Combine two arrange types together
GDP5 <- bind_rows(`arrange()`= GDP5_1, `packing()`= GDP5_2, .id = "Arrange type")

# Set layout parameters 
tv_lt <- tv(scale_x = 3, scale_y = 3, thinner = T, thinner_gap = .5)

# Build donut chart 
ggplot(GDP5, aes(value = GDP, fill = region)) + 
  geom_donut_int(r_int = .25, col="white", linewidth=.1) +
  geom_donut_ext(aes(opacity = country), col="white", linewidth=.1, show.legend = F) +
  geom_text_int(aes(label = "{scales::percent(.prc)}", col = region), 
                size=3, r = 1.25, show.legend = F) + 
  geom_pin(aes(col = region), size=.5, linewidth=.1, show.legend = F, cut=.1, r = 1.9,
           layout =  tv_lt) +
  geom_label_ext(aes(col = region, 
                     label = paste(stringr::str_wrap(country, 5),"\n{scales::percent(.prc, .01)}")), 
                 size=3, col = "white", show.legend = F, label.padding=unit(0.1, "lines"),
                 lineheight = .8, layout = tv_lt) + 
  scale_fill_viridis_d(option = "mako", begin = .1, end = .8) +
  scale_color_viridis_d(option = "mako", begin = .1, end = .8) +
  coord_radial(theta = "y", expand = F) +
  facet_grid(`Arrange type`~date) +
  theme(legend.position="inside", axis.text=element_blank(), 
        axis.ticks=element_blank(), panel.grid=element_blank(), 
        legend.position.inside=c(.5, .5), legend.direction = "horizontal") +
  labs(title = "GDP, PPP (current international $)")

