 # install.packages("tidyverse")
# install.packages("psych")
# install.packages("ggplot2")
# install.packages("janitor")
# install.packages("patchwork")
 
library(tidyverse)
library(psych)
library(ggplot2)
library(janitor)
library(patchwork)

# Download data from DOI 10.5281/zenodo.23162308 (https://zenodo.org/records/23162309)
whole.data <- read.csv("./games.csv")

data.clean <- whole.data |>
  
  # select the relevant variables
  select(name, # name of game
         release_year, # year of game release
         min_ram_gb, # minimum required ram to run game in gigabytes
         min_gpu_g3d, # minimum required graphics benchmark
  ) |>
  
  #convert to snake case to stop the number issue
  # also removes all apostrophes, colons, periods and dashes so the next function stops being weird
  mutate(name = make_clean_names(name)) |>
  
  # create a column to separate CD Projekt RED games and other
  mutate(dev = case_when(name == "the_witcher_3_wild_hunt" |
                           name == "the_witcher_2_assassins_of_kings" |
                           name == "the_witcher" |
                           name == "the_witcher_3_wild_hunt_hearts_of_stone" |
                           name == "the_witcher_3_wild_hunt_blood_and_wine" |
                           name == "the_witcher_2_assassins_of_kings_enhanced_edition" |
                           name == "thronebreaker_the_witcher_tales" |
                           name == "gwent_the_witcher_card_game" |
                           name == "the_witcher_enhanced_edition_directors_cut" |
                           name == "the_witcher_adventure_game" |
                           name == "the_witcher_3_wild_hunt_new_quest_where_the_cat_and_wolf_play" |
                           name == "the_witcher_3_wild_hunt_new_quest_contract_skelliges_most_wanted" |
                           name == "the_witcher_3_wild_hunt_new_quest_fools_gold" |
                           name == "the_witcher_3_wild_hunt_new_quest_contract_missing_miners" |
                           name == "reigns_the_witcher" |
                           name == "the_witcher_3_wild_hunt_new_quest_scavenger_hunt_wolf_school_gear" |
                           name == "cyberpunk_2077" |
                           name == "cyberpunk_2077_phantom_liberty"
                       ~ "CDPR", TRUE ~ "Other")) |>
  
  # convert a few older game observations mistakenly recorded in mb to gb
  mutate(min_ram_gb = case_when(min_ram_gb >= 32 & name != "under_a_desert_sun" ~ min_ram_gb / 1000, TRUE ~ min_ram_gb))

# make a new dataframe with only non-CD Projekt RED games
other.dev <- data.clean |> 
  filter(dev == "Other")

# select only the years relevent for the 4 CD Projekt RED games
other.dev.year <- other.dev |>
  filter(release_year == 2007 |
           release_year == 2011 |
           release_year == 2015 |
           release_year == 2020)
  
other.dev.year$release_year <- as.factor(other.dev.year$release_year)

#select only the main base-games from CD Projekt RED, no DLC or spinoff games
cdpr.dev <- data.clean |>
  filter(name == "the_witcher_3_wild_hunt" |
           name == "the_witcher_2_assassins_of_kings" |
           name == "the_witcher" |
           name == "cyberpunk_2077")

cdpr.dev$release_year <- as.factor(cdpr.dev$release_year)

# computes descriptive statistics for each group, transforms into a matrix
descriptives <- describe(other.dev) |>
  mutate_if(is.numeric, round, 2)

descriptives.year <- describeBy(other.dev.year, group = other.dev.year$release_year, mat = TRUE, digits = 2)

journal.scatter <- ggplot() +
 geom_jitter(data = other.dev, aes(x = min_gpu_g3d, 
                                   y = min_ram_gb), 
             alpha = 0.3,
             height = 0.5, 
             width = 0.5,
             colour = "gray") +
  geom_point(data = cdpr.dev, aes(x = min_gpu_g3d, 
                                  y = min_ram_gb,
                                  shape = name), 
             colour = "black") +
  theme_classic() +
  labs(title = "Scatterplot of Minimum GPU and CPU for CD Project RED Against Other Games",
       x = "Minimum Required GPU Benchmark Score", 
       y = "Minimum REquired RAM in GB", 
       shape = "CD Projekt RED Games") 

gpu.comb.boxplot <- ggplot() +
  geom_boxplot(data = other.dev, aes(x = dev,
                                     y = min_gpu_g3d), colour = "black") +
  geom_point(data = cdpr.dev, aes(x = dev,
                                  y = min_gpu_g3d,
                                  shape = name)) +
  theme_classic() +
  labs(title = "Boxplot of Minimum GPU for CD Projekt RED Against Other Games",
       x = "Developer", 
       y = "Minimum Required GPU Benchmark Score", 
       shape = "CD Projekt RED Games") 

ram.comb.boxplot <- ggplot() +
  geom_boxplot(data = other.dev, aes(x = dev, 
                                          y = min_ram_gb), colour = "black") +
  geom_point(data = cdpr.dev, aes(x = dev, 
                                  y = min_ram_gb,
                                  shape = name)) +
  theme_classic() +
  labs(title = "Boxplot of Minimum RAM for CD Projekt RED Against Other Games",
       x = "Developer", 
       y = "Minimum Required RAM (gb)", 
       shape = "CD Projekt RED Games") 

journal.comb.boxplot <- gpu.comb.boxplot / ram.comb.boxplot

gpu.boxplot <- ggplot() +
  geom_boxplot(data = other.dev.year, aes(x = release_year, 
                                     y = min_gpu_g3d), colour = "gray") +
  geom_point(data = cdpr.dev, aes(x = release_year, 
                                  y = min_gpu_g3d,
                                  shape = name)) +
  theme_classic() +
  labs(title = "Boxplot of Minimum GPU for CD Projekt RED Against Other Games in the Release Year",
       x = "Game Release Year", 
       y = "Minimum Required GPU Benchmark Score", 
       shape = "CD Projekt RED Games") 

ram.boxplot <- ggplot() +
  geom_boxplot(data = other.dev.year, aes(x = release_year, 
                                          y = min_ram_gb), colour = "gray") +
  geom_point(data = cdpr.dev, aes(x = release_year, 
                                  y = min_ram_gb,
                                  shape = name)) +
  theme_classic() +
  labs(title = "Boxplot of Minimum RAM for CD Projekt RED Against Other Games in the Release Year",
       x = "Game Release Year", 
       y = "Minimum Required RAM (gb)", 
       shape = "CD Projekt RED Games") 

journal.boxplot <- gpu.boxplot / ram.boxplot

masses.scatter <- ggplot() +
  geom_jitter(data = other.dev, aes(x = min_gpu_g3d, 
                                    y = min_ram_gb), 
              alpha = 0.5,
              height = 0.5, 
              width = 0.5,
              colour = "pink") +
  geom_text(data = cdpr.dev, aes(x = min_gpu_g3d, 
                                  y = min_ram_gb), 
            label = c("Witcher3", "CP2077", "Witcher2", "Witcher"), 
            colour = "black") +
  theme_classic() +
  labs(title = "Minimum Graphics and Memory Requirements Increase Together and Most Requirements are Low",
       subtitle = "The Case for both CD Projekt RED and Other Developer Games",
       x = "Minimum Required Graphics Benchmark Score", 
       y = "Minimum Required Memory")

gpu.comb.boxplot2 <- ggplot() +
  geom_boxplot(data = other.dev, aes(x = dev,
                                     y = min_gpu_g3d), fill = "pink") +
  geom_text(data = cdpr.dev, aes(x = dev, 
                                 y = min_gpu_g3d), 
            label = c("Witcher3", "CP2077", "Witcher2", "Witcher"), 
            colour = "black") +
  theme_classic() +
  labs(title = "Newer CDPR Games Require Higher Graphics Benchmarks and Memory than Other Games",
       subtitle = "Older CD Projekt RED Games Require the Same or Slightly Less",
       x = "Game Developer", 
       y = "Minimum Required Graphics Benchmark Score")

ram.comb.boxplot2 <- ggplot() +
  geom_boxplot(data = other.dev, aes(x = dev, 
                                     y = min_ram_gb), fill = "pink") +
  geom_text(data = cdpr.dev, aes(x = dev, 
                                 y = min_ram_gb), 
            label = c("Witcher3", "CP2077", "Witcher2", "Witcher"), 
            colour = "black") +
  theme_classic() +
  labs(x = "Game Developer", 
       y = "Minimum Required Memory")

masses.comb.boxplot <- gpu.comb.boxplot2 / ram.comb.boxplot2

gpu.boxplot2 <- ggplot() +
  geom_boxplot(data = other.dev.year, aes(x = release_year, 
                                          y = min_gpu_g3d), colour = "pink") +
  geom_text(data = cdpr.dev, aes(x = release_year, 
                                 y = min_gpu_g3d), 
            label = c("Witcher3", "CP2077", "Witcher2", "Witcher"), 
            colour = "black") +
  theme_classic() +
  labs(title = "Even Within the Same Release Year, Newer CD ProJekt RED Game Requirements Surpass Others",
       subtitle = "Older CD Projekt RED Requirements are on Par with Others in Their Release Year",
       x = "Game Release Year", 
       y = "Minimum Required Graphics Benchmark Score") 

ram.boxplot2 <- ggplot() +
  geom_boxplot(data = other.dev.year, aes(x = release_year, 
                                          y = min_ram_gb), colour = "pink") +
  geom_text(data = cdpr.dev, aes(x = release_year, 
                                 y = min_ram_gb), 
            label = c("Witcher3", "CP2077", "Witcher2", "Witcher"), 
            colour = "black") +
  theme_classic() +
  labs(x = "Game Release Year", 
       y = "Minimum Required Memory") 

masses.boxplot <- gpu.boxplot2 / ram.boxplot2



