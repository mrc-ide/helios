#--------------------------------------------------------------------------------------------------#
#----- Provision -----#
#---------------------#

# Provide the packages that hipercow is required to install for the simulations:
install.packages("devtools")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("stringr")
install.packages("tictoc")
install.packages("EnvStats")
install.packages("dqrng")

# Install a specific branch of the individual package:
remotes::install_github('mrc-ide/individual@feat/logi_size')

# Install helios:
remotes::install_github('mrc-ide/helios')
