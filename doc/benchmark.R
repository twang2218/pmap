# Provision
# docker-machine create -d digitalocean --digitalocean-region=nyc3 --digitalocean-size=4gb d1
# apt install -y r-base libxml2-dev libssl-dev libcurl4-openssl-dev htop
# curl -sSL https://agent.digitalocean.com/install.sh | sh
install.packages("pmap")
install.packages("microbenchmark")
install.packages("devtools")

clean <- function() {
  if ("pmap" %in% installed.packages()[,"Package"]) {
    # 'pmap' has been installed, remove first
    library(pmap)
    detach("package:pmap")
    remove.packages("pmap")
    # .rs.restartR()
  }
}

setup <- function(version) {
  devtools::install_github("twang2218/pmap", ref = version)
  pkgdb <- installed.packages()
  if ("pmap" %in% pkgdb[,"Package"]) {
    print(paste("Installed pmap version ", pkgdb["pmap", "Version"]))
    library(pmap)
  } else {
    stop(paste("Unable to install pmap of version:", version))
  }
}

library(dplyr)
library(microbenchmark)
library(pmap)

clean()
setup("master")

el_10k <- generate_eventlog(10000, 2000, c("campaign", "sale"), c(10, 3))
el_100k <- generate_eventlog(100000, 20000, c("campaign", "sale"), c(10, 3))
el_1000k <- generate_eventlog(1000000, 200000, c("campaign", "sale"), c(10, 3))

# 0.2.0
clean()
setup("v0.2.0")

el_10k <- generate_random_eventlog(10000, 2000, c("campaign", "sale"), c(10, 3))
el_100k <- generate_random_eventlog(100000, 20000, c("campaign", "sale"), c(10, 3))
el_1000k <- generate_random_eventlog(1000000, 200000, c("campaign", "sale"), c(10, 3))

microbenchmark(
  create_pmap_from_eventlog(el_10k),
  create_pmap_from_eventlog(el_100k),
  # create_pmap_from_eventlog(el_1000k),
  times = 10
)
# Unit: seconds
#                                expr      min        lq      mean    median        uq      max neval
#   create_pmap_from_eventlog(el_10k)  1.56275  1.600056  1.637137  1.643266  1.671124  1.73218    10
#  create_pmap_from_eventlog(el_100k) 47.74824 48.093884 50.321483 50.637030 51.098359 54.20957    10
# system.time(create_pmap_from_eventlog(el_1000k)) => 130 mins (156x)

# 0.3.2
clean()
setup("v0.3.2")

microbenchmark(
  create_pmap(el_10k),
  create_pmap(el_100k),
  # create_pmap(el_1000k),
  times = 10
)
# Unit: seconds
#                  expr       min        lq     mean    median        uq      max neval
#   create_pmap(el_10k)  1.727415  1.799521  1.86058  1.828059  1.888508  2.13194    10
#  create_pmap(el_100k) 55.219807 56.539817 57.64809 57.757177 58.450986 59.80957    10


# 0.4.0
clean()
setup("v0.4.0")

microbenchmark(
  create_pmap(el_10k),
  create_pmap(el_100k),
  # create_pmap(el_1000k),
  times = 10
)
# Unit: seconds
#                  expr       min        lq      mean    median        uq      max neval
#   create_pmap(el_10k)  1.685579  1.773115  1.865584  1.826707  1.973622  2.11452    10
#  create_pmap(el_100k) 57.925665 59.242784 61.246433 60.450505 62.296957 65.79441    10

# master
clean()
setup("master")

library(data.table)

el_10k <- generate_eventlog(10000, 2000, c("campaign", "sale"), c(10, 3))
el_100k <- generate_eventlog(100000, 20000, c("campaign", "sale"), c(10, 3))
el_1m <- generate_eventlog(1000000, 200000, c("campaign", "sale"), c(10, 3))
el_10m <- generate_eventlog(10000000, 2000000, c("campaign", "sale"), c(10, 3))

# Use `copy()` to avoid `data.table::setorder()` modify the original dataset.
microbenchmark(
  create_pmap(data.table::copy(el_10k)),
  create_pmap(data.table::copy(el_100k)),
  create_pmap(data.table::copy(el_1m)),
  create_pmap(data.table::copy(el_10m)),
  times = 10
)

system.time(data.table::copy(el_10m))

el_100k <- generate_eventlog(100000, 20000, c("campaign", "sale"), c(10, 3))
create_pmap(data.table::copy(el_100k))

el_10m <- generate_eventlog(10000000, 2000000, c("campaign", "sale"), c(10, 3))
create_pmap(data.table::copy(el_1m))

# macbook pro
# Unit: milliseconds
#                   expr       min        lq      mean    median        uq       max neval
#    create_pmap(el_10k)  392.4646  411.3784  452.0301  421.8778  426.4990  602.4456    10
#   create_pmap(el_100k)  719.7945  732.0896  741.9988  744.0928  752.5453  767.8442    10
#  create_pmap(el_1000k) 4934.7507 5007.0168 5077.1100 5052.3182 5173.4299 5224.5544    10

# d1 (Digital Ocean - 4GB)
# Unit: milliseconds
#                   expr       min        lq      mean    median        uq       max neval
#    create_pmap(el_10k)  507.7968  524.3655  564.0430  542.9571  572.6458  685.7234    10
#   create_pmap(el_100k)  807.3958  841.5750  857.1228  859.4381  880.2861  884.7447    10
#  create_pmap(el_1000k) 4746.3864 4827.8861 5020.3630 5057.1328 5192.0509 5321.1395    10

# d2 (Digital Ocean - 4GB)
# Unit: milliseconds
#                   expr       min        lq      mean    median        uq       max neval
#    create_pmap(el_10k)  559.0618  581.3116  626.3841  611.7513  622.4935  755.7317    10
#   create_pmap(el_100k)  864.9402  885.6535  919.1273  894.4643  934.1724 1095.2642    10
#  create_pmap(el_1000k) 4804.6721 5142.7089 5173.3926 5178.5153 5245.1480 5451.2313    10

# d3 (Digital Ocean - 4GB)
# Unit: milliseconds
#                   expr       min        lq      mean    median        uq       max neval
#    create_pmap(el_10k)  558.3585  564.0215  601.3523  568.4247  583.9266  851.9249    10
#   create_pmap(el_100k)  867.2925  898.8099  937.5693  929.7195 1001.6153 1016.4914    10
#  create_pmap(el_1000k) 4921.7149 4945.8692 5116.3571 5119.2399 5278.9299 5317.3816    10

# d2
el_10m <- generate_eventlog(10000000, 2000000, c("campaign", "sale"), c(10, 3))
system.time(create_pmap(el_10m))
# 10 Millions records:
#    user  system elapsed
#  68.376   0.872  69.329

# dplyr::arrange
# Unit: milliseconds
#                  expr        min         lq      mean    median         uq       max neval
#   create_pmap(el_10k)   519.5634   532.7716   669.306   626.399   764.8286  1070.190    10
#  create_pmap(el_100k)   851.8473   963.7521  1354.263  1121.218  1391.0232  2527.904    10
#    create_pmap(el_1m)  4758.2447  4799.4362  4959.207  4945.269  5066.6584  5308.487    10
#   create_pmap(el_10m) 59258.2326 59877.8646 60903.077 60678.284 62170.0964 63480.491    10

# data.table:setorder()
# Unit: milliseconds
#                        expr        min         lq       mean     median         uq        max neval
#   create_pmap(copy(el_10k))   331.3813   430.4172  1004.1038   486.5171   518.0146  5945.3911    10
#  create_pmap(copy(el_100k))   417.2192   452.8110   544.5129   591.1354   607.8555   641.2991    10
#    create_pmap(copy(el_1m))  2057.1037  2202.2641  4609.8769  2631.9217  7847.6545  8466.5252    10
#   create_pmap(copy(el_10m)) 23082.6826 28037.9299 31128.4557 29865.4468 33355.8728 42216.4454    10


el_1m <- generate_eventlog(1000000, 200000, c("campaign", "sale"), c(10, 3))
dt_1m <- as.data.table(el_1m)

microbenchmark(
  # data.frame
  head(el_1m, nrow(el_1m) - 1),
  el_1m[-nrow(el_1m),],
  tail(el_1m, nrow(el_1m) - 1),
  el_1m[-1,],
  # data.table
  head(dt_1m, nrow(dt_1m) - 1),
  dt_1m[-nrow(dt_1m),],
  tail(dt_1m, nrow(dt_1m) - 1),
  dt_1m[-1,],
  # options
  times = 10
)

# Unit: milliseconds
#                          expr       min        lq      mean    median        uq        max neval
#  head(el_1m, nrow(el_1m) - 1) 210.21150 220.92995 498.46559 227.53659 351.69505 2609.82393    10
#         el_1m[-nrow(el_1m), ] 221.22007 232.19248 522.66130 242.74177 772.59692 1577.07064    10
#  tail(el_1m, nrow(el_1m) - 1) 199.24440 221.85880 359.02000 229.58408 293.01069 1116.23355    10
#                   el_1m[-1, ] 221.73122 256.88776 557.31452 582.11652 790.43861  915.05392    10
#  head(dt_1m, nrow(dt_1m) - 1)  50.43247  60.00987 211.00094  64.51988  75.36679 1004.58526    10
#         dt_1m[-nrow(dt_1m), ]  45.35805  57.66475 136.56469  62.09029  73.12450  778.36658    10
#  tail(dt_1m, nrow(dt_1m) - 1)  60.19097  62.80756 110.81733  71.27415 102.08019  431.90562    10
#                   dt_1m[-1, ]  58.69383  61.06019  65.61778  63.77155  64.25639   93.41506    10


# `head()/tail()` ==> `data.table[]`
# Unit: milliseconds
#                        expr        min         lq       mean     median         uq        max neval
#   create_pmap(copy(el_10k))   339.9077   377.3973   642.3140   456.4188   971.7753  1240.7110    10
#  create_pmap(copy(el_100k))   411.8673   414.4656   535.7509   451.7886   633.8586   956.8731    10
#    create_pmap(copy(el_1m))  1181.9168  1206.9495  1629.7927  1542.5033  1881.3097  2672.0710    10
#   create_pmap(copy(el_10m)) 16923.7892 16957.7548 19407.6900 18383.3316 19774.4897 29064.6801    10

el_1m_2 <- generate_eventlog(
  size_of_eventlog = 1000 * 1000,
  number_of_cases = 100 * 1000,
  activity_categories_size = c(30, 5, 20, 5)
)

library(data.table)

el_10m <- generate_eventlog(10000000, 2000000, c("campaign", "sale"), c(10, 3))
profvis::profvis(pmap::create_pmap(data.table::copy(el_10m)))
