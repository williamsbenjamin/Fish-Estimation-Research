#Calculate Total and PSE


tot_pse <- function(al_tot,al_pse,
                    fl_tot,fl_pse,
                    ms_tot=0,ms_pse=0){
  tot <- al_tot + fl_tot + ms_tot
  se_al <- al_pse*al_tot
  se_fl <- fl_pse*fl_tot
  se_ms <- ms_pse*ms_tot
  se_tot <- sqrt(se_al^2 + se_fl^2 + se_ms^2)
  pse_tot <- se_tot / tot
  return(list(total = tot,
              pse = pse_tot))
}

tot_pse()

#RED SNAPPER
#h
tot_pse(86495,0.193,
        168030,0.191,
        5374,0.242)
#r
tot_pse(178341,0.166,
        513226,0.138,
        5071,0.371)
#vermilion
#r
tot_pse(508,1.04,
        59554,0.396)
#spanish
#h
tot_pse(15390,0.48,
        150785,0.192,
        9256,0.429)
#r
tot_pse(452,0.54,
        39180,0.215)
#red porgy
#r
tot_pse(1321,0.505,
        409,0.867)
#gray trigger
#r
tot_pse(109099,0.221,
        361970,0.178)

#king mackerel
#h
tot_pse(8174,0.234,
        107692,0.134,
        320,0.516)
#r
tot_pse(1599,0.575,
        19316,0.246)
#cobia
#h
tot_pse(239,0.54,
        1697,0.376,
        77,1.038)
#r
tot_pse(545,1.069,
        11517,0.346,
        49,0.963)
#gray snap
#r
tot_pse(8291,0.363,
        117199,0.221)
#great amberjack
#r
tot_pse(19849,0.342,
        45019,0.27)
#lil tunny
#R
tot_pse(10843,0.306,
        70941,0.226,
        46,1.046)
#R
tot_pse(12469,0.758,
        22200,0.429)
#red drum
#h
tot_pse(8558,0.371,
        9464,0.201,
        2589,0.392)
#r
tot_pse(2820,0.46,
        13358,0.29,
        513,0.656)
#sheep
#h
tot_pse(10211,0.324,
        3186,0.381,
        924,0.964)
#r
tot_pse(725,0.462,
        1691,0.509)

