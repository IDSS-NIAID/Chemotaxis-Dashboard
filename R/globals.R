# globals.R
# for all of those pesky "no visible binding" notes

                                           #   mpare_two_functions
                                           #  o  ne_experiment
                                           # c  o  process_experiments
globalVariables(c('angle_mean',            #    x
                  'angle_median',          #    x
                  'angle_migration',       #    x
                  'angle_sd',              #    x
                  'ce',                    #    x
                  'ce_mean',               #    x
                  'ce_median',             #    x
                  'ce_sd',                 #    x
                  'channel',               #    x
                  'channel_a',             #    x
                  'cross_at',              #    x
                  'd',                     #    x
                  'delta_x',               #    x
                  'delta_y',               #    x
                  'directed_v_undirected', #    x
                  'distance_traveled',     #    x
                  'dvud',                  #    x
                  'dvud_p',                #    x
                  'experiment',            #    x
                  'finished',              #    x
                  'Frame',                 #    x
                  'frames',                #    x
                  'f.tmp',                 # x
                  'grp',                   #    x
                  'l',                     #    x
                  'max_y',                 #    x
                  'max_v',                 #    x
                  'max_v_mean',            #    x
                  'max_v_median',          #    x
                  'max_v_sd',              #    x
                  'minutes',               #    x
                  'nchannels',             #    x
                  'nsamps',                #    x
                  'ntrts',                 #    x
                  'perm_lab',              # x
                  'prop_finished',         #    x
                  'tot_finished',          #    x
                  'Track',                 # x  x
                  'treatment',             #    x
                  'v',                     #    x
                  'v_x',                   #    x
                  'v_y',                   #    x
                  'X',                     #    x  x
                  'x',                     #    x
                  'Y',                     #    x  x
                  'y',                     #    x
                  'y_max',                 #    x
                  'y_min'                  #    x
                  ))