-type metric_name() :: atom() | string().

-record(counter, { name
                 , help
                 , labels = []
                 }).
-record(gauge, { name
               , help
               , labels = []
               }).
-record(histogram, { name
                   , help
                   , labels = []
                   , buckets
                   }).
