-ifndef(__BANK_DAYS__).
-define(__BANK_DAYS__, true).

-define(bank_days, #{ {1,1} => true
                    , {1,2} => true
                    , {1,24} => true
                    , {4,22} => true
                    , {4,24} => true
                    , {4,25} => true
                    , {5,1} => true
                    , {6,1} => true
                    , {6,12} => true
                    , {6,13} => true
                    , {8,15} => true
                    , {11,30} => true
                    , {12,1} => true
                    , {12,25} => true
                    , {12,26} => true
                    }).

-endif.