let Config : Type =
      {
        cfgChannel : Natural
      , cfgNote : Natural
      , cfgFrom : Double
      , cfgTo : Double
      , cfgSocket : Text
      }
let makeCfg =  \(ch: Natural)
            -> \(n: Natural)
            -> \(f: Double)
            -> \(t: Double)
            -> \(s: Text)
            -> { cfgChannel = ch
               , cfgNote = n
               , cfgFrom = f
               , cfgTo = t
               , cfgSocket = s
               }
let sock1 : Text = "/tmp/mpvsocket"
let configs : List Config =
      [
      , makeCfg 9 36 2.6   3.0 "${sock1}" -- light
      , makeCfg 9 37 12.17 13.0 "${sock1}" -- cup
      , makeCfg 9 38 22.9  23.5 "${sock1}" -- rice cooker
      , makeCfg 9 39 26.5  27.5 "${sock1}" -- fridge upper
      , makeCfg 9 40 29.1  29.5 "${sock1}" -- fridge lower
      , makeCfg 9 41 32.5  33.5 "${sock1}" -- microwave open
      , makeCfg 9 42 34.25  35.0 "${sock1}" -- microwave close
      , makeCfg 9 43 39.5  40.5 "${sock1}" -- microwave cancel
      ]
in configs