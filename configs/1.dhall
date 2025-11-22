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
      , makeCfg 8 48 1.5 2.0 "${sock1}"
      , makeCfg 8 49 2.0 3.0 "${sock1}"
      ]
in configs