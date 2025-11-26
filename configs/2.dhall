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
      , makeCfg 9 36 1.85   3.0 "${sock1}" -- low 5
      , makeCfg 9 37 15.20   16.0 "${sock1}" -- 2
      , makeCfg 9 38 85.2   87.0 "${sock1}" -- pat 1 
      , makeCfg 9 39 76.5   78.0 "${sock1}" -- pat 2 
      , makeCfg 9 40 96.5   98.0 "${sock1}" -- pat 3 
      , makeCfg 9 41 28.3   30.0 "${sock1}" -- 3 
      ]
in configs