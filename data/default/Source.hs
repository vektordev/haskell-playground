--imports

-- /imports

doFunc :: [String] -> Memory -> (Memory, [String])
evFunc :: [String] -> Memory -> (Memory, String)

doFunc ((a:b:[c]):(d:e:[f]):(g:h:[i]):[] ) mem = (1:mem, "1":"2":"3":"4":"5":"6":"7":"8":["9"])
evFunc (source:othersources) mem = (2:mem, source)

