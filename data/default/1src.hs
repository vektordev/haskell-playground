--header: preproc definitions / safety declarations

-- /header

--imports

-- /imports

--Strong typing for doFunc's [String] param/retval - possibly giving information about the simulation the AI is in?

doFunc :: [String] -> Memory -> (Memory, [String])
evFunc :: [String] -> Memory -> (Memory, String)

doFunc ((a:b:[c]):(d:e:[f]):(g:h:[i]):[] ) mem = (1:mem, "1":"2":"3":"4":"5":"6":"7":"8":["9"])
evFunc (source:othersources) mem = (2:mem, source)

