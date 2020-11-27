--lista de polimorfismo 

--lista de concat

funConcat:: [[t]]->[t]
funConcat [] = []
funConcat (a:[]) = a
funConcat (a:x) = a++(funConcat x)


unZip:: [(t,t)]->[[t]]
unZip []=[]
unZip lista = [primeiro,segundo]
    where
        primeiro=[fst e|e<-lista]
        segundo=[snd e|e<-lista]

funInit:: [t]->[t]
funInit []=[]
funInit (a:x) = (take (length (a:x)-1) (a:x))


funTome:: Int->[t]->[t]
funTome _ []=[]
funTome n (a:x) = (take n (a:x))


funTire:: Int->[t]->[t]
funTire _ []=[]
funTire n (a:x) = (drop n (a:x))