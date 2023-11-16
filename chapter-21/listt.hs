app [] _ = []
app _ [] = []
app (f:[]) xs = (f <$> xs)++[]
app (f:fs) xs = (f <$> xs)++(app fs xs)

seqA (fx:[]) = (:[]) <$> fx
seqA (fx:fxs) = ((:) <$> fx) `app` (seqA fxs)

