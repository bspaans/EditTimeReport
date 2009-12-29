import Report
import Text.XHtml
import Text.Printf

html :: Report Html
html = foldIR htmlAlgebra
htmlAlgebra :: SIndexedAlgebra y m d Html
htmlAlgebra = (concatHtml, id, id, d)
  where d = br


