ocamlfind dep -syntax camlp5o -package GT-p5,OCanren.syntax,GT.syntax.all  *.ml > .depend
# noCanren  -o Topsort.ml -unnesting-mode   Topsort.ml2mk.ml
ocamlfind opt -c -rectypes -g -package GT,OCanren -inline 10  -syntax camlp5o -package GT-p5,OCanren.syntax,GT.syntax.all  Topsort.ml
ocamlfind opt -c -rectypes -g -package GT,OCanren -inline 10  -syntax camlp5o -package GT-p5,OCanren.syntax,GT.syntax.all  TopsortD.ml
ocamlfind opt -o Topsort -rectypes -g -package GT,OCanren -inline 10  -linkpkg Topsort.cmx TopsortD.cmx
ocamlfind c -c -rectypes -g -package GT,OCanren -syntax camlp5o -package GT-p5,OCanren.syntax,GT.syntax.all  Topsort.ml
ocamlfind c -c -rectypes -g -package GT,OCanren -syntax camlp5o -package GT-p5,OCanren.syntax,GT.syntax.all  TopsortD.ml
ocamlfind c -o Topsort.byte -rectypes -g -package GT,OCanren  -linkpkg Topsort.cmo TopsortD.cmo

