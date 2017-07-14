DIR="docs"

pdf:
	Rscript --quiet _render.R "bookdown::pdf_book"

gitbook:
	Rscript --quiet _render.R "bookdown::gitbook"

tufte:
	Rscript --quiet _render.R "bookdown::tufte_html_book"

htmlbook:
	Rscript --quiet _render.R "bookdown::html_book"

html:
	Rscript --quiet _render.R "bookdown::html_document2" && mv main.html public/index.html

serve:
	browser-sync start --server $(DIR) --files $(DIR) --no-open --no-ui

all:
	Rscript --quiet _render.R

clean:
	rm -rf $(DIR)
