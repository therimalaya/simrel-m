DIR="docs"

manuscript:
	Rscript --quiet _render.R "manuscript::manuscript"

gitbook:
	Rscript --quiet _render.R "bookdown::gitbook"

pdf:
	Rscript --quiet _render.R "bookdown::pdf_book"

tufte:
	Rscript --quiet _render.R "bookdown::tufte_html_book"

serve:
	browser-sync start --server $(DIR) --files $(DIR) --no-open --no-ui

all:
	Rscript --quiet _render.R

clean:
	rm -rf $(DIR)
