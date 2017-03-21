DIR="docs"

pdf:
	Rscript --quiet _render.R "bookdown::pdf_book"

gitbook:
	Rscript --quiet _render.R "bookdown::gitbook"

all:
	Rscript --quiet _render.R

serve:
	browser-sync start --server $(DIR) --files $(DIR)
