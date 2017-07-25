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

coverletter:
	Rscript --quiet -e "rmarkdown::render('CoverLetter.Rmd', output_format = rmarkdown::pdf_document(template = 'templates/CoverLetter.tex'), output_dir = 'letters')"

letter:
	Rscript --quiet -e "rmarkdown::render('Letter.Rmd', output_format = rmarkdown::pdf_document(template = 'templates/Letter.tex', keep_tex = TRUE), output_dir = 'letters')"

serve:
	browser-sync start --server $(DIR) --files $(DIR) --no-open --no-ui

all:
	Rscript --quiet _render.R && mv main.html public/index.html

clean:
	rm -rf $(DIR)
