# emacs-utils

Personal utilities for Emacs.

This project is a kind of shelf providing many functions. With a few clearly indicated exceptions, these functions are self-supporting. They do not require any dependency, or to be built/integrated in any specific way. Because "the truly reusable code is the one that you can simply copy-paste".

## Usage

Each file is independent. Load only what you need:

```elisp
(load-file "~/path/to/emacs-utils/pdf.el")
```

Or add the directory to your `load-path`:

```elisp
(add-to-list 'load-path "~/path/to/emacs-utils/")
(load "pdf")
```

**Customization required:** several functions shell out to external programs (ImageMagick, PDFtk, Tesseract, 7-Zip) whose paths are hardcoded as placeholders like `c:/.../convert.exe`. Edit these paths inside the relevant `.el` file to match your install before calling those functions.

## Tags

Each function is annotated with the context in which it works:

- `[dired]` — must be invoked from a dired buffer
- `[Windows]` — Windows-only (uses PowerShell or Windows-path conversion)
- `[org-mode]` — operates on an Org-mode buffer or Org content
- `[ImageMagick]`, `[PDFtk]`, `[Tesseract]`, `[7-Zip]`, `[Ollama]` — external program required
- `[f]` — requires the `f` Emacs package
- No tag — pure elisp, works anywhere

## Functions

File **clipboard.el**
   - function `my/insert-string-in-clipboard`
     - Example: `(my/insert-string-in-clipboard "hello world")`
   - paste image from clipboard to file: see `images.el`

File **csv.el**
   - function `my/parse-csv-file-to-list`
     - Example: `(my/parse-csv-file-to-list "c:/data/input.csv" :separator "," :verbose t)`
   - function `my/write-CSV-to-file`
     - Example: `(my/write-CSV-to-file '(("a" "b") ("c" "d")) "c:/data/out.csv")`
   - function `my/reconciliate-two-csv-files`
     - Example: `(my/reconciliate-two-csv-files :input-file-1 "ledger.csv" :key-column-1 "A" :value-column-1 "C" :input-file-2 "bank.csv" :key-column-2 "B" :value-column-2 "D" :output-file "reconciliation.csv")`

File **files.el**
   - function `my/copy-file-here` `[dired]`
     - Example: in a dired buffer, place point on a file then `M-x my/copy-file-here`
   - function `my/list-big-files-in-current-directory-and-subdirectories` `[dired]` `[f]`
     - Example: in a dired buffer, `M-x my/list-big-files-in-current-directory-and-subdirectories` then enter `50` (Mo)
   - function `my/list-directories-with-many-files-or-direct-subdirectories` `[dired]` `[f]`
     - Example: in a dired buffer, `M-x my/list-directories-with-many-files-or-direct-subdirectories` then enter `100`
   - function `my/list-directories-of-big-size` `[dired]` `[f]`
     - Example: in a dired buffer, `M-x my/list-directories-of-big-size` then enter `500` (Mo)
   - function `my/list-directories-containing-zip-files` `[dired]` `[f]`
     - Example: in a dired buffer at the top of an archive tree, `M-x my/list-directories-containing-zip-files`
   - function `my/find-files-with-same-size-in-same-subdirectory` `[dired]`
     - Example: in a dired buffer, `M-x my/find-files-with-same-size-in-same-subdirectory` to detect duplicate-by-size candidates

File **images.el**
   - function `my/paste-image-from-clipboard-to-file-with-imagemagick` `[ImageMagick]`
     - Example: `(my/paste-image-from-clipboard-to-file-with-imagemagick "c:/tmp/snapshot.png" t)`
   - function `my/paste-image-from-clipboard-to-here` `[dired]` `[ImageMagick]`
     - Example: in a dired buffer, `M-x my/paste-image-from-clipboard-to-here` then type a name like `screenshot-2026-05-23`

File **macros.el**
   - macro `aprogn`
     - Example: `(aprogn 3 (* it 2) (+ it 1))` returns `7`
   - macro `amapcar`
     - Example: `(amapcar (* it it) '(1 2 3 4))` returns `(1 4 9 16)`
   - macro `awhen`
     - Example: `(awhen (assoc 'x '((x . 1) (y . 2))) (cdr it))` returns `1`
   - macro `aif`
     - Example: `(aif (gethash "k" my-ht) (format "found: %s" it) "missing")`

File **ocr.el**
   - function `my/insert-ocr-clipboard` `[ImageMagick]` `[Tesseract]`
     - Example: copy a text screenshot into the clipboard, then `M-x my/insert-ocr-clipboard` in any buffer
   - function `my/scanned-pdf-to-txt` `[dired]` `[ImageMagick]` `[Tesseract]`
     - Example: in a dired buffer, place point on a scanned PDF then `M-x my/scanned-pdf-to-txt`

File **ollama.el**
   - function `my/ollama-call1` `[Ollama]`
     - Example: `(my/ollama-call1 "define recursion in three sentences")`
   - function `my/ollama-call2` `[Ollama]`
     - Example: `(my/ollama-call2 "summarize the Treaty of Westphalia" "llama3")` (prints duration and token stats)

File **org.el**
   - function `my/org-copy-link-or-inline-code-or-verbatim-or-block` `[org-mode]`
     - Example: in an Org buffer, place point inside `=foo=`, `~bar~`, a `#+BEGIN_SRC` block, on a link, or on a word, then `M-x my/org-copy-link-or-inline-code-or-verbatim-or-block`
   - function `my/save-region-as-html`
     - Example: select a region, then `(my/save-region-as-html "c:/tmp/region.html")`
   - function `my/org-copy-region-ready-to-be-pasted-into-Word-Teams-Thunderbird-Gmail` `[Windows]` `[org-mode]`
     - Example: select an Org region, `M-x my/org-copy-region-ready-to-be-pasted-into-Word-Teams-Thunderbird-Gmail`, then paste into Outlook/Teams/Gmail
   - function `my/paste-clipboard-as-raw-html` `[Windows]`
     - Example: copy formatted text from a browser, then in a buffer `M-x my/paste-clipboard-as-raw-html`
   - function `my/html-to-org`
     - Example: `(my/html-to-org "<h1>Title</h1><p>Hello <b>world</b></p>")`
   - function `my/org-paste-from-Teams-Word-as-org` `[Windows]` `[org-mode]`
     - Example: copy a message in Teams or Word, then in an Org buffer `M-x my/org-paste-from-Teams-Word-as-org`

File **pdf.el**
   - function `my/pdf-burst` `[dired]` `[PDFtk]`
     - Example: in a dired buffer, place point on a PDF then `M-x my/pdf-burst` (produces `page_001_of_FILE.pdf`, etc.)
   - function `my/pdf-extract` `[dired]` `[PDFtk]`
     - Example: in a dired buffer, place point on a PDF, `M-x my/pdf-extract`, then enter `2` and `4` to extract pages 2-4
   - function `my/pdf-join` `[dired]` `[PDFtk]`
     - Example: in a dired buffer, mark several PDFs then `M-x my/pdf-join` and enter `combined.pdf`

File **thunderbird-and-eml-files.el**
   - function `my/eml-add-date-at-beginning-of-eml-file` `[dired]`
     - Example: in a dired buffer, place point on an `.eml` file then `M-x my/eml-add-date-at-beginning-of-eml-file` (renames it with `YYYY-MM-DD _` prefix)

File **windows.el**
   - function `my/add-to-environment-variable`
     - Example: `(my/add-to-environment-variable "PATH" "ImageMagick" "c:/tools/ImageMagick/")`
   - function `my/delete-to-recycle-bin` `[Windows]`
     - Example: `(my/delete-to-recycle-bin "c:/tmp/old-file.txt")`
   - function `my--find-process` `[Windows]`
     - Example: `(my--find-process "chrome.exe")`

File **zip.el**
   - function `my/unzip` `[dired]` `[7-Zip]`
     - Example: in a dired buffer, place point on a `.zip` then `M-x my/unzip` (asks whether to create a sub-directory)
   - function `my/zip-content-of-current-directory` `[dired]` `[7-Zip]`
     - Example: in a dired buffer, `M-x my/zip-content-of-current-directory` then enter `archive`
   - function `my/list-zip-content` `[dired]` `[7-Zip]`
     - Example: in a dired buffer, place point on a `.zip` then `M-x my/list-zip-content`
   - function `my/zip-add-to-archive-present-in-same-directory` `[dired]` `[7-Zip]`
     - Example: in a dired buffer, mark a few files (with a `.zip` already in the same directory) then `M-x my/zip-add-to-archive-present-in-same-directory`

Any comment? Open an [issue](https://github.com/occisn/emacs-utils/issues), or start a discussion [here](https://github.com/occisn/emacs-utils/discussions) or [at profile level](https://github.com/occisn/occisn/discussions).

(end of README)
