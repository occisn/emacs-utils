# emacs-utils

Personal utilities for Emacs.

This project is a kind of shelf providing many functions. With a few clearly indicated exceptions, these functions are self-supporting. They do not require any dependency, or to be built/integrated in any specific way. Because "the truly reusable code is the one that you can simply copy-paste".

File **clipboard.el**  
   - function `my/insert-string-in-clipboard`  
   - paste image from clipboard to file: see `images.el`
   
File **csv.el**  
   - function `my/parse-csv-file-to-list`  
   - function `my/write-CSV-to-file`  
   - function `my/reconciliate-two-csv-files`
   
File **files.el**  
   - function `my/copy-file-here`  
   - function `my/list-big-files-in-current-directory-and-subdirectories`  
   - function `my/list-directories-with-many-files-or-direct-subdirectories`  
   - function `my/list-directories-of-big-size`  
   - function `my/list-directories-containing-zip-files`  
   - function `my/find-files-with-same-size-in-same-subdirectory`
   
File **images.el**  
   - function `my/paste-image-from-clipboard-to-file-with-imagemagick`  
   - function `my/paste-image-from-clipboard-to-here`
   
File **macros.el**  
   - macro `aprogn`  
   - macro `amapcar`  
   - macro `awhen`  
   - macro `aif`
         
File **ocr.el**  
   - function `my/insert-ocr-clipboard`  
   - function `my/scanned-pdf-to-txt`

File **ollama.el**  
   - functions `my/ollama-call1` and `my/ollama-call2`

File **org.el**  
   - function `my/org-copy-link-or-inline-code-or-verbatim-or-block`  
   - function `my/save-region-as-html`  
   - function `my/org-copy-region-ready-to-be-pasted-into-Word-Teams-Thunderbird-Gmail`  
   - function `my/paste-clipboard-as-raw-html`  
   - function `my/html-to-org`  
   - function `my/org-paste-from-Teams-Word-as-org`

File **pdf.el**  
   - function `my/pdf-burst`  
   - function `my/pdf-extract`  
   - function `my/pdf-join`
   
File **thunderbird-and-eml-files**  
   - function `eml-add-date-at-beginning-of-eml-file`
   
File **windows.el**  
   - function `my/add-to-environment-variable`  
   - function `my/delete-to-recycle-bin`  
   - function `my--find-process`
   
File **zip.el**  
   - function `my/unzip`  
   - function `my/zip-content-of-current-directory`  
   - function `my/list-zip-content`  
   - function `my/zip-add-to-archive-present-in-same-directory`
   
Any comment? Open an [issue](https://github.com/occisn/emacs-utils/issues), or start a discussion [here](https://github.com/occisn/emacs-utils/discussions) or [at profile level](https://github.com/occisn/occisn/discussions).

(end of README)
