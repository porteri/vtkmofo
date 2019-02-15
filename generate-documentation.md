---
project: vtkmofo
version: 1.0
author: Ian Porter
email:
project_github: https://github.com/porteri/vtkmofo
summary: Modern Fortran interface for vtk format
src_dir: src
media_dir: ./files
md_base_dir: ./
preprocess: false
display: public
         protected
         private
predocmark_alt: >
predocmark: <
docmark_alt:
docmark: !
source: true
exclude_dir:
graph: true
coloured_edges: true
sort: alpha
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc(anchorlink=False)
               markdown.extensions.smarty(smart_quotes=False)
license: bsd

---

{!README.md!}

[This document is a FORD project file, formatted with Pythonic Markdown. Build HTML documentation with the following command: ]:#
[  ford --no-search generate-documentation.md                                                                                 ]:#
[See https://github.com/cmacmackin/ford/wiki/Project-File-Options for more info on writing FORD project files                 ]:#

[Commands:]:#
[source: display source code corresponding to item being documented]:#
[graph: generate call graphs, module dependency graphs, derive type composition/inheritance graphs ]:#
[sort: different sorting schemes for the modules or procedures or programs or derived types (alpha = alphabetical see wiki).]:#
[extra_mods: documentation for intrinsic modules]:#

--------------------
