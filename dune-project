(lang dune 3.16)

(name subtext)

(generate_opam_files true)

(source
 (github thomasvolk/subtext))

(authors "Thomas Volk")

(maintainers "Thomas Volk")

(license LICENSE)

(documentation https://github.com/thomasvolk/subtext)

(package
 (name subtext)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune re)
 (tags
  (topics subtext notes cli)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
