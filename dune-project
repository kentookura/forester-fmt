(lang dune 3.7)

(name forester_format)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name forester_format)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune (forester (>= 4.2.0)) pretty_expressive pandoc alcotest)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
