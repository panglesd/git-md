(lang dune 3.6)

(name git_md)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name git_md)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune hockmd logs cmdliner lwt_ssl (ppx_yojson_conv (< v0.16)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
