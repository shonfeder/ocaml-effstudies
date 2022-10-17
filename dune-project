(lang dune 2.9)
(cram enable)
(generate_opam_files true)

(name effstudies)
(license MIT)
(authors "Shon Feder")
(maintainers "Shon Feder")
(source (github shonfeder/effstudies))

(package
 (name effstudies)
 (synopsis "Short description")
 (description "Longer description")
 (depends
  (dune (>= 2.9))
  ocaml
  (ppx_inline_test :with-test)
  (ppx_expect :with-test)
  (alcotest :with-test)
  (qcheck :with-test)
  (qcheck-alcotest :with-test)
))
