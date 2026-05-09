; Inject Haskell into #{ ... }
((text) @injection.content
  (#match? @injection.content "#\\{[^}]*\\}")
  (#set! injection.language "haskell"))

; Inject Haskell into @{ ... } (routes often contain expressions)
((text) @injection.content
  (#match? @injection.content "@\\{[^}]*\\}")
  (#set! injection.language "haskell"))

; Inject Haskell into ^{ ... } (widgets)
((text) @injection.content
  (#match? @injection.content "\\^\\{[^}]*\\}")
  (#set! injection.language "haskell"))

((text) @injection.content
  (#match? @injection.content "#\\{(.|\\n)*?\\}")
  (#set! injection.language "haskell"))
