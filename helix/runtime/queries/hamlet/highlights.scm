; --- Comments ---
((text) @comment
  (#match? @comment "^\\s*\\$#.*"))

; --- Control keywords ---
((text) @keyword.control
  (#match? @keyword.control
    "\\$(if|then|else|maybe|forall|case|of|with|doctype|newline|else-if)\\b"))

; --- Interpolation #{...} ---
  ; Separate delimiters from content
((text) @punctuation.special
  (#match? @punctuation.special "#\\{|\\}"))

((text) @embedded
  (#match? @embedded "#\\{[^}]*\\}"))

; --- URL/route interpolation @{...} ---
((text) @function
  (#match? @function "@\\{[^}]*\\}"))

; --- Hamlet attributes ^{...} ---
((text) @type
  (#match? @type "\\^\\{[^}]*\\}"))

; --- Tags (basic approximation) ---
((text) @tag
  (#match? @tag "^\\s*%[a-zA-Z0-9:-]+"))

; --- Classes (.foo) ---
((text) @attribute
  (#match? @attribute "\\.[a-zA-Z0-9_-]+"))

; --- IDs (#foo) ---
((text) @constant
  (#match? @constant "#[a-zA-Z0-9_-]+"))

; --- Strings (quoted attributes) ---
((text) @string
  (#match? @string "\"[^\"]*\""))
