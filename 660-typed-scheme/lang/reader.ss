(module reader scheme/base
  (require (prefix-in r: "../typed-reader.ss"))
  (provide (rename-out [r:read read] [r:read-syntax read-syntax])))