#lang racket
(module normal racket
  (provide path? path-string? path->string string->path build-path path-has-extension?
           file-exists? file-or-directory-modify-seconds file-size copy-file! rename-file-or-directory!
           delete-file! delete-directory!
           directory-exists? make-directory!
           directory-files directory-subdirectories)
  (provide path-directory path-filename)
  (require (for-syntax syntax/parse racket/sequence))

  (define (safe-path! name p pos)
    (unless (and (relative-path? p) (not (memq 'up (explode-path p))))
      (raise-argument-error name "a relative path with no `..'s" pos p)))

  ;; wrappers to make things safe

  (define (copy-file! src dest [exists-ok? #f])
    (safe-path! 'copy-file! src 0)
    (safe-path! 'dest-file! src 0)
    (copy-file src dest exists-ok?))

  (define (rename-file-or-directory! old new [exists-ok? #f])
    (safe-path! 'rename-file-or-directory! old 0)
    (safe-path! 'rename-file-or-directory! new 0)
    (rename-file-or-directory old new exists-ok?))

  (define (delete-file! p)
    (safe-path! 'delete-file! p 0)
    (delete-file p))

  (define (delete-directory! p)
    (safe-path! delete-directory! p 0)
    (delete-directory p))

  (define (make-directory! p)
    (safe-path! 'make-directory! p 0)
    (make-directory p))

  ;; Special stuff defined here


  (define (path-directory path)
    (define-values (directory filename must-be-dir?) (split-path path))
    directory)

  (define (path-filename path)
    (define-values (directory filename must-be-dir?) (split-path path))
    filename)

  (define (directory-subdirectories path)
    (filter directory-exists?
            (directory-list path #:build? #t)))

  (define (directory-files path)
    (filter file-exists?
            (directory-list path #:build? #t)))

  (define (path-has-extension? path extension)
    (define string (if (string? path)
                       path
                       (path->string path)))
    (define l (string-length string))
    (define ext-length (string-length extension))
    (define maybe-extension
      (if (< ext-length l)
          (substring string (- l ext-length) l)
          ""))
    (string=? extension maybe-extension)))

(module mocks racket
  (provide path? path-string? path->string string->path build-path path-has-extension?
           file-exists? file-or-directory-modify-seconds file-size copy-file! rename-file-or-directory!
           delete-file! delete-directory!
           directory-exists? make-directory!
           directory-files directory-subdirectories)
  (provide path-directory path-filename)

  (require (for-syntax syntax/parse))


  (define-syntax (check-expect stx)
    (syntax-parse stx
      [(n name:id result)
       #'(n name name result)]
      [(n (name:id a ...) result)
       #'(n name (name a ...) result)]
      [(_ str expr result)
       #`(unless (equal? expr result)
           #,(syntax/loc stx (error (string-append (~a str) " failed. expected " (~a result)
                                                   " given " (~a expr)))))]))

  (define-syntax-rule (with-tree t . b)
    (dynamic-wind
      (lambda ()
        (file-tree t)
        (clear-ops!))
      (lambda () . b)
      (lambda ()
        (file-tree 'no-tree-set)
        (clear-ops!))))
  (define (path-directory path)
    (call-with-values (λ () (split-path path))
                      (λ (directory filename must-be-dir?)
                        directory)))

  (define (path-filename path)
    (call-with-values (λ () (split-path path))
                      (λ (directory filename must-be-dir?) filename)))

  (struct file (modified in:size)
    #:extra-constructor-name make-file
    #:transparent)
  (struct dir (modified children)
    #:extra-constructor-name make-dir
    #:transparent)
  (define ft 'no-tree-set)
  (define file-tree
    (case-lambda
      [() ft]
      [(a) (set! ft a)]))

  (define set-tree! file-tree)
  (define operations null)
  (define (get-operations)
    operations)
  (define (clear-ops!) (set! operations null))

  (define (get p*)
    (define p (explode-path p*))
    (for/fold ([file-tree (file-tree)])
              ([p p]
               #:break (not file-tree))
      (and file-tree
           (match p
             ['same file-tree]
             [_
              (define p* (if (string? p) p (path->string p)))
              (and file-tree
                   (dir? file-tree)
                   (hash-ref (dir-children file-tree) p* #f))]))))

  (define (sanitize a)
    (for/list ([a a])
      (if (path? a)
          (path->string a)
          a)))

  (define-syntax-rule (define-operations id ...)
    (begin
      (define (id . a)
        (set! operations (cons `(id ,@(sanitize a)) operations)))
      ...))

  (define (file-exists? path)
    (file? (get path)))

  (define (file-or-directory-modify-seconds path)
    (unless (file-exists? path)
      (error "file does not exist: " path))
    (file-modified (get path)))

  (define (file-size path)
    (unless (file-exists? path)
      (error "file does not exist: " path))
    (file-in:size (get path)))

  (define-operations
    copy-file!
    rename-file-or-directory!
    make-directory!
    delete-directory!
    delete-file!)

  (define (directory-exists? path)
    (dir? (get path)))

  (define (current-directory)
    (build-path "."))

  (define (directory-list path)
    (unless (directory-exists? path)
      (error "directory does not exist: " path))
    (map string->path (hash-keys (dir-children (get path)))))

  (define (directory-files path)
    (unless (directory-exists? path)
      (error "directory does not exist: " path))
    (for/list ([(name node) (dir-children (get path))]
               #:when (file? node))
      (build-path
       (if (string? path) (string->path path) path)
       (string->path name))))

  (define (directory-subdirectories path)
    (unless (directory-exists? path)
      (error "directory does not exist: " path))
    (for/list ([(name node) (dir-children (get path))]
               #:when (dir? node))
      (build-path (if (string? path) (string->path path) path)
                  (string->path name))))

  (define (path-has-extension? path extension)
    (define string (if (string? path)
                       path
                       (path->string path)))
    (define l (string-length string))
    (define ext-length (string-length extension))
    (define maybe-extension
      (if (< ext-length l)
          (substring string (- l ext-length) l)
          ""))
    (string=? extension maybe-extension))

  (module+ backdoor
    (provide file-tree get get-operations clear-ops!
             dir file
             set-tree!
             make-file make-dir hash set list->set))

  (module+ test
    (require rackunit)
    (file-tree
     (dir 0
          (hash "thing.txt"
                (make-file 0 12)
                "home"
                (dir 1 (hash "florence"
                             (dir 2
                                  (hash "thing.txt"
                                        (make-file 3 3)))))
                "bin"
                (dir 0 (hash "ls"
                             (make-file 0 10)
                             "sh"
                             (make-file 0 15))))))

    (check-true (directory-exists? "."))

    (check-true (file-exists? "bin/ls"))
    (check-true (file-exists? "home/florence/thing.txt"))
    (check-true (file-exists? "thing.txt"))
    (check-false (file-exists? "sfsef"))
    (check-false (file-exists? "home/florence"))

    (check-equal? (file-or-directory-modify-seconds "home/florence/thing.txt")
                  3)

    (check-equal? (file-size "home/florence/thing.txt")
                  3)
    (check-equal? (file-size "bin/sh")
                  15)

    (check-true (directory-exists? "home/florence"))
    (check-true (directory-exists? "bin"))
    (check-false (directory-exists? "bin/sh"))

    (check-equal? (list->set (map path->string (directory-files "home/florence")))
                  (set "home/florence/thing.txt"))
    (check-equal? (list->set (map path->string (directory-files "bin")))
                  (set "bin/sh" "bin/ls"))

    (check-equal? (list->set (map path->string (directory-subdirectories "home")))
                  (set "home/florence"))))

(module+ backdoor
  (require (submod ".." mocks backdoor))
  (require (for-syntax syntax/parse racket/syntax))
  (define-syntax safe-reprovide
    (syntax-parser
      [(_ n:id ...)
       #:with (k ...) (generate-temporaries #'(n ...))
       #'(begin
           (provide (rename-out [k n] ...))
           (define (k . a) (apply n a)) ...)]))
  (safe-reprovide file-tree get get-operations clear-ops!
                  set-tree!
                  file dir
                  make-file make-dir hash use-mocks!
                  list->set set)
  )

(require (prefix-in normal: (submod "." normal))
         (prefix-in backdoor: (submod "." mocks))
         (for-syntax syntax/parse racket/syntax))

(define use-mocks? #f)
(define (use-mocks!) (set! use-mocks? #t))

(define-syntax provide-with-mocks
  (syntax-parser
    [(_ n:id ...)
     #'(begin (provide-with-mock n) ...)]))
(define-syntax provide-with-mock
  (syntax-parser
    [(_ n:id)
     #`(begin
         (provide n)
         (define (n . a)
           (if use-mocks?
               (apply #,(format-id #'n "~a:~a" 'backdoor #'n) a)
               (apply #,(format-id #'n "~a:~a" 'normal #'n) a))))]))

(provide-with-mocks
 path? path-string? path->string string->path build-path path-has-extension?
 file-exists? file-or-directory-modify-seconds file-size copy-file! rename-file-or-directory!
 delete-file! delete-directory!
 directory-exists? make-directory!
 directory-files directory-subdirectories
 path-directory path-filename)





;(path-has-extension? (current-directory) ".jpg")
;(path-has-extension? (build-path (current-directory)
;                                 "foo.jpg")
;                     ".jpg")
