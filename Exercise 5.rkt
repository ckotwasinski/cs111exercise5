;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "./file_operations.rkt")

; Part 1

; backup! : Path Path -> Void
;
; Recursively copies all the files and subdirectories in the `from`
; directory to the `to` directory. This is a modified version of the
; `copy-tree` procedure we discussed in class.
;
; EFFECT: `to` and all its contents now exist
; EFFECT: may overwrite existing files at `to`
(define (backup! from to) 
  (begin
    ; create the destination directory if it doesn't already exist
    (unless (directory-exists? to)
      (make-directory! to))

    ; for each file (leaf node) in the origin directory,
    ; copy it over to the destination directory
    (for-each (λ (file)
         (unless (file-exists? (build-path from file)) ;added unless
                ; print the name of the file being copied into the REPL
                ; for more on how `printf` works, see Appendix 1 in the pdf
                  (begin
                  (printf "Copying file ~A to ~A~n" file to)
                  (copy-file! file
                              (build-path to (path-filename file))
                              #true))))
              (directory-files from))

    ; for each folder (recursive child node) in the origin directory,
    ; recursively `backup!` its contents
    (for-each (λ (subdir)
                (unless (directory-exists? (build-path from subdir)) ;added unless
                  (backup! subdir
                         ; add the subdirectory's name to the
                         ; end of the original destination path
                         (build-path to (path-filename subdir)))))
              (directory-subdirectories from))))

; Part 2

; count-files: path -> number
(define accumulator 0)

(define (count-files path)
  (if (empty? (directory-files (build-path path)))
     0
     (begin (for-each (λ (file)
                (set! accumulator (+ accumulator 1)))
               (directory-files (build-path path)))
            accumulator)))

; directory-size: path -> number
(define (directory-size path)
  (+ (foldl + 0
            (map file-size (directory-files path)))
     (foldl + 0
            (map directory-size (directory-subdirectories path)))))
                               
(check-expect (directory-size "test/test_2") 520)

; search-directory: string path -> (listof path)
(define (search-directory name path)
  (apply append(filter(λ(file) (string-contains? name
                                                 (path->string (path-filename file))))
                      (directory-files path))
         (map (λ(directions)
                (search-directory name directions))(directory-subdirectories path))))

; filter-directory: (path -> boolean) path -> (listof path)
(define (filter-directory predicate path)
  (apply append (filter predicate (directory-files path))
         (map (λ(sub-dir)(filter-directory predicate sub-dir))
              (directory-subdirectories path))))

; find-file-type: string path -> (listof path)
(define (find-file-type extension path)
  (append (filter (λ(file)(path-has-extension? file extension))
                  (directory-files path))
          (for-each (λ(subdir) (filter (λ(file)(path-has-extension? file extension))
                                       (directory-files subdir)))
                    (directory-subdirectories path))))

; file-type-disk-usage: string path -> number
(define (file-type-disk-usage extension path)
  (if  (empty? (directory-subdirectories path))
       (map file-size (find-file-type extension path))
       (apply + (append (map file-size (find-file-type extension path))
                        (map (file-type-disk-usage extension (directory-subdirectories path)))))))
