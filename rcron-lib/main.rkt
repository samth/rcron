#lang racket/base

;; rcron - Cron expression parsing and system-level cron job scheduling.

(require racket/contract/base
         racket/file
         racket/math
         racket/port
         racket/string
         racket/system)

;; ============================================================================
;; Cron expression struct (bitset representation)
;; ============================================================================

;; minutes:           integer, bits 0-59
;; hours:             integer, bits 0-23
;; days:              integer, bits 1-31
;; months:            integer, bits 1-12
;; weekdays:          integer, bits 0-6 (0=Sunday)
;; days-is-wildcard:  boolean
;; weekdays-is-wildcard: boolean
(struct cron-expr (minutes hours days months weekdays days-is-wildcard? weekdays-is-wildcard?)
  #:transparent)

;; ============================================================================
;; Name lookup tables
;; ============================================================================

(define weekday-names
  (hash "sun"
        0
        "mon"
        1
        "tue"
        2
        "wed"
        3
        "thu"
        4
        "fri"
        5
        "sat"
        6
        "sunday"
        0
        "monday"
        1
        "tuesday"
        2
        "wednesday"
        3
        "thursday"
        4
        "friday"
        5
        "saturday"
        6))

(define month-names
  (hash "jan"
        1
        "feb"
        2
        "mar"
        3
        "apr"
        4
        "may"
        5
        "jun"
        6
        "jul"
        7
        "aug"
        8
        "sep"
        9
        "oct"
        10
        "nov"
        11
        "dec"
        12
        "january"
        1
        "february"
        2
        "march"
        3
        "april"
        4
        "june"
        6
        "july"
        7
        "august"
        8
        "september"
        9
        "october"
        10
        "november"
        11
        "december"
        12))

;; ============================================================================
;; Bit operations
;; ============================================================================

(define (bit-set? n pos)
  (bitwise-bit-set? n pos))

(define (bits-range min max)
  (for/fold ([bits 0]) ([i (in-range min (add1 max))])
    (bitwise-ior bits (arithmetic-shift 1 i))))

(define all-minutes (bits-range 0 59))
(define all-hours (bits-range 0 23))
(define all-days (bits-range 1 31))
(define all-months (bits-range 1 12))
(define all-weekdays (bits-range 0 6))

;; ============================================================================
;; Nickname parsing
;; ============================================================================

(define (parse-nickname str)
  (define s (string-downcase str))
  (cond
    [(or (string=? s "@yearly") (string=? s "@annually"))
     (cron-expr (arithmetic-shift 1 0)
                (arithmetic-shift 1 0)
                (arithmetic-shift 1 1)
                (arithmetic-shift 1 1)
                all-weekdays
                #f
                #t)]
    [(string=? s "@monthly")
     (cron-expr (arithmetic-shift 1 0)
                (arithmetic-shift 1 0)
                (arithmetic-shift 1 1)
                all-months
                all-weekdays
                #f
                #t)]
    [(string=? s "@weekly")
     (cron-expr (arithmetic-shift 1 0)
                (arithmetic-shift 1 0)
                all-days
                all-months
                (arithmetic-shift 1 0)
                #t
                #f)]
    [(or (string=? s "@daily") (string=? s "@midnight"))
     (cron-expr (arithmetic-shift 1 0) (arithmetic-shift 1 0) all-days all-months all-weekdays #t #t)]
    [(string=? s "@hourly")
     (cron-expr (arithmetic-shift 1 0) all-hours all-days all-months all-weekdays #t #t)]
    [else #f]))

;; ============================================================================
;; Field parsing
;; ============================================================================

(define (parse-value str min-val max-val kind)
  (cond
    [(eq? kind 'weekday)
     (define v (hash-ref weekday-names (string-downcase str) #f))
     (or v (parse-numeric-value str min-val max-val kind))]
    [(eq? kind 'month)
     (define v (hash-ref month-names (string-downcase str) #f))
     (or v (parse-numeric-value str min-val max-val kind))]
    [else (parse-numeric-value str min-val max-val kind)]))

(define (parse-numeric-value str min-val max-val kind)
  (define n (string->number str))
  (cond
    [(not n) #f]
    [(not (exact-nonnegative-integer? n)) #f]
    ;; Weekdays accept 0-7 (7 = Sunday, folded to bit 0 after bitset computation)
    [(and (eq? kind 'weekday) (> n 7)) #f]
    [(and (not (eq? kind 'weekday)) (or (< n min-val) (> n max-val))) #f]
    [(and (eq? kind 'weekday) (< n min-val)) #f]
    [else n]))

(define (string-index-of str ch)
  (for/first ([i (in-range (string-length str))]
              #:when (char=? (string-ref str i) ch))
    i))

(define (split-range str)
  (define idx (string-index-of str #\-))
  (cond
    [(not idx) #f]
    [(= idx 0) #f]
    [(= idx (sub1 (string-length str))) #f]
    [else
     (define rest (substring str (add1 idx)))
     (if (string-index-of rest #\-)
         #f
         (list (substring str 0 idx) rest))]))

(define (parse-field field min-val max-val kind)
  (when (string=? field "")
    (error 'cron-parse "empty field"))
  (define parts (string-split field "," #:trim? #f))
  (for/fold ([result 0]) ([part (in-list parts)])
    (when (string=? part "")
      (error 'cron-parse "empty element in list"))
    (define slash-parts (string-split part "/" #:trim? #f))
    (when (> (length slash-parts) 2)
      (error 'cron-parse "invalid step expression: ~a" part))
    (define base (car slash-parts))
    (define step-str (and (= (length slash-parts) 2) (cadr slash-parts)))
    (define step
      (cond
        [step-str
         (define n (string->number step-str))
         (when (or (not n) (not (exact-positive-integer? n)))
           (error 'cron-parse "invalid step value: ~a" step-str))
         n]
        [else 1]))

    (define-values (range-min range-max)
      (cond
        [(string=? base "*") (values min-val max-val)]
        [else
         (define range-parts (split-range base))
         (cond
           [range-parts
            (define lo (parse-value (car range-parts) min-val max-val kind))
            (define hi (parse-value (cadr range-parts) min-val max-val kind))
            (when (or (not lo) (not hi))
              (error 'cron-parse "invalid range value in: ~a" base))
            (when (> lo hi)
              (error 'cron-parse "invalid range ~a-~a" lo hi))
            (values lo hi)]
           [else
            (define v (parse-value base min-val max-val kind))
            (unless v
              (error 'cron-parse "invalid value: ~a" base))
            (values v (if step-str max-val v))])]))

    (define field-bits
      (bitwise-ior result
                   (for/fold ([bits 0]) ([i (in-range range-min (add1 range-max) step)])
                     (bitwise-ior bits (arithmetic-shift 1 i)))))
    ;; Fold weekday bit 7 (Sunday-as-7) into bit 0 (Sunday-as-0)
    (if (and (eq? kind 'weekday) (bitwise-bit-set? field-bits 7))
        (bitwise-ior (bitwise-and field-bits (bitwise-not (arithmetic-shift 1 7))) 1)
        field-bits)))

;; ============================================================================
;; Main parser
;; ============================================================================

(define (cron-parse expr-str)
  (define expr (string-trim expr-str))
  (cond
    [(and (> (string-length expr) 0) (char=? (string-ref expr 0) #\@))
     (or (parse-nickname expr) (error 'cron-parse "unknown nickname: ~a" expr))]
    [else
     (define fields (string-split expr))
     (when (> (length fields) 5)
       (error 'cron-parse "too many fields (expected 5, got ~a)" (length fields)))
     (when (< (length fields) 5)
       (error 'cron-parse "too few fields (expected 5, got ~a)" (length fields)))
     (define days-bits (parse-field (list-ref fields 2) 1 31 'none))
     (define weekdays-bits (parse-field (list-ref fields 4) 0 6 'weekday))
     (cron-expr (parse-field (list-ref fields 0) 0 59 'none)
                (parse-field (list-ref fields 1) 0 23 'none)
                days-bits
                (parse-field (list-ref fields 3) 1 12 'month)
                weekdays-bits
                (= days-bits all-days)
                (= weekdays-bits all-weekdays))]))

;; ============================================================================
;; Date/time helpers (UTC)
;; ============================================================================

(define (days-in-month month year)
  (case month
    [(1 3 5 7 8 10 12) 31]
    [(4 6 9 11) 30]
    [(2) (if (leap-year? year) 29 28)]
    [else (error 'days-in-month "invalid month: ~a" month)]))

(define (leap-year? year)
  (or (and (zero? (modulo year 4)) (not (zero? (modulo year 100)))) (zero? (modulo year 400))))

(define (day-of-week year month day)
  (define t (vector 0 3 2 5 0 3 5 1 4 6 2 4))
  (define y
    (if (< month 3)
        (sub1 year)
        year))
  (modulo (+ y (quotient y 4) (- (quotient y 100)) (quotient y 400) (vector-ref t (sub1 month)) day)
          7))

(define (seconds->utc-components secs)
  (define d (seconds->date secs #f))
  (values (date-year d)
          (date-month d)
          (date-day d)
          (date-hour d)
          (date-minute d)
          (date-second d)
          (date-week-day d)))

(define (utc-components->seconds year month day hour minute second)
  (find-seconds-utc second minute hour day month year))

(define (find-seconds-utc second minute hour day month year)
  (define jdn (gregorian->jdn year month day))
  (define epoch-jdn (gregorian->jdn 1970 1 1))
  (define day-diff (- jdn epoch-jdn))
  (+ (* day-diff 86400) (* hour 3600) (* minute 60) second))

(define (gregorian->jdn year month day)
  (define a (quotient (- 14 month) 12))
  (define y (+ year 4800 (- a)))
  (define m (+ month (* 12 a) -3))
  (+ day
     (quotient (+ (* 153 m) 2) 5)
     (* 365 y)
     (quotient y 4)
     (- (quotient y 100))
     (quotient y 400)
     -32045))

(define (normalize-date year month day hour minute second)
  (define extra-hours (quotient minute 60))
  (define norm-minute (modulo minute 60))
  (define hour2 (+ hour extra-hours))
  (define extra-days (quotient hour2 24))
  (define norm-hour (modulo hour2 24))
  (define day2 (+ day extra-days))
  (define extra-years (quotient (sub1 month) 12))
  (define norm-month0 (add1 (modulo (sub1 month) 12)))
  (define year2 (+ year extra-years))
  (let loop ([y year2]
             [m norm-month0]
             [d day2])
    (define dim (days-in-month m y))
    (cond
      [(> d dim)
       (define new-m (add1 m))
       (if (> new-m 12)
           (loop (add1 y) 1 (- d dim))
           (loop y new-m (- d dim)))]
      [(<= d 0)
       (define prev-m (sub1 m))
       (define-values (py pm)
         (if (<= prev-m 0)
             (values (sub1 y) 12)
             (values y prev-m)))
       (loop py pm (+ d (days-in-month pm py)))]
      [else (values y m d norm-hour norm-minute second)])))

;; ============================================================================
;; Next occurrence calculator
;; ============================================================================

(define (cron-next expr from-secs)
  (define-values (yr mo dy hr mn sc _wd) (seconds->utc-components (exact-floor from-secs)))

  (define mn2 (add1 mn))
  (define-values (start-yr start-mo start-dy start-hr start-mn start-sc)
    (normalize-date yr mo dy hr mn2 0))

  (define max-iterations (* 1500 24 60))

  (let loop ([year start-yr]
             [month start-mo]
             [day start-dy]
             [hour start-hr]
             [minute start-mn]
             [iter 0])
    (cond
      [(>= iter max-iterations) #f]
      [else
       (define-values (ny nm nd nh nmin _ns) (normalize-date year month day hour minute 0))
       (define wd (day-of-week ny nm nd))

       (cond
         [(not (bit-set? (cron-expr-months expr) nm)) (loop ny (add1 nm) 1 0 0 (add1 iter))]

         [(let ()
            (define day-ok (bit-set? (cron-expr-days expr) nd))
            (define wd-ok (bit-set? (cron-expr-weekdays expr) wd))
            (define both-restricted
              (and (not (cron-expr-days-is-wildcard? expr))
                   (not (cron-expr-weekdays-is-wildcard? expr))))
            (define day-match
              (if both-restricted
                  (or day-ok wd-ok)
                  (and day-ok wd-ok)))
            (not day-match))
          (loop ny nm (add1 nd) 0 0 (add1 iter))]

         [(not (bit-set? (cron-expr-hours expr) nh)) (loop ny nm nd (add1 nh) 0 (add1 iter))]

         [(not (bit-set? (cron-expr-minutes expr) nmin)) (loop ny nm nd nh (add1 nmin) (add1 iter))]

         [else (utc-components->seconds ny nm nd nh nmin 0)])])))

;; ============================================================================
;; Format cron expression as normalized numeric string
;; ============================================================================

(define (format-bitfield bits min-val max-val)
  (define all-set?
    (for/and ([i (in-range min-val (add1 max-val))])
      (bit-set? bits i)))
  (cond
    [all-set? "*"]
    [else
     (string-join (for/list ([i (in-range min-val (add1 max-val))]
                             #:when (bit-set? bits i))
                    (number->string i))
                  ",")]))

(define (cron-expr->string expr)
  (string-join (list (format-bitfield (cron-expr-minutes expr) 0 59)
                     (format-bitfield (cron-expr-hours expr) 0 23)
                     (format-bitfield (cron-expr-days expr) 1 31)
                     (format-bitfield (cron-expr-months expr) 1 12)
                     (format-bitfield (cron-expr-weekdays expr) 0 6))
               " "))

;; ============================================================================
;; System cron job scheduler
;; ============================================================================

;; Detect the current platform
(define current-platform
  (case (system-type 'os)
    [(unix)
     (if (regexp-match? #rx"(?i:darwin)" (with-output-to-string (lambda () (system "uname -s"))))
         'macos
         'linux)]
    [(macosx) 'macos]
    [(windows) 'windows]
    [else (error 'rcron "unsupported platform: ~a" (system-type 'os))]))

(define rcron-marker "# rcron: ")

;; Validate that a job name contains only alphanumeric, hyphens, and underscores.
(define (validate-name! name)
  (unless (regexp-match? #rx"^[a-zA-Z0-9_-]+$" name)
    (error 'cron "invalid job name (must be alphanumeric, hyphens, underscores): ~a" name)))

;; Convert a command (string or list of strings) to a shell command string.
(define (command->string cmd)
  (cond
    [(string? cmd) cmd]
    [(list? cmd)
     (string-join (for/list ([arg (in-list cmd)])
                    (if (regexp-match? #rx"[ \t\"']" arg)
                        (if (eq? current-platform 'windows)
                            (string-append "\"" (string-replace arg "\"" "\"\"") "\"")
                            (string-append "'" (string-replace arg "'" "'\\''") "'"))
                        arg))
                  " ")]
    [else (error 'cron "command must be a string or list of strings")]))

;; --- Linux: crontab ---

(define (crontab-read)
  (define-values (proc out in err) (subprocess #f #f #f "/usr/bin/env" "crontab" "-l"))
  (close-output-port in)
  (define content (port->string out))
  (close-input-port out)
  (close-input-port err)
  (subprocess-wait proc)
  (if (zero? (subprocess-status proc)) content ""))

(define (crontab-write content)
  (define tmp (make-temporary-file "rcron-~a"))
  (call-with-output-file tmp #:exists 'truncate (lambda (p) (display content p)))
  (define ok? (system* (find-executable-path "crontab") (path->string tmp)))
  (delete-file tmp)
  (unless ok?
    (error 'cron "failed to install crontab")))

;; Remove a job's marker comment and its following cron line from crontab content.
(define (crontab-remove-job name content)
  (define marker (string-append rcron-marker name))
  (define lines (string-split content "\n" #:trim? #f))
  (let loop ([ls lines]
             [acc '()])
    (cond
      [(null? ls) (string-join (reverse acc) "\n")]
      [(string=? (string-trim (car ls)) marker)
       ;; Skip the marker line and the following cron line
       (loop (if (and (pair? (cdr ls)) (not (string-prefix? (cadr ls) "#")))
                 (cddr ls)
                 (cdr ls))
             acc)]
      [else (loop (cdr ls) (cons (car ls) acc))])))

(define (cron-install/linux name schedule command)
  (define expr (cron-parse schedule))
  (define marker-line (string-append rcron-marker name))
  ;; Escape % as \% in crontab lines (vixie cron treats unescaped % as newline)
  (define cmd-str (string-replace (command->string command) "%" "\\%"))
  (define cron-line (string-append (cron-expr->string expr) " " cmd-str))
  (define existing (crontab-read))
  (define cleaned (crontab-remove-job name existing))
  (define new-content (string-append (string-trim cleaned) "\n" marker-line "\n" cron-line "\n"))
  (crontab-write new-content))

(define (cron-remove/linux name)
  (define existing (crontab-read))
  (define cleaned (crontab-remove-job name existing))
  (crontab-write cleaned))

(define (cron-list/linux)
  (define content (crontab-read))
  (define lines (string-split content "\n" #:trim? #f))
  (for/list ([line (in-list lines)]
             #:when (string-prefix? (string-trim line) rcron-marker))
    (substring (string-trim line) (string-length rcron-marker))))

(define (cron-remove-all/linux)
  (define names (cron-list/linux))
  (when (pair? names)
    (define content (crontab-read))
    (define cleaned
      (for/fold ([c content]) ([name (in-list names)])
        (crontab-remove-job name c)))
    (crontab-write cleaned)))

;; --- macOS: launchd ---

(define (launchd-plist-dir)
  (build-path (find-system-path 'home-dir) "Library" "LaunchAgents"))

(define (launchd-label name)
  (string-append "com.rcron." name))

(define (launchd-plist-path name)
  (build-path (launchd-plist-dir) (string-append (launchd-label name) ".plist")))

(define (bitfield->list bits min-val max-val)
  (for/list ([i (in-range min-val (add1 max-val))]
             #:when (bit-set? bits i))
    i))

(define (generate-calendar-intervals expr)
  (define minutes (bitfield->list (cron-expr-minutes expr) 0 59))
  (define hours (bitfield->list (cron-expr-hours expr) 0 23))
  (define days (bitfield->list (cron-expr-days expr) 1 31))
  (define months (bitfield->list (cron-expr-months expr) 1 12))
  (define weekdays (bitfield->list (cron-expr-weekdays expr) 0 6))
  (define all-mins? (= (length minutes) 60))
  (define all-hrs? (= (length hours) 24))
  (define all-days? (= (length days) 31))
  (define all-months? (= (length months) 12))
  (define all-wdays? (= (length weekdays) 7))
  (apply string-append
         (for*/list ([mo (in-list (if all-months?
                                      '(#f)
                                      months))]
                     [dy (in-list (if all-days?
                                      '(#f)
                                      days))]
                     [wd (in-list (if all-wdays?
                                      '(#f)
                                      weekdays))]
                     [hr (in-list (if all-hrs?
                                      '(#f)
                                      hours))]
                     [mn (in-list (if all-mins?
                                      '(#f)
                                      minutes))])
           (string-append "    <dict>\n"
                          (if mo
                              (format "      <key>Month</key>\n      <integer>~a</integer>\n" mo)
                              "")
                          (if dy
                              (format "      <key>Day</key>\n      <integer>~a</integer>\n" dy)
                              "")
                          (if wd
                              (format "      <key>Weekday</key>\n      <integer>~a</integer>\n" wd)
                              "")
                          (if hr
                              (format "      <key>Hour</key>\n      <integer>~a</integer>\n" hr)
                              "")
                          (if mn
                              (format "      <key>Minute</key>\n      <integer>~a</integer>\n" mn)
                              "")
                          "    </dict>\n"))))

(define (xml-escape str)
  (regexp-replace* #rx"[&<>\"']"
                   str
                   (lambda (m)
                     (case (string-ref m 0)
                       [(#\&) "&amp;"]
                       [(#\<) "&lt;"]
                       [(#\>) "&gt;"]
                       [(#\") "&quot;"]
                       [(#\') "&apos;"]))))

(define (generate-plist name command expr)
  (define args
    (cond
      [(string? command) (list "/bin/sh" "-c" command)]
      [(list? command) command]
      [else (error 'cron "command must be a string or list of strings")]))
  (string-append "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                 "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" "
                 "\"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n"
                 "<plist version=\"1.0\">\n"
                 "<dict>\n"
                 "  <key>Label</key>\n"
                 "  <string>"
                 (xml-escape (launchd-label name))
                 "</string>\n"
                 "  <key>ProgramArguments</key>\n"
                 "  <array>\n"
                 (apply string-append
                        (for/list ([arg (in-list args)])
                          (string-append "    <string>" (xml-escape arg) "</string>\n")))
                 "  </array>\n"
                 "  <key>StartCalendarInterval</key>\n"
                 "  <array>\n"
                 (generate-calendar-intervals expr)
                 "  </array>\n"
                 "</dict>\n"
                 "</plist>\n"))

(define (launchd-uid)
  (string-trim (with-output-to-string (lambda () (system "id -u")))))

(define (cron-install/macos name schedule command)
  (define expr (cron-parse schedule))
  (define plist-path (launchd-plist-path name))
  ;; Unload existing if present
  (when (file-exists? plist-path)
    (system* (find-executable-path "launchctl")
             "bootout"
             (format "gui/~a/~a" (launchd-uid) (launchd-label name))))
  (define dir (launchd-plist-dir))
  (unless (directory-exists? dir)
    (make-directory* dir))
  (call-with-output-file plist-path
                         #:exists 'truncate
                         (lambda (p) (display (generate-plist name command expr) p)))
  (define ok?
    (system* (find-executable-path "launchctl")
             "bootstrap"
             (format "gui/~a" (launchd-uid))
             (path->string plist-path)))
  (unless ok?
    (error 'cron "failed to load launchd job ~a" name)))

(define (cron-remove/macos name)
  (define plist-path (launchd-plist-path name))
  (when (file-exists? plist-path)
    (system* (find-executable-path "launchctl")
             "bootout"
             (format "gui/~a/~a" (launchd-uid) (launchd-label name)))
    (delete-file plist-path)))

(define (cron-list/macos)
  (define dir (launchd-plist-dir))
  (if (directory-exists? dir)
      (for/list ([f (in-list (directory-list dir))]
                 #:when (let ([s (path->string f)])
                          (and (string-prefix? s "com.rcron.") (string-suffix? s ".plist"))))
        (define s (path->string f))
        (substring s 10 (- (string-length s) 6)))
      '()))

(define (cron-remove-all/macos)
  (for ([name (in-list (cron-list/macos))])
    (cron-remove/macos name)))

;; --- Windows: schtasks ---

(define (schtasks-task-name name)
  (string-append "rcron-" name))

(define schtasks-day-names (vector "SUN" "MON" "TUE" "WED" "THU" "FRI" "SAT"))

(define (cron-install/windows name schedule command)
  (define expr (cron-parse schedule))
  (define task-name (schtasks-task-name name))
  (define cmd-str (command->string command))
  ;; Delete existing task if present
  (system* (find-executable-path "schtasks") "/delete" "/tn" task-name "/f")
  ;; Map cron patterns to schtasks schedule types.
  ;; Only simple patterns are supported; error on anything else.
  (define minutes (bitfield->list (cron-expr-minutes expr) 0 59))
  (define hours (bitfield->list (cron-expr-hours expr) 0 23))
  (define weekdays (bitfield->list (cron-expr-weekdays expr) 0 6))
  (define all-mins? (= (length minutes) 60))
  (define all-hrs? (= (length hours) 24))
  (define all-days? (cron-expr-days-is-wildcard? expr))
  (define all-months? (= (cron-expr-months expr) all-months))
  (define all-wdays? (= (length weekdays) 7))
  (define single-min? (= (length minutes) 1))
  (define single-hr? (= (length hours) 1))
  (define single-wd? (= (length weekdays) 1))
  ;; Check for */N minute pattern: evenly-spaced minutes across 0-59
  (define minute-step
    (and all-hrs?
         all-days?
         all-months?
         all-wdays?
         (> (length minutes) 1)
         (let ([step (- (cadr minutes) (car minutes))])
           (and (= (car minutes) 0)
                (for/and ([i (in-range 1 (length minutes))])
                  (= (list-ref minutes i) (* i step)))
                step))))
  (define ok?
    (cond
      ;; */N * * * * → MINUTE with /mo N
      [minute-step
       (system* (find-executable-path "schtasks")
                "/create"
                "/tn"
                task-name
                "/tr"
                cmd-str
                "/sc"
                "MINUTE"
                "/mo"
                (number->string minute-step)
                "/f")]
      ;; * * * * * → MINUTE with /mo 1
      [(and all-mins? all-hrs? all-days? all-months? all-wdays?)
       (system* (find-executable-path "schtasks")
                "/create"
                "/tn"
                task-name
                "/tr"
                cmd-str
                "/sc"
                "MINUTE"
                "/mo"
                "1"
                "/f")]
      ;; N * * * * → HOURLY
      [(and single-min? all-hrs? all-days? all-months? all-wdays?)
       (system* (find-executable-path "schtasks")
                "/create"
                "/tn"
                task-name
                "/tr"
                cmd-str
                "/sc"
                "HOURLY"
                "/mo"
                "1"
                "/st"
                (format "00:~a" (pad2 (car minutes)))
                "/f")]
      ;; N N * * * → DAILY
      [(and single-min? single-hr? all-days? all-months? all-wdays?)
       (system* (find-executable-path "schtasks")
                "/create"
                "/tn"
                task-name
                "/tr"
                cmd-str
                "/sc"
                "DAILY"
                "/st"
                (format "~a:~a" (pad2 (car hours)) (pad2 (car minutes)))
                "/f")]
      ;; N N * * N → WEEKLY with /d DAY_NAME
      [(and single-min? single-hr? all-days? all-months? single-wd?)
       (system* (find-executable-path "schtasks")
                "/create"
                "/tn"
                task-name
                "/tr"
                cmd-str
                "/sc"
                "WEEKLY"
                "/d"
                (vector-ref schtasks-day-names (car weekdays))
                "/st"
                (format "~a:~a" (pad2 (car hours)) (pad2 (car minutes)))
                "/f")]
      [else
       (error 'cron
              "schedule ~a is too complex for Windows Task Scheduler; use a simpler expression"
              schedule)]))
  (unless ok?
    (error 'cron "failed to create scheduled task ~a" name)))

(define (pad2 n)
  (if (< n 10)
      (string-append "0" (number->string n))
      (number->string n)))

(define (cron-remove/windows name)
  (system* (find-executable-path "schtasks") "/delete" "/tn" (schtasks-task-name name) "/f")
  (void))

(define (cron-list/windows)
  (define out
    (with-output-to-string
     (lambda () (system "schtasks /query /fo LIST | findstr \"rcron-\""))))
  (define lines (string-split out "\n"))
  (for/list ([line (in-list lines)]
             #:when (string-contains? line "rcron-"))
    (define trimmed (string-trim line))
    (define prefix "TaskName:")
    (define name-part
      (if (string-prefix? trimmed prefix)
          (string-trim (substring trimmed (string-length prefix)))
          trimmed))
    (define task-prefix "\\rcron-")
    (cond
      [(string-prefix? name-part task-prefix) (substring name-part (string-length task-prefix))]
      [(string-prefix? name-part "rcron-") (substring name-part 7)]
      [else name-part])))

(define (cron-remove-all/windows)
  (for ([name (in-list (cron-list/windows))])
    (cron-remove/windows name)))

;; --- Platform dispatch ---

(define (cron name schedule command)
  (validate-name! name)
  (cron-parse schedule) ;; validate
  (case current-platform
    [(linux) (cron-install/linux name schedule command)]
    [(macos) (cron-install/macos name schedule command)]
    [(windows) (cron-install/windows name schedule command)]))

(define (cron-remove name)
  (case current-platform
    [(linux) (cron-remove/linux name)]
    [(macos) (cron-remove/macos name)]
    [(windows) (cron-remove/windows name)]))

(define (cron-jobs-list)
  (case current-platform
    [(linux) (cron-list/linux)]
    [(macos) (cron-list/macos)]
    [(windows) (cron-list/windows)]))

(define (cron-stop-all)
  (case current-platform
    [(linux) (cron-remove-all/linux)]
    [(macos) (cron-remove-all/macos)]
    [(windows) (cron-remove-all/windows)]))

;; ============================================================================
;; Provides
;; ============================================================================

(provide (struct-out cron-expr)

         (contract-out [cron-parse (-> string? cron-expr?)]
                       [cron-next (-> cron-expr? real? (or/c exact-integer? #f))]
                       [cron-expr->string (-> cron-expr? string?)])

         (contract-out [cron (-> string? string? (or/c string? (listof string?)) void?)]
                       [cron-remove (-> string? void?)]
                       [cron-jobs-list (-> (listof string?))]
                       [cron-stop-all (-> void?)]))
