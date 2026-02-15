#lang racket/base

(require rcron
         rackunit)

;; ============================================================================
;; Cron expression parsing tests
;; ============================================================================

(test-case "parse simple wildcard expression"
  (define expr (cron-parse "* * * * *"))
  (check-true (cron-expr? expr))
  (check-true (cron-expr-days-is-wildcard? expr))
  (check-true (cron-expr-weekdays-is-wildcard? expr)))

(test-case "parse specific values"
  (define expr (cron-parse "30 2 15 6 1"))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 30))
  (check-false (bitwise-bit-set? (cron-expr-minutes expr) 0))
  (check-true (bitwise-bit-set? (cron-expr-hours expr) 2))
  (check-true (bitwise-bit-set? (cron-expr-days expr) 15))
  (check-true (bitwise-bit-set? (cron-expr-months expr) 6))
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 1)))

(test-case "parse step expression */15"
  (define expr (cron-parse "*/15 * * * *"))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 0))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 15))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 30))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 45))
  (check-false (bitwise-bit-set? (cron-expr-minutes expr) 5))
  (check-false (bitwise-bit-set? (cron-expr-minutes expr) 10)))

(test-case "parse range expression 1-5"
  (define expr (cron-parse "* * * * 1-5"))
  (check-false (bitwise-bit-set? (cron-expr-weekdays expr) 0))
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 1))
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 5))
  (check-false (bitwise-bit-set? (cron-expr-weekdays expr) 6)))

(test-case "parse list expression"
  (define expr (cron-parse "0,15,30,45 * * * *"))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 0))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 15))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 30))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 45))
  (check-false (bitwise-bit-set? (cron-expr-minutes expr) 10)))

(test-case "parse range with step 1-30/2"
  (define expr (cron-parse "1-30/2 * * * *"))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 1))
  (check-false (bitwise-bit-set? (cron-expr-minutes expr) 2))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 3))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 29))
  (check-false (bitwise-bit-set? (cron-expr-minutes expr) 30)))

(test-case "parse named weekdays"
  (define expr (cron-parse "30 2 * * MON"))
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 1))
  (check-false (bitwise-bit-set? (cron-expr-weekdays expr) 0)))

(test-case "parse named weekdays case-insensitive"
  (define expr (cron-parse "0 9 * * mon-fri"))
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 1))
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 5))
  (check-false (bitwise-bit-set? (cron-expr-weekdays expr) 0))
  (check-false (bitwise-bit-set? (cron-expr-weekdays expr) 6)))

(test-case "parse named months"
  (define expr (cron-parse "0 0 1 JAN *"))
  (check-true (bitwise-bit-set? (cron-expr-months expr) 1))
  (check-false (bitwise-bit-set? (cron-expr-months expr) 2)))

(test-case "parse full month names"
  (define expr (cron-parse "0 0 1 January *"))
  (check-true (bitwise-bit-set? (cron-expr-months expr) 1)))

(test-case "parse Sunday as 7"
  (define expr (cron-parse "0 0 * * 7"))
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 0)))

(test-case "weekday range 0-7 covers all days"
  (define expr (cron-parse "0 0 * * 0-7"))
  (for ([i (in-range 0 7)])
    (check-true (bitwise-bit-set? (cron-expr-weekdays expr) i) (format "bit ~a should be set" i))))

(test-case "weekday range 1-7 is Mon-Sun"
  (define expr (cron-parse "0 0 * * 1-7"))
  ;; Sunday (0) set via folding 7→0
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 0))
  (for ([i (in-range 1 7)])
    (check-true (bitwise-bit-set? (cron-expr-weekdays expr) i) (format "bit ~a should be set" i))))

(test-case "weekday range 5-7 is Fri-Sat-Sun"
  (define expr (cron-parse "0 0 * * 5-7"))
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 0)) ;; Sun via 7→0
  (check-false (bitwise-bit-set? (cron-expr-weekdays expr) 1))
  (check-false (bitwise-bit-set? (cron-expr-weekdays expr) 4))
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 5))
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 6)))

(test-case "*/1 in day field is treated as wildcard for POSIX OR"
  ;; "0 0 */1 * 1": days-is-wildcard? must be true since */1 = *
  (define expr (cron-parse "0 0 */1 * 1"))
  (check-true (cron-expr-days-is-wildcard? expr)))

(test-case "*/1 in weekday field is treated as wildcard for POSIX OR"
  (define expr (cron-parse "0 0 15 * */1"))
  (check-true (cron-expr-weekdays-is-wildcard? expr)))

;; ============================================================================
;; Nickname parsing tests
;; ============================================================================

(test-case "parse @yearly"
  (define expr (cron-parse "@yearly"))
  (check-true (cron-expr? expr))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 0))
  (check-true (bitwise-bit-set? (cron-expr-hours expr) 0))
  (check-true (bitwise-bit-set? (cron-expr-days expr) 1))
  (check-true (bitwise-bit-set? (cron-expr-months expr) 1)))

(test-case "parse @annually"
  (check-equal? (cron-parse "@annually") (cron-parse "@yearly")))

(test-case "parse @monthly"
  (define expr (cron-parse "@monthly"))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 0))
  (check-true (bitwise-bit-set? (cron-expr-hours expr) 0))
  (check-true (bitwise-bit-set? (cron-expr-days expr) 1)))

(test-case "parse @weekly"
  (define expr (cron-parse "@weekly"))
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 0)))

(test-case "parse @daily"
  (define expr (cron-parse "@daily"))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 0))
  (check-true (bitwise-bit-set? (cron-expr-hours expr) 0))
  (check-true (cron-expr-days-is-wildcard? expr)))

(test-case "parse @midnight"
  (check-equal? (cron-parse "@midnight") (cron-parse "@daily")))

(test-case "parse @hourly"
  (define expr (cron-parse "@hourly"))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 0)))

;; ============================================================================
;; Error handling tests
;; ============================================================================

(test-case "error on too many fields"
  (check-exn exn:fail? (lambda () (cron-parse "* * * * * *"))))

(test-case "error on too few fields"
  (check-exn exn:fail? (lambda () (cron-parse "* * *"))))

(test-case "error on invalid number"
  (check-exn exn:fail? (lambda () (cron-parse "60 * * * *"))))

(test-case "error on invalid range"
  (check-exn exn:fail? (lambda () (cron-parse "30-10 * * * *"))))

(test-case "error on invalid step"
  (check-exn exn:fail? (lambda () (cron-parse "*/0 * * * *"))))

(test-case "error on out-of-range weekday"
  (check-exn exn:fail? (lambda () (cron-parse "* * * * 8"))))

(test-case "error on out-of-range month"
  (check-exn exn:fail? (lambda () (cron-parse "* * * 13 *"))))

(test-case "error on invalid nickname"
  (check-exn exn:fail? (lambda () (cron-parse "@invalid"))))

;; ============================================================================
;; cron-expr->string tests
;; ============================================================================

(test-case "format all-wildcard expression"
  (check-equal? (cron-expr->string (cron-parse "* * * * *")) "* * * * *"))

(test-case "format specific values"
  (check-equal? (cron-expr->string (cron-parse "30 2 15 6 1")) "30 2 15 6 1"))

(test-case "format step expression"
  (check-equal? (cron-expr->string (cron-parse "*/15 * * * *")) "0,15,30,45 * * * *"))

;; ============================================================================
;; Next occurrence tests
;; ============================================================================

(define (utc-timestamp year month day hour minute second)
  (define (gregorian->jdn y m d)
    (define a (quotient (- 14 m) 12))
    (define y2 (+ y 4800 (- a)))
    (define m2 (+ m (* 12 a) -3))
    (+ d
       (quotient (+ (* 153 m2) 2) 5)
       (* 365 y2)
       (quotient y2 4)
       (- (quotient y2 100))
       (quotient y2 400)
       -32045))
  (define jdn (gregorian->jdn year month day))
  (define epoch-jdn (gregorian->jdn 1970 1 1))
  (+ (* (- jdn epoch-jdn) 86400) (* hour 3600) (* minute 60) second))

(define (secs->utc-list secs)
  (define d (seconds->date secs #f))
  (list (date-year d) (date-month d) (date-day d) (date-hour d) (date-minute d) (date-second d)))

(test-case "next: every 15 minutes"
  (define expr (cron-parse "*/15 * * * *"))
  (define from (utc-timestamp 2025 1 28 10 3 0))
  (define n1 (cron-next expr from))
  (check-equal? (secs->utc-list n1) '(2025 1 28 10 15 0))
  (define n2 (cron-next expr n1))
  (check-equal? (secs->utc-list n2) '(2025 1 28 10 30 0))
  (define n3 (cron-next expr n2))
  (check-equal? (secs->utc-list n3) '(2025 1 28 10 45 0))
  (define n4 (cron-next expr n3))
  (check-equal? (secs->utc-list n4) '(2025 1 28 11 0 0)))

(test-case "next: every 6 hours"
  (define expr (cron-parse "0 */6 * * *"))
  (define from (utc-timestamp 2025 1 28 10 0 0))
  (define n1 (cron-next expr from))
  (check-equal? (secs->utc-list n1) '(2025 1 28 12 0 0))
  (define n2 (cron-next expr n1))
  (check-equal? (secs->utc-list n2) '(2025 1 28 18 0 0))
  (define n3 (cron-next expr n2))
  (check-equal? (secs->utc-list n3) '(2025 1 29 0 0 0)))

(test-case "next: weekday 9 AM (MON-FRI)"
  (define expr (cron-parse "0 9 * * 1-5"))
  (define from (utc-timestamp 2025 1 28 10 0 0))
  (define n1 (cron-next expr from))
  (check-equal? (secs->utc-list n1) '(2025 1 29 9 0 0))
  (define n2 (cron-next expr n1))
  (check-equal? (secs->utc-list n2) '(2025 1 30 9 0 0))
  (define n3 (cron-next expr n2))
  (check-equal? (secs->utc-list n3) '(2025 1 31 9 0 0))
  (define n4 (cron-next expr n3))
  (check-equal? (secs->utc-list n4) '(2025 2 3 9 0 0)))

(test-case "next: specific day of week (Monday 2:30 AM)"
  (define expr (cron-parse "30 2 * * 1"))
  (define from (utc-timestamp 2025 1 28 0 0 0))
  (define n1 (cron-next expr from))
  (check-equal? (secs->utc-list n1) '(2025 2 3 2 30 0)))

(test-case "next: yearly (Jan 1 at midnight)"
  (define expr (cron-parse "@yearly"))
  (define from (utc-timestamp 2025 3 15 12 0 0))
  (define n1 (cron-next expr from))
  (check-equal? (secs->utc-list n1) '(2026 1 1 0 0 0)))

(test-case "next: monthly (1st at midnight)"
  (define expr (cron-parse "@monthly"))
  (define from (utc-timestamp 2025 1 15 12 0 0))
  (define n1 (cron-next expr from))
  (check-equal? (secs->utc-list n1) '(2025 2 1 0 0 0))
  (define n2 (cron-next expr n1))
  (check-equal? (secs->utc-list n2) '(2025 3 1 0 0 0)))

(test-case "next: daily"
  (define expr (cron-parse "@daily"))
  (define from (utc-timestamp 2025 1 28 12 0 0))
  (define n1 (cron-next expr from))
  (check-equal? (secs->utc-list n1) '(2025 1 29 0 0 0)))

(test-case "next: hourly"
  (define expr (cron-parse "@hourly"))
  (define from (utc-timestamp 2025 1 28 10 30 0))
  (define n1 (cron-next expr from))
  (check-equal? (secs->utc-list n1) '(2025 1 28 11 0 0))
  (define n2 (cron-next expr n1))
  (check-equal? (secs->utc-list n2) '(2025 1 28 12 0 0)))

(test-case "next: year boundary"
  (define expr (cron-parse "0 0 1 1 *"))
  (define from (utc-timestamp 2025 12 31 23 59 0))
  (define n1 (cron-next expr from))
  (check-equal? (secs->utc-list n1) '(2026 1 1 0 0 0)))

(test-case "next: leap year Feb 29"
  (define expr (cron-parse "0 0 29 2 *"))
  (define from (utc-timestamp 2025 1 1 0 0 0))
  (define n1 (cron-next expr from))
  (check-equal? (secs->utc-list n1) '(2028 2 29 0 0 0)))

(test-case "next: POSIX OR logic"
  (define expr (cron-parse "0 0 15 * 1"))
  (define from (utc-timestamp 2025 1 1 0 0 0))
  (define n1 (cron-next expr from))
  (check-equal? (secs->utc-list n1) '(2025 1 6 0 0 0))
  (define n2 (cron-next expr n1))
  (check-equal? (secs->utc-list n2) '(2025 1 13 0 0 0))
  (define n3 (cron-next expr n2))
  (check-equal? (secs->utc-list n3) '(2025 1 15 0 0 0))
  (define n4 (cron-next expr n3))
  (check-equal? (secs->utc-list n4) '(2025 1 20 0 0 0)))

(test-case "next: */1 day field uses POSIX OR correctly"
  ;; */1 in day field is equivalent to *, so days-is-wildcard? = #t
  ;; This means only the weekday restriction matters (AND logic with wildcard)
  (define expr (cron-parse "0 0 */1 * 1"))
  (define from (utc-timestamp 2025 1 5 12 0 0)) ;; Sunday
  (define n (cron-next expr from))
  ;; Should match next Monday, not require both day-of-month AND Monday
  (check-equal? (secs->utc-list n) '(2025 1 6 0 0 0)))

;; ============================================================================
;; System scheduler integration tests
;; ============================================================================

(require racket/file
         racket/port
         racket/string
         racket/system)

(define current-os (system-type 'os))

;; --- Platform-specific save/restore ---

;; Linux: save/restore crontab content
(define saved-crontab #f)

(define (run-crontab-read)
  (define-values (proc out in err) (subprocess #f #f #f "/usr/bin/env" "crontab" "-l"))
  (close-output-port in)
  (define content (port->string out))
  (close-input-port out)
  (close-input-port err)
  (subprocess-wait proc)
  (if (zero? (subprocess-status proc)) content ""))

(define (run-crontab-write content)
  (define tmp (make-temporary-file "rcron-restore-~a"))
  (call-with-output-file tmp #:exists 'truncate (lambda (p) (display content p)))
  (system* (find-executable-path "crontab") (path->string tmp))
  (delete-file tmp))

;; macOS: launchd plist directory
(define (launchd-plist-dir)
  (build-path (find-system-path 'home-dir) "Library" "LaunchAgents"))

(define (launchd-plist-path name)
  (build-path (launchd-plist-dir) (string-append "com.rcron." name ".plist")))

;; Windows: schtasks task name
(define (schtasks-task-exists? name)
  (define task-name (string-append "rcron-" name))
  (define out
    (with-output-to-string (lambda ()
                             (system (string-append "schtasks /query /tn " task-name " 2>nul")))))
  (string-contains? out task-name))

;; Save state before tests
(define (save-system-state)
  (case current-os
    [(unix macosx) (set! saved-crontab (run-crontab-read))]
    [else (void)]))

;; Restore state after tests
(define (restore-system-state)
  (cron-stop-all)
  (case current-os
    [(unix macosx)
     (when saved-crontab
       (run-crontab-write saved-crontab))]
    [else (void)]))

;; Run a test thunk with save/restore.
(define (with-system-cleanup thunk)
  (dynamic-wind save-system-state thunk restore-system-state))

;; --- Cross-platform tests ---

(test-case "cron install and list (string command)"
  (with-system-cleanup (lambda ()
                         (cron "test-string-cmd" "0 3 * * *" "echo hello")
                         (check-not-false (member "test-string-cmd" (cron-jobs-list))))))

(test-case "cron install with list command"
  (with-system-cleanup (lambda ()
                         (cron "test-list-cmd" "*/5 * * * *" (list "echo" "hello" "world"))
                         (check-not-false (member "test-list-cmd" (cron-jobs-list))))))

(test-case "cron-remove removes a job"
  (with-system-cleanup (lambda ()
                         (cron "test-remove" "@hourly" "echo test")
                         (check-not-false (member "test-remove" (cron-jobs-list)))
                         (cron-remove "test-remove")
                         (check-false (member "test-remove" (cron-jobs-list))))))

(test-case "cron re-register replaces existing"
  (with-system-cleanup
   (lambda ()
     (cron "test-replace" "0 1 * * *" "echo first")
     (cron "test-replace" "0 2 * * *" "echo second")
     (check-equal? (length (filter (lambda (n) (string=? n "test-replace")) (cron-jobs-list))) 1))))

(test-case "cron-stop-all clears all rcron jobs"
  (with-system-cleanup (lambda ()
                         (cron "test-stop-a" "@daily" "echo a")
                         (cron "test-stop-b" "@hourly" "echo b")
                         (check-true (>= (length (cron-jobs-list)) 2))
                         (cron-stop-all)
                         (check-equal? (cron-jobs-list) '()))))

(test-case "cron-remove is idempotent"
  (with-system-cleanup (lambda () (cron-remove "nonexistent-job"))))

(test-case "cron validates job names"
  (check-exn exn:fail? (lambda () (cron "bad name" "@daily" "echo x")))
  (check-exn exn:fail? (lambda () (cron "bad/name" "@daily" "echo x")))
  (check-exn exn:fail? (lambda () (cron "" "@daily" "echo x"))))

;; --- Platform-specific verification ---

(test-case "verify system state directly"
  (with-system-cleanup (lambda ()
                         (cron "test-verify" "@daily" "echo verify-test")
                         (case current-os
                           [(unix)
                            ;; Linux: check crontab content for marker and command
                            (define content (run-crontab-read))
                            (check-not-false (regexp-match? #rx"# rcron: test-verify" content))
                            (check-not-false (regexp-match? #rx"echo verify-test" content))]
                           [(macosx)
                            ;; macOS: check plist file exists
                            (check-true (file-exists? (launchd-plist-path "test-verify")))
                            (define plist (file->string (launchd-plist-path "test-verify")))
                            (check-not-false (regexp-match? #rx"com\\.rcron\\.test-verify" plist))
                            (check-not-false (regexp-match? #rx"echo verify-test" plist))]
                           ;; Windows: check schtasks reports the task
                           [(windows) (check-true (schtasks-task-exists? "test-verify"))])
                         ;; Remove and verify it's gone from the system
                         (cron-remove "test-verify")
                         (case current-os
                           [(unix)
                            (define after (run-crontab-read))
                            (check-false (regexp-match? #rx"# rcron: test-verify" after))]
                           [(macosx) (check-false (file-exists? (launchd-plist-path "test-verify")))]
                           [(windows) (check-false (schtasks-task-exists? "test-verify"))]))))

;; --- % escaping test (Linux only) ---

(when (eq? current-os 'unix)
  (test-case "percent signs are escaped in crontab"
    (with-system-cleanup (lambda ()
                           (cron "test-percent" "@hourly" "date +%Y-%m-%d")
                           (define content (run-crontab-read))
                           ;; The crontab line should have \% instead of bare %
                           (check-not-false (regexp-match? #rx"date \\+\\\\%Y-\\\\%m-\\\\%d"
                                                           content))))))

;; --- Windows unsupported schedule test ---

(when (eq? current-os 'windows)
  (test-case "Windows errors on unsupported complex schedule"
    (with-system-cleanup (lambda ()
                           ;; Complex schedule with multiple hours and days should error
                           (check-exn #rx"too complex"
                                      (lambda () (cron "test-complex" "0,30 1,2 * * *" "echo x")))))))

;; --- launchd semantics test (POSIX OR) ---

(test-case "launchd calendar intervals should not combine Day and Weekday in one dict"
  ;; Access unexported generate-calendar-intervals via the module namespace.
  ;; rcron-lib/main is already loaded since we (require rcron).
  (define ns (module->namespace 'rcron/main))
  (define generate-calendar-intervals (eval 'generate-calendar-intervals ns))
  (define expr (cron-parse "0 0 15 * 1"))
  (define intervals (generate-calendar-intervals expr))
  (define dicts (string-split intervals "</dict>"))
  (define (dict-has-both? d)
    (and (regexp-match? #rx"<key>Day</key>" d)
         (regexp-match? #rx"<key>Weekday</key>" d)))
  (check-false (for/or ([d (in-list dicts)]) (dict-has-both? d))
               "POSIX OR semantics require separate Day and Weekday entries"))

;; --- Execution test: verify the system scheduler actually runs the command ---

;; Check if the system scheduler daemon is running.
(define (scheduler-running?)
  (case current-os
    [(unix)
     ;; Check if cron daemon is active
     (define out
       (with-output-to-string
        (lambda ()
          (system "systemctl is-active cron 2>/dev/null || service cron status 2>/dev/null"))))
     (regexp-match? #rx"active|running" out)]
    ;; launchd is always running on macOS (it's PID 1)
    [(macosx) #t]
    [(windows)
     ;; Check if Task Scheduler service is running
     (define out (with-output-to-string (lambda () (system "sc query Schedule 2>nul"))))
     (string-contains? out "RUNNING")]
    [else #f]))

(when (scheduler-running?)
  (test-case "system scheduler executes the command"
    (with-system-cleanup (lambda ()
                           (define marker
                             (path->string (build-path (find-system-path 'temp-dir)
                                                       (format "rcron-exec-test-~a"
                                                               (current-seconds)))))
                           ;; Schedule a job that touches the marker file every minute
                           (define cmd
                             (case current-os
                               [(windows) (string-append "cmd /c echo done > \"" marker "\"")]
                               [else (string-append "touch " marker)]))
                           (cron "test-exec" "* * * * *" cmd)
                           ;; Poll for up to 120 seconds
                           (define found?
                             (let loop ([attempts 0])
                               (cond
                                 [(file-exists? marker) #t]
                                 [(>= attempts 120) #f]
                                 [else
                                  (sleep 1)
                                  (loop (add1 attempts))])))
                           (check-true found? "scheduled command was not executed within 120 seconds")
                           ;; Clean up marker file
                           (when (file-exists? marker)
                             (delete-file marker))))))

(module+ test
  (require (submod "..")))
