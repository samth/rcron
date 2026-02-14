# rcron

Cron expression parsing and system-level job scheduling for Racket.

Ported from [Bun's cron implementation](https://github.com/oven-sh/bun/pull/26999)
(cron parser and next-occurrence calculator in Zig), adapted to idiomatic
Racket. Uses the platform's native scheduler: `crontab` on Linux, `launchd`
on macOS, `schtasks` on Windows.

## Installation

```
raco pkg install rcron
```

## Usage

```racket
(require rcron)

;; Parse a cron expression
(define expr (cron-parse "*/15 * * * *"))

;; Find the next matching time (UTC seconds)
(cron-next expr (current-seconds))

;; Schedule a system cron job (string command)
(cron "cleanup" "0 */6 * * *" "rm -rf /tmp/cache/*")

;; Schedule with a list of arguments (as with system*)
(cron "backup" "@daily" (list "/usr/bin/rsync" "-a" "/data/" "/backup/"))

;; List registered jobs
(cron-jobs-list)

;; Remove a job
(cron-remove "cleanup")

;; Remove all rcron jobs
(cron-stop-all)
```

## Cron Expression Format

Standard 5-field cron expressions:

```
minute (0-59)  hour (0-23)  day-of-month (1-31)  month (1-12)  day-of-week (0-6, Sun=0)
```

Supported syntax: `*`, specific values, ranges (`1-5`), lists (`1,3,5`),
steps (`*/15`, `1-30/2`). Month and weekday names are accepted
(e.g., `Mon`, `January`). Sunday is both `0` and `7`.

When both day-of-month and day-of-week are restricted, either matching
triggers the schedule (POSIX OR logic).

Nicknames: `@yearly`, `@annually`, `@monthly`, `@weekly`, `@daily`,
`@midnight`, `@hourly`.

## Documentation

After installing, run `raco docs rcron` to view the Scribble documentation.

## License

Apache-2.0
