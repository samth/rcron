#lang scribble/manual

@(require (for-label racket/base racket/contract/base rcron))

@title{rcron: Cron Expression Parsing and Job Scheduling}
@defmodule[rcron]

The @racketmodname[rcron] library provides cron expression parsing, next-occurrence
calculation, and system-level cron job scheduling using the platform's native
scheduler (@tt{crontab} on Linux, @tt{launchd} on macOS, @tt{schtasks} on Windows).

Cron expressions use the standard 5-field format:

@verbatim{minute hour day-of-month month day-of-week}

Each field accepts:
@itemlist[
@item{@tt{*} -- all values in the field's range}
@item{A specific number (e.g., @tt{5})}
@item{A range (e.g., @tt{1-5})}
@item{A list (e.g., @tt{1,3,5})}
@item{A step (e.g., @tt{*/15} or @tt{1-30/2})}
]

Month and day-of-week fields accept English names (e.g., @tt{Mon}, @tt{January}).
Sunday can be specified as @tt{0} or @tt{7}.

When both the day-of-month and day-of-week fields are restricted (not @tt{*}),
the schedule fires when @emph{either} field matches (POSIX OR logic).

The following nicknames are also supported:
@tt|{@yearly}|, @tt|{@annually}|, @tt|{@monthly}|, @tt|{@weekly}|,
@tt|{@daily}|, @tt|{@midnight}|, @tt|{@hourly}|.

@section{Cron Expressions}

@defstruct[cron-expr ([minutes exact-nonnegative-integer?]
                      [hours exact-nonnegative-integer?]
                      [days exact-nonnegative-integer?]
                      [months exact-nonnegative-integer?]
                      [weekdays exact-nonnegative-integer?]
                      [days-is-wildcard? boolean?]
                      [weekdays-is-wildcard? boolean?])]{
Represents a parsed cron expression. Each numeric field is a bitset encoding
the active values for that field. The @racket[days-is-wildcard?] and
@racket[weekdays-is-wildcard?] fields track whether the original expression
used @tt{*}, which affects POSIX OR logic for matching.
}

@section{Parsing and Formatting}

@defproc[(cron-parse [expr string?]) cron-expr?]{
Parses a cron expression string into a @racket[cron-expr]. Accepts standard
5-field expressions and nickname strings (e.g., @racket["@hourly"]).

Raises @racket[exn:fail] if the expression is invalid.

@racketblock[
(cron-parse "*/15 * * * *")
(cron-parse "@daily")
(cron-parse "0 9 * * Mon-Fri")
]
}

@defproc[(cron-expr->string [expr cron-expr?]) string?]{
Formats a @racket[cron-expr] as a normalized 5-field string. Each field lists
its active values separated by commas, or @tt{*} if all values are active.

@racketblock[
(cron-expr->string (cron-parse "*/15 * * * *"))
]
}

@section{Next Occurrence}

@defproc[(cron-next [expr cron-expr?] [from-secs real?]) (or/c exact-integer? #f)]{
Computes the next UTC time (in seconds since the Unix epoch) at which the
cron expression matches, starting after @racket[from-secs]. Returns
@racket[#f] if no match is found within approximately 4 years.

@racketblock[
(cron-next (cron-parse "0 0 1 1 *") (current-seconds))
]
}

@section{Job Scheduler}

The scheduler registers jobs with the platform's native scheduling system:
@tt{crontab} on Linux, @tt{launchd} on macOS, and @tt{schtasks} on Windows.
Jobs persist across process restarts and are managed by the operating system.

Each job is identified by a unique name string. Registering a job with a name
that already exists replaces the previous job.

The command to run can be specified as either a string (passed to the shell)
or a list of strings (program and arguments, as with @racket[system*]).

@defproc[(cron [name string?] [schedule string?] [command (or/c string? (listof string?))]) void?]{
Registers a cron job with the system scheduler. The @racket[command] is
executed by the OS each time the @racket[schedule] fires.

@racketblock[
(cron "cleanup" "0 */6 * * *" "rm -rf /tmp/cache/*")
(cron "backup" "@daily" (list "/usr/bin/rsync" "-a" "/data/" "/backup/"))
]
}

@defproc[(cron-remove [name string?]) void?]{
Removes the cron job with the given @racket[name] from the system scheduler.
Does nothing if no job with that name exists.
}

@defproc[(cron-jobs-list) (listof string?)]{
Returns the names of all rcron-managed jobs currently registered with the
system scheduler.
}

@defproc[(cron-stop-all) void?]{
Removes all rcron-managed jobs from the system scheduler.
}
