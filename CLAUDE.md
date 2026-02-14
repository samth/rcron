# rcron

## Package Structure

This is a multi-package Racket project:

- `rcron-lib/` -- Implementation (the `rcron` collection)
  - `main.rkt` -- All code: parser, next-occurrence calculator, scheduler
  - `info.rkt` -- Package metadata, depends on `base`
- `rcron/` -- Documentation and tests
  - `main.scrbl` -- Scribble documentation
  - `tests/cron.rkt` -- Test suite
  - `info.rkt` -- Package metadata, depends on `rcron-lib`

Both packages define `(define collection "rcron")` so they contribute to
the same collection.

## Building and Testing

```bash
raco pkg install --auto rcron-lib/ rcron/
raco test -y rcron/tests/
raco setup rcron  # rebuild docs
```

## Key Design Decisions

- Cron fields use bitset representation (integers with bit positions for active values)
- `days-is-wildcard?` and `weekdays-is-wildcard?` track whether `*` was used,
  needed for POSIX OR logic
- Scheduler uses Racket threads with `alarm-evt` for timing
- All time calculations are UTC
- `racket/base` plus `racket/math`, `racket/contract/base`, `racket/string` only
