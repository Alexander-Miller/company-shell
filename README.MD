
# Company Shell

company mode completion backend for your shell functions:

![](./screenshot.png)

## Features

### Completion Candidates

`company-shell` will offer completions for 2 different sources:

* binaries that are found on your `$PATH`
* fish-shell's functions, if it is installed

`company-shell` will detect if fish-shell is installed by trying to find an executable named `fish`.
If it's found it will collect the output of the `functions` command, which will fetch both fish-shell's
builtin as well any user defined functions.

The list of completions purged from possible duplicate entries and is alphabetically sorted.

### Doc Strings

`company-shell` will first try to find documentation for candidate *c* by checking the output of `man c`.
If *c* does not have a manpage it will then try the output of `c --help` and then `c -h`. The meta doc-string
(shown in the minibuffer during completion) is provided by (the first line of) `whatis c`.

### Caching

As the process of searching though the content of your `$PATH` and building the completion lists is likely
to cause a visible delay it is executed exactly once - when `company-shell` is invoked for the first time.
The list of all possible completions will then be saved in the variable `company-shell--cache`. This
cache may be manually rebuilt by interactively executing `company-shell-create-completion-list`.

## Setup

Add `company-shell` to your `company-backends`:
`(add-to-list 'company-backends 'company-shell)`

## Dependencies

* company
* cl-lib
* dash

## Future plans & maybes

* better documentation
* Better formatting for meta strings
