# Notes

This is random documentation on this library / verifier for metamath files
in Common Lisp.

## General Performance

This is designed to be high performance. We want the code to be clear,
but some decisions make a big difference to performance.  Some tips:
- Minimize system/foreign calls, e.g., don't use the slow
  READ-CHAR/PEEK-CHAR calls.  Instead, read in a big buffer at once;
  this reduces tokenization from 24sec+ to ~4sec.
- Prefer defstructure over defclass; dispatch is slower than straight call.
- Maximize open coding, e.g., provide constants, define specific types
- Prefer arrays over linked lists (like lists) where it makes sense,
  even if you have to insert in middle (if we use small elements).
  Cache misses are a killer. Some may find this surprising. See:
  http://www.codeproject.com/Articles/340797/Number-crunching-Why-you-should-never-ever-EVER-us
- Prefer eq for equality checks
- http://www.cliki.net/performance (list of tips)
- https://www.cs.utexas.edu/users/novak/lispeff.html (Gordon S. Novak Jr)
- http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.150.640
- https://common-lisp.net/project/cmucl/doc/cmu-user/compiler-hint.html
- Inlining can sometimes harm performance, because of the limited
  number of registers available.  This is especially a
  a problem on x86 32-bit, because it only has 8 registers.
  However, x86_64 and ARM have 16 registers, not 8, so we'll use inlining
  under the assumption that we have more registers.  Inlining is only a
  hint; the compiler is allowed to figure out that it shouldn't
  actually inline it.
  We're really focused on fast execution for CPUs like x86_64 and ARM.

I develop with sbcl; various runoffs suggest it's the fastest FLOSS
implementation.

## Type declarations

Explicit type declarations make it easier to detect errors, and
can also produce faster code.  Sbcl (the implementation used for development)
and its CMU ("Python") predecessor can do a lot of type inference,
but only when they're given some information to start.
[CMUCL documentation on efficiency hints](https://common-lisp.net/project/cmucl/doc/cmu-user/compiler-hint.html)
has lots of hints on types related to performance, in particular:
* Declare the types of all function arguments and structure slots as
  precisely as possible (while avoiding not, and and satisfies). Put these
  declarations in during initial coding so that type assertions can find
  bugs for you during debugging.
* Use the member type specifier where there are a small number of possible
  symbol values, for example: (member :red :blue :green).
* Use the or type specifier in situations where the type is not certain,
  but there are only a few possibilities, for example: (or list vector).
* Declare integer types with the tightest bounds that you can, such as
  (integer 3 7).
* Define deftype or defstruct types before they are used. Definition
  after use is legal (producing no "undefined type" warnings), but type
  tests and structure operations will be compiled much less efficiently.

For function declarations, use the standard Common Lisp declaim ftype,
not the nonstandard "values" declaration.

For global type information, prefer "declaim" (not "proclaim") since
it's effective at compile time (proclaim typically is not).
For local declarations, use "declare".  See:
http://stackoverflow.com/questions/14813801/proclaim-declaim-declare

## Reading quickly

We have
extra code to *quickly* read files.  This brings tokenization times down
from ~24 seconds to ~4 seconds.  This deserves an explanation.
Lisp's read-char and peek-char provide the necessary functions, but per
<http://www.gigamonkeys.com/book/files-and-file-io.html>,
 "READ-SEQUENCE is likely to be quite a bit more efficient
 than filling a sequence by repeatedly calling READ-BYTE or READ-CHAR."
 (verified in SBCL by profiling).
I'd like to use mmap(), but I've had a lot of trouble getting that to work.
The osicat library requires a version of ASDF that's not in Ubuntu 2014, and
we can't override sbcl's ASDF, so we can't use osicat for common setups.
SBCL has a nonportable interface to mmap, but it's poorly documented.
For now, just load the whole file at once.

TODO:  In the future we could use a loop over a big buffer & handle
multiple files.
For an example, see:
http://stackoverflow.com/questions/38667846/how-to-improve-the-speed-of-reading-a-large-file-in-common-lisp-line-by-line


## Common Lisp conventions

The [Common Lisp wiki](http://www.cliki.net) has lots of useful info.

We try to reuse common conventions:
- ASDF is the library standard
- Quicklisp is the usual way to download packages

See [currently-recommended libraries](http://www.cliki.net/Current%20recommended%20libraries).


Use the [Cliki naming convetions](http://cliki.net/coding%20convention) and
[Google style guide for Common Lisp](https://google.github.io/styleguide/lispguide.xml).
Exceptions:
- Indentation is 2-space.
- Lines should normally be 80 characters or less.

In Common Lisp () is identical to nil.
Still, I'll try to use null(p) to mean "is this the empty list?",
primarily so that it would be easier to port to other languages
(like Scheme, where (), nil, and #f are different).

Wimpie Nortje (Dark Chestnut) has interesting posts on how to
deploy CL applications:
https://www.darkchestnut.com/archive/

Other relevant links:

http://bimib.disco.unimib.it/people/Marco.Antoniotti/Projects/CL/HELAMBDAP/tests/asdf-uiop/docs/html/dictionary/dictionary.html
Uiop docs

http://ryepup.unwashedmeme.com/blog/2010/11/21/coroutines-in-common-lisp-with-bordeaux-threads/

https://common-lisp.net/project/iterate/doc/Problems-with-Code-Movement.html#Problems-with-Code-Movement

http://stackoverflow.com/questions/27743440/how-do-i-read-whitespace-delimited-words-from-a-stream

Qlot is a dependency management tool, probably ought to use it longer-term.
http://quickdocs.org/qlot/

~~~~
wget http://www.xach.com/lisp/buildapp.tgz
tar xvzf buildapp.tgz
cd buildapp*
make
# Install in ~/bin
make DESTDIR="$HOME" install
cd ..
~~~~

https://github.com/xach/quickproject
