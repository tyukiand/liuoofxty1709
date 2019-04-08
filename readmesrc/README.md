# liuoofxty

A simple programming **l**anguage that is 

  * **i**nterpreted,
  * **u**ntyped,
  * **o**bject-**o**riented,
  * **f**unctional,
  * **x**-lacking any kind of import / include / packaging or module system.

-----

<BADGES>

### Features

  * functions as first-class citizens
  * closures that can share mutable local variables
  * basic class system (members vars, methods, constructors, but no subclassing)
  * implemented as a single script under 1000 lines of code
  * Java interop via reflection (Java interfaces not implementable from within the language)

### Why?

  * Experiment with Java reflection (how hard is it to use Java classes from
    a separate scripting language?)
  * Better understand closures in OOP languages with mutable state.
    It demonstrates the phenomenon that in e.g. in Scala,

        def foo = {
          var x = 0
          (() => x, () => { x += 1 })
        }

    the local mutable variable `x` will force the compiler to allocate a
    separate object on the heap, that is then referenced by both returned
    functions. 
    So, it's not just shared mutable state, it's *shared mutable state in
    an invisible synthetic object that you've never instantiated explicitly*,
    which is kind of fun.
  * Get some feeling for object initialization.

### What does the syntax look like?

Here is an example that shows how classes can be defined and instantiated,
and also how mutually interdependent closures that share a common mutable
environment can be defined:

    <FILE:examples/example06_interdependentClosures.liuoofxty>

Using Java collections:

    <FILE:examples/example04_usingJavaClasses.liuoofxty>

More code examples can be found in `./examples/`.

### Running

The simplest way to run it on new example scripts would be by entering

    run <YOUR_SCRIPT_FILE_PATH>

in the `sbt` console.
