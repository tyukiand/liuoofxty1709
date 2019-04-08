# liuoofxty

A simple programming **l**anguage that is 

  * **i**nterpreted,
  * **u**ntyped,
  * **o**bject-**o**riented,
  * **f**unctional,
  * **x**-lacking any kind of import / include / packaging or module system.

-----

 ![version: 0.1.0](https://img.shields.io/static/v1.svg?label=version&message=0.1.0&color=lightgrey) ![local-build: passing](https://img.shields.io/static/v1.svg?label=local-build&message=passing&color=green) ![local-tests: passing](https://img.shields.io/static/v1.svg?label=local-tests&message=passing&color=green) ![statement-coverage: 78.66%](https://img.shields.io/static/v1.svg?label=statement-coverage&message=78.66%&color=yellow)  ![branch-coverage: 53.12%](https://img.shields.io/static/v1.svg?label=branch-coverage&message=53.12%&color=yellow)

  *Tested locally on x86_64 GNU/Linux
 with `scalaVersion = 2.12.7`, `sbtVersion = 1.1.4`. Readme generated on 2019-04-08.* 

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

    
    class Pair {
      var fst;
      var snd;
      this(x, y) {
        this.fst = x;
        this.snd = y;
      }
    }
    
    def makePairOfClosures(startInt) {
      var x = startInt;
      def incrementer() {
        x = x + 1;
      }
      def getter() {
        x;
      }
      var result = new Pair(incrementer, getter);
      result;
    }
    
    var p1 = makePairOfClosures(100);
    var p2 = makePairOfClosures(10000);
    var i1 = p1.fst;
    var g1 = p1.snd;
    var i2 = p2.fst;
    var g2 = p2.snd;
    
    i1(); i1();
    println(g1());
    i1();
    println(g1());
    
    i2();
    println(g2());
    i1(); i2();
    println(g1());
    println(g2());

Using Java collections:

    
    using java.util.ArrayList;
    
    var x = new ArrayList(10);
    x.add(2 + 25);
    x.add(3);
    x.add(5);
    println(x.size() + 1000);
    
    using java.util.HashMap;
    
    var h = new HashMap();
    h.put(24, 42);
    h.put(85, 58);
    println(h.get(20 + 4) + h.get(17 * 5));

More code examples can be found in `./examples/`.

### Running

The simplest way to run it on new example scripts would be by entering

    run <YOUR_SCRIPT_FILE_PATH>

in the `sbt` console.
