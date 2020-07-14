A "portability" library for working with some implementations of concurrent 
hash tables, which do not have to be entirely locked in their operation. 

Note that we have not thoroughly tested this library on every implementation, 
and in many scenarios. We are, however, fairly confident that it works on SBCL
and Clozure Common Lisp.

This library was lifted from the *decentralise2* utilities, as we found that
contention between threads was limiting the throughput of the system. This new
library, however, is licensed under the BSD two-clause license, and not the
Cooperative Software License, as decentralise2 is.

![Some timelines generated from profiling decentralise2. The serial listing parser
took 8 seconds, but has a 2 second pause while it was parsing; the concurrent parser
took 9 seconds but had no pause; and the concurrent parser with concurrent hash table
took about 7.5 seconds with no pause.](Images/timelines.png)

This concurrent hash table has improved the performance of decentralise2, as it
allows node information parsing to occur concurrently with other connections'
activities. (We made these timelines using [clim.flamegraph](https://github.com/scymtym/clim.flamegraph).)

## Protocol

`(concurrent-hash-table:make-chash-table &key test hash-function segment-hash-function size &allow-other-keys)`

Create a concurrent hash table. 

`:hash-function` is a function, which is used somehow by the hash table 
implementation to hash keys into fixnums.
`:segment-hash-function` is also a function that produces fixnums from keys,
but is used by *segmenting* hash tables to pick a segment for each key, and
thus should be faster and possibly less spread out.

It is expected that `(funcall test k1 k2)` implies
`(= (funcall hash-function k1) (funcall hash-function k2))` (and similar for
the `segment-hash-function`.)

`:size` is the size of the concurrent hash table, defaulting to 1000.

`:test` is treated the same as it is with `make-hash-table`. 

--- 

`getchash`, `(setf getchash)`, `remchash` and `mapchash` work like their Common 
Lisp counterparts.

---

`(modchash key hash-table modification-function)` atomically modifies a key-value
pair in a hash table, by calling the `modification-function` with its value and
`t` if the key is present, or some bogus value and `nil` if it is not. The 
function then returns a new value and true if the key should be present, or 
some bogus value and `nil` if the key should not.

`(modify-value (key hash-table) (value present?) body ...)` is a more 
"imperative" sugaring over `modchash`.

--- 

`(update-chash function hash-table)` works in a similar way, but by calling the
function with every key and value in the table.

`(do-concurrent-table (key value hash-table) body ...)` is a more "imperative"
sugaring over `update-chash`.

## Performance

You may want to run the benchmark to check if *concurrent-hash-tables* is right 
for you:

```lisp
Testing Unsynchronised hash table, one thread:
  Finished in 0.12 seconds
Testing Boxed hash table, one thread:
  Finished in 0.18 seconds
Testing Boxed hash table, ten threads:
  Finished in 0.73 seconds
  Finished in 0.74 seconds
  Finished in 0.74 seconds
  Finished in 0.75 seconds
  ...
Testing Synchronised hash table, one thread:
  Finished in 0.25 seconds
Testing Synchronised hash table, ten threads:
  Finished in 1.13 seconds
  Finished in 1.13 seconds
  Finished in 1.13 seconds
  Finished in 1.13 seconds
  ...
Testing Concurrent hash table, one thread:
  Finished in 0.25 seconds
Testing Concurrent hash table, ten threads:
  Finished in 0.09 seconds
  Finished in 0.09 seconds
  Finished in 0.09 seconds
  Finished in 0.09 seconds
  ...
```

(I'm not sure why a concurrent hash table is apparently faster than an 
unsynchronised table, and I'm too afraid to ask.)

Note that maintaining and retrieving the hash table count may cause some 
contention on implementations that we don't yet support atomic counting on
(i.e. implementations that are not Clozure or SBCL); as we have to lock the 
hash table count to do so. This value should be locked for less time than
a non-concurrent hash table, though, so we still believe it should improve
throughput anyway.
