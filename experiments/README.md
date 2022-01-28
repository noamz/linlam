# Notes on compiling experiments

* The Haskell files in this directory are experiments based on the [LinLam](https://github.com/noamz/linlam) library.
  They assume you have already installed LinLam, e.g., by running `cabal install --lib` from the root of the cloned LinLam repository.

* If you have a computationally intensive experiment it is a good idea to compile your code with `ghc`, rather than interpreting it interactively with `ghci`, since it will run much faster. 
  You should also set the `-O2` flag to turn on optimizations.
  For example:
  ```console
  $ ghc -O2 ClosedSubterms
  ```

# Notes on running experiments

GHC has some facilities for parallelization but not much is done automatically by the compiler, as far as I know.
On the other hand, the [GNU Parallel](https://www.gnu.org/software/parallel/) shell tool makes it easy to run multiple processes in parallel, which can be useful if you have a multicore processor.
Here's an example of using `parallel` to run 8 instances of the `ClosedSubterms` program, each of which generates 500 linear terms of size 3*100+2 and outputs a histogram.
```console
$ (for i in {1..8} ; do echo ; done) | parallel ./ClosedSubterms 100 500 > data
```

And the resulting output:
```console
$ cat data
[(0,174),(1,198),(2,93),(3,28),(4,6),(5,1)]
[(0,165),(1,189),(2,110),(3,31),(4,5)]
[(0,172),(1,196),(2,90),(3,32),(4,9),(6,1)]
[(0,200),(1,165),(2,87),(3,40),(4,6),(5,2)]
[(0,162),(1,199),(2,93),(3,32),(4,11),(5,3)]
[(0,170),(1,198),(2,88),(3,35),(4,6),(5,3)]
[(0,194),(1,171),(2,95),(3,31),(4,6),(5,2),(6,1)]
[(0,174),(1,190),(2,90),(3,35),(4,8),(5,3)]
```

The resulting samples can be processed using the simple [topy.sh](topy.sh) script to produce something suitable as input to Python:
```console
$ ./topy.sh data
data = [] \
  + [(0,174),(1,198),(2,93),(3,28),(4,6),(5,1)] \
  + [(0,165),(1,189),(2,110),(3,31),(4,5)] \
  + [(0,172),(1,196),(2,90),(3,32),(4,9),(6,1)] \
  + [(0,200),(1,165),(2,87),(3,40),(4,6),(5,2)] \
  + [(0,162),(1,199),(2,93),(3,32),(4,11),(5,3)] \
  + [(0,170),(1,198),(2,88),(3,35),(4,6),(5,3)] \
  + [(0,194),(1,171),(2,95),(3,31),(4,6),(5,2),(6,1)] \
  + [(0,174),(1,190),(2,90),(3,35),(4,8),(5,3)] \
  + []
```
