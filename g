#./t
#exit

# (stack build tmi && stack exec tmi) 2>&1 | tee out

profargs="--library-profiling --executable-profiling --profile"
#profargs=

rm -f out

# profargs='--library-profiling --executable-profiling --profile'
# (stack build --force-dirty $profargs --ghc-options="-fprof-auto -O0" rhythmr && stack exec rhythmr $profargs -- "$@" +RTS -pa -xc -RTS) 2>&1 | tee out

# with profiling:
# (stack build --force-dirty --ghc-options="-fprof-auto -O0" --library-profiling --executable-profiling --profile tmi && stack exec tmi --library-profiling --executable-profiling --profile -- +RTS -pa -xc -RTS) 2>&1 | tee out

# without profiling:
(stack build $moreprofargs $profargs tmi && stack exec tmi $profargs -- "$@") 2>&1 | tee out

echo ====
# diff golden out
#(stack build tmi && stack exec tmi) 2>&1 | tee out
