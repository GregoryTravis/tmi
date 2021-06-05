# (stack build tmi && stack exec tmi) 2>&1 | tee out

profargs="--library-profiling --executable-profiling --profile"
(stack build $profargs tmi && stack exec tmi $profargs -- "$@" +RTS -xc -RTS) 2>&1 | tee out
#(stack build tmi && stack exec tmi) 2>&1 | tee out
