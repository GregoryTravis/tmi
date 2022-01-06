(stack test) 2>&1 | tee out
exit

# (stack build tmi && stack exec tmi) 2>&1 | tee out

profargs="--library-profiling --executable-profiling --profile"
profargs=

rm -f out

# profiling: (stack build $profargs tmi && stack exec tmi $profargs -- "$@" +RTS -xc -RTS) 2>&1 | tee out
(stack build $profargs tmi && stack exec tmi $profargs -- "$@") 2>&1 | tee out
echo ====
# diff golden out
#(stack build tmi && stack exec tmi) 2>&1 | tee out
