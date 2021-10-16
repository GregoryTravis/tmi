# (stack build tmi && stack exec tmi) 2>&1 | tee out

profargs="--library-profiling --executable-profiling --profile"
rm -f out
(stack build $profargs tmi && stack exec tmi $profargs -- "$@" +RTS -xc -RTS) 2>&1 | tee out
echo ====
# diff golden out
#(stack build tmi && stack exec tmi) 2>&1 | tee out
