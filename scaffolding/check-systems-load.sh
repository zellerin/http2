# Check that all asdf systems load

check_system ()
{
    name="$1"
    sbcl --non-interactive --no-userinit --load "$HOME/quicklisp/setup.lisp" --eval "(setq  ql:*local-project-directories* '(\"~/projects/\"))" --eval "(ql:quickload \"$name\")"
    #sbcl --non-interactive --no-sysinit -no-userinit --load "$HOME/quicklisp/setup.lisp" --eval "(ql:quickload \"$name\")"
}

if which sed
then
    SED=sed
fi

if which gsed
then
    SED=gsed
fi

systems=$($SED '/(defsystem/{s/.*defsystem "\([^"]*\)".*/\1/g;p};d' ./http2.asd)

for system in $systems
do
    check_system $system || exit 1
done
