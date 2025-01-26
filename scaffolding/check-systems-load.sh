# Check that all asdf systems load

check_system ()
{
    name="$1"
    sbcl --non-interactive --no-userinit --load "$HOME/quicklisp/setup.lisp" --eval "(setq  ql:*local-project-directories* '(\"~/projects/\"))" --eval "(ql:quickload \"$name\")"
    #sbcl --non-interactive --no-sysinit -no-userinit --load "$HOME/quicklisp/setup.lisp" --eval "(ql:quickload \"$name\")"
}

for system in "" /core /stream-based /tls /client /server
              # /test
do
    check_system http2$system
done
