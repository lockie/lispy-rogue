(proclaim '(optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
(ql-util:without-prompting (ql:update-all-dists))
(ql:quickload '(#:lispy-rogue #:deploy))

(asdf:make :lispy-rogue)
