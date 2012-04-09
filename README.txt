qlmapper is an SBCL-only library for loading some code in a fresh SBCL
environment for each system or release in Quicklisp.

It is available from https://github.com/xach/qlmapper/

MAP-SYSTEMS takes a file name, and for each system available via
Quicklisp, then:

  - ensures that the system is installed

  - starts a fresh SBCL instance

    - uses --non-interactive --no-userinit --no-sysinit

  - loads Quicklisp in the new instance

  - binds CL-USER:*QLMAPPER-OBJECT-NAME* to the name of the system

  - binds CL:*DEFAULT-PATHNAME-DEFAULTS* to the project directory for
    the system in question

  - loads the file given as the first argument via --load

The intent is to make it easy to write loadable Lisp files that
analyze each system in some way. For example, here's a file that will
gather all available ASDF system descriptions into a file:

    ;; ~/gather-descriptions.lisp
    (with-open-file (s "~/descriptions.sexp" :direction :output
		       :if-does-not-exist :create
		       :if-exists :append)
      (let ((system (asdf:find-system cl-user::*qlmapper-object-name*)))
	(print (list (asdf:component-name system)
		     (asdf:system-description system))
	       s)))

Then the following will load the file for each system:

    (qlmapper:map-systems "~/gather-descriptions.lisp")


MAP-RELEASES is like MAP-SYSTEMS except CL-USER:*QLMAPPER-OBJECT-NAME*
is bound to the name of each release rather than each system.

MAP-LOADED-SYSTEMS is like MAP-SYSTEMS but the system in question is
loaded with ql:quickloaded before loading the mapped file.

This could be used to determine which projects have license files,
documentation subdirectories, readme files, etc.

For any questions or comments, please email me or open an issue on github.

Zach Beane
<xach@xach.com>
