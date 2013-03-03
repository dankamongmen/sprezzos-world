;;;; Find out the FASL version of this Clisp release and dump it to the
;;;; debian/clisp.substvars variable, so it can be used in
;;;; debian/control
;;;;
;;;; Packages that want to provide binary Clisp FASLs can then depend
;;;; on clisp-fasl-loader-XX
;;;;
;;;;  -- René van Bevern <rvb@pro-linux.de>, Sat Sep  3 19:23:20 2005

(with-open-file (substvars "debian/clisp.substvars"
			   :direction :output
			   :if-exists :append
			   :if-does-not-exist :create)
  (format substvars "~&clisp:fasl-version=clisp-fasl-loader-~A~%"
	  (car (system::version))))

(ext:quit)

