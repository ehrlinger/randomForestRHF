# randomForestRHF 1.0.2

* Maintenance update with bug fixes and CRAN-related cleanups.
* Added explicit POSIX socket/select header includes in `src/server.h` for improved musl/Alpine compatibility.
* Replaced unsuppressible console output in `print.rhf()`, `print.auct.rhf()`, and RHF importance helpers with suppressible message-based output.
* Hardened prediction and native-interface handling, including more robust checks for variable-level metadata passed from R to compiled code.
* Updated native routine registration to match the current C entry-point signatures and applied minor build/source cleanups.
  
# randomForestRHF 1.0.0

* Initial release.
