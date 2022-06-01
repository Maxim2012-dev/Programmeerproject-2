#lang r6rs


(import (prefix (a-d disk config) disk:)
        (prefix (a-d disk file-system) fs:)
        (prefix (a-d file sequential output-file) out:)
        (prefix (a-d file sequential input-file) in:)
        (rnrs base)
        (rnrs control)
        (rnrs io simple)
        (a-d scheme-tools))
 
(define d (disk:new "My Computer"))
(fs:format! d)

(define f (out:new d "TestFile"))

(out:write! f 3.14)
(out:write! f 42)
(out:write! f "Done!")

(out:close-write! f)

(set! f (in:open-read! d "TestFile"))
(display (in:read f))(newline)
(display (in:read f))(newline)
(display (in:read f))(newline)

(in:close-read! f)