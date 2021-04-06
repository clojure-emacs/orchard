(ns orchard.util.io)

(defn wrap-silently
  "Middleware that executes `(f)` without printing to `System/out` or `System/err`.

  (Note that `System/out` is different from `*out*`)"
  [f]
  (fn []
    (let [old-out System/out
          old-err System/err
          ps (java.io.PrintStream. (proxy [java.io.OutputStream] []
                                     (write
                                       ([a])
                                       ([a b c])
                                       ([a b c d e]))))]
      (try
        (System/setOut ps)
        (System/setErr ps)
        (f)
        (finally
          (when (= ps System/out) ;; `System/out` may have changed in the meantime (in face of concurrency)
            (System/setOut old-out))
          (when (= ps System/err) ;; `System/err` may have changed in the meantime (in face of concurrency)
            (System/setErr old-err)))))))
