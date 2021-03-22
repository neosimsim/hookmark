module Debug.Trace.Phony
  ( trace,
  )
where

trace :: String -> IO a -> IO a
trace _ = id
