;;  Copyright (c) Shawn Hoover. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.

(ns clojure.contrib.test-clojure.agents
  (:use clojure.contrib.test-is))

(deftest handle-all-throwables-during-agent-actions
  ;; Bug fixed in r1198; previously hung Clojure or didn't report agent errors
  ;; after OutOfMemoryError, yet wouldn't execute new actions.
  (let [agt (agent nil)]
    (send agt (fn [state] (throw (Throwable. "just testing Throwables"))))
    (try
     ;; Let the action finish; eat the "agent has errors" error that bubbles up
     (await agt)
     (catch RuntimeException _))
    (is (instance? Throwable (first (agent-errors agt))))
    (is (= 1 (count (agent-errors agt))))

    ;; And now send an action that should work
    (clear-agent-errors agt)
    (is (= nil @agt))
    (send agt nil?)
    (await agt)
    (is (true? @agt))))


; http://clojure.org/agents

; agent
; deref, @-reader-macro, agent-errors
; send send-off clear-agent-errors
; await await-for
; set-validator get-validator
; add-watch remove-watch
; shutdown-agents

