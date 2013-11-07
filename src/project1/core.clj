(ns project1.core)

(defn expand-jobs [jobs]
  (letfn [(expand-job-helper [[history start-time] [job-name job-time]]
    (let [expanded-job
          (for [t (range job-time)]
            [job-name, (+ start-time t)]) ]
      [(concat history expanded-job), (+ start-time job-time)]))])
  (first (reduce expand-job-helper [[] 0] jobs)))

(defn round-robin-expand [jobs time-slice]
  (letfn [(round-robin-expand-helper [[history time-left-per-job start-time] [job-name job-time]]
    (if (every? #(= % 0) (vals time-left-per-job))
      (reduced history)

      (if (> (time-left-per-job job-name) 0)
        (let [time-left (time-left-per-job job-name)
              time-to-take (min time-left time-slice)
              expanded-job (for [t (range time-to-take)]
                             [job-name, (+ start-time t)])
              ]
         [(concat history expanded-job), (assoc time-left-per-job job-name (- time-left time-to-take)) (+ start-time time-to-take)])
        [history time-left-per-job start-time]
        )))]
    (reduce round-robin-expand-helper [[] (into {} jobs) 0] (cycle jobs))))

(defn average [nums]
  (/ (reduce + nums) (count nums)))

(defn avg-completion-time [jobs]
  (letfn [(compare-jobs [job-map [job-name time-slice]]
            (if-let [old-time-slice (job-map job-name)]
              (if (> time-slice old-time-slice)
                (assoc job-map job-name time-slice)
                job-map)

              (assoc job-map job-name time-slice)
              ))]
    (let [jobs-to-times-map (reduce compare-jobs {} jobs)]
      (average (vals jobs-to-times-map)))))

(defn show-job-execution [history job-times]
  (letfn [(run-job-slice [[time-left-per-job running-jobs] [job-name time-slice]]
            (let [new-running-jobs
                  (do
                     (if-not (running-jobs job-name)
                       (println "starting job " job-name))
                     (conj running-jobs job-name))]
              (do
                (println (str "time slice " time-slice " doing job " job-name))
                (do (if (= 0 (- (time-left-per-job job-name) 1))
                      (println (str "finished job " job-name)))
                  [(assoc time-left-per-job job-name (- (time-left-per-job job-name) 1)) new-running-jobs]))))]
    (reduce run-job-slice [(into {} job-times) #{}] history)))

(defn first-come-first-serve [jobs]
  (expand-jobs jobs))

(defn shortest-job-first [jobs]
  (expand-jobs (sort-by #(% 1) jobs)))

(defn round-robin [jobs time-slice]
  (round-robin-expand jobs time-slice))

(def file-names ["testdata3.txt" "testdata2.txt" "testdata1.txt"])
(doall (for [file-name file-names]
  (let [input (clojure.string/split (slurp "testdata3.txt") #"\n")
        job-pairs (map vector (take-nth 2 input) (map read-string (take-nth 2 (rest input))))]
    (do
      (println (str "Testcase: " file-name))
      (show-job-execution (first-come-first-serve job-pairs) job-pairs)
      (println "Average completion time for FCFS")
      (println (float (avg-completion-time (first-come-first-serve job-pairs))))
      (println "Average completion time for SJF")
      (println (float (avg-completion-time (shortest-job-first job-pairs))))
      (show-job-execution (round-robin-expand job-pairs 2) job-pairs)
      (println "Average completion time for RR with ts 2")
      (println (float (avg-completion-time (round-robin-expand job-pairs 2))))
      (show-job-execution (round-robin-expand job-pairs 3) job-pairs)
      (println "Average completion time for RR with ts 3")
      (println (float (avg-completion-time (round-robin-expand job-pairs 3))))))))
