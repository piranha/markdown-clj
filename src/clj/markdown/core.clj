(ns markdown.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [markdown.common :as common]
            [markdown.links :as links]
            [markdown.transformers :as trans])
  (:import [java.io BufferedReader
                    BufferedWriter
                    StringReader
                    StringWriter
                    Writer]))

(set! *warn-on-reflection* true)

(defn- write [^Writer writer ^String text]
  (doseq [c text] (.write writer (int c))))

(defn- init-transformer [writer {:keys [replacement-transformers custom-transformers inhibit-separator]}]
  (fn [line next-line state]
    (binding [common/*inhibit-separator* inhibit-separator]
      (let [[text new-state]
            (reduce
              (fn [[text, state] transformer]
                (transformer text (assoc state :next-line next-line)))
              [line state]
              (or replacement-transformers
                  (into trans/transformer-vector custom-transformers)))]
        (write writer text)
        new-state))))

(defn parse-references [lines]
  (let [references (atom {})]
    (doseq [line lines]
      (links/parse-reference-link line references))
    @references))

(defn parse-footnotes [lines]
  (let [footnotes (atom {:next-fn-id 1 :processed {} :unprocessed {}})]
    (doseq [line lines]
      (links/parse-footnote-link line footnotes))
    @footnotes))

(defn parse-metadata [lines]
  (trans/parse-metadata-headers lines))

(defn parse-params
  [params]
  (when (not= 0 (mod (count params) 2))
    (throw (IllegalArgumentException.
             "Must supply an even number of parameters")))
  (when params (apply assoc {} params)))

(defn md-to-html*
  "reads markdown content from the input stream and writes HTML to the provided
  output stream. If metadata was requested to be parsed it is returned, otherwise
  nil is returned."
  [text params]
  (binding [common/*substring* (fn [^String s n] (.substring s n))
            trans/*formatter*  clojure.core/format]
    (let [out        (StringWriter.)
          params     (parse-params params)
          lines      (line-seq (io/reader (StringReader. text)))
          references (when (:reference-links? params) (parse-references lines))
          footnotes  (when (:footnotes? params) (parse-footnotes lines))
          metadata   (when (:parse-meta? params) (parse-metadata lines))]
      (with-open [^BufferedReader rdr (io/reader (StringReader. text))
                  ^BufferedWriter wrt (io/writer out)]
        (when (and metadata (:parse-meta? params))
          (while (not= "" (string/trim (.readLine rdr)))))
        (let [transformer (init-transformer wrt params)]
          (loop [^String line      (.readLine rdr)
                 ^String next-line (.readLine rdr)
                 state             (merge {:last-line-empty? true
                                           :references       references
                                           :footnotes        footnotes}
                                     params)]
            (let [line  (if (:skip-next-line? state) "" line)
                  buf   (:buf state)
                  state (if buf
                          (transformer buf
                            (:next-line state)
                            (-> state
                                (dissoc :buf :lists :next-line)
                                (assoc :last-line-empty? true)))
                          state)]
              (if line
                (recur next-line
                       (.readLine rdr)
                       (assoc (transformer line next-line (dissoc state :skip-next-line?))
                         :last-line-empty? (empty? (.trim line))))
                (transformer (trans/footer (:footnotes state)) nil (assoc state :eof true))))))
        (.flush wrt)
        {:metadata metadata
         :html     (.toString out)}))))

(defn md-to-html [text & params]
  (:html (md-to-html* text params)))
