;; STUDENT NAME: KINYONYI DAVID HOPE
;; REG. NO. : 2024/HD05/22098U
;; STUDENT NUMBER: 2400722098
;; COURSE NAME: STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS
;; COURSE CODE: MCN 7105
;; ASSIGNMENT: END OF SEMESTER PROJECT
;; PROGRAM: MDSE


#lang racket

(require net/url)
(require data-science-master)
(require plot)
(require math)

; Function to load the dataset from a CSV file
(define (load-data csv-path)
  (read-csv csv-path #:->number? #f #:header? #t))


; Function to preprocess tweet text (remove punctuation, lowercase, etc.)
(define (preprocess-text raw-text)
  (string-normalize-spaces
   (remove-punctuation
    (string-downcase raw-text) #:websafe? #t)))


; Function to tokenize and process a list of tweets
(define (tokenize-tweets tweets)
  (document->tokens (string-join tweets "")))


; Function for sentiment analysis using a specified lexicon
(define (analyze-sentiment tokens lexicon-type)
  (list->sentiment tokens #:lexicon lexicon-type))


; Function to aggregate sentiment frequencies
(define (aggregate-sentiment sentiment)
  (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq)))


; Function to plot the sentiment distribution for NRC lexicon
(define (plot-nrc-sentiment sentiment-counts)
  (parameterize ((plot-width 800)) ; Setting plot width
    (plot (list
           (tick-grid)
           (discrete-histogram
            (sort sentiment-counts (λ (x y) (> (second x) (second y)))) ; Sort descending
            #:color "Purple"
            #:line-color "Orange"))
          #:x-label "Affective Label"  
          #:y-label "Frequency")))


; Function to plot the sentiment distribution for Bing lexicon (inverted)
(define (plot-bing-sentiment sentiment-counts)
  (parameterize ([plot-height 200])
    (plot (discrete-histogram
           sentiment-counts
           #:y-min 0
           #:y-max 2500
           #:invert? #t  ; Invert the graph for Bing lexicon
           #:color "Green"
           #:line-color "MediumOrchid")
          #:x-label "Frequency"
          #:y-label "Sentiment Polarity")))


; Function to load, preprocess, tokenize, and perform sentiment analysis on tweet data
(define (perform-sentiment-analysis csv-path lexicon-type)
  (define data (load-data csv-path))  ; Load the CSV data
  (define tweets (map (λ (row) (list-ref row 9)) data))  ; Extract tweet text from column 9
  (define processed-tweets (map preprocess-text tweets))  ; Preprocess each tweet
  (define tokens (tokenize-tweets processed-tweets))  ; Tokenize the processed tweets
  (define sentiment (analyze-sentiment tokens lexicon-type))  ; Analyze sentiment using the chosen lexicon
  (define sentiment-counts (aggregate-sentiment sentiment))  ; Aggregate sentiment counts

   ; Display the sentiment counts used for plotting
  (display "Sentiment Counts: ")
  (for-each (λ (pair) (displayln pair)) sentiment-counts)  ; Print each sentiment and frequency pair

  ; Use of cond to choose the correct graph type based on the lexicon selected
  (cond
    [(eq? lexicon-type 'nrc) (plot-nrc-sentiment sentiment-counts)]  ; Plot NRC graph
    [(eq? lexicon-type 'bing) (plot-bing-sentiment sentiment-counts)] ; Plot Bing graph
    [else (error "Unknown lexicon type" lexicon-type)]))            ; Handle other cases

; Perform sentiment analysis with NRC lexicon and plot the results
(perform-sentiment-analysis "./uganda_tweets.csv" 'nrc)

; Perform sentiment analysis with Bing lexicon and plot the results
(perform-sentiment-analysis "./uganda_tweets.csv" 'bing)
