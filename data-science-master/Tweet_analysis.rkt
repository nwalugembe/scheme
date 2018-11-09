#lang racket
(require data-science-master)
(require json)
(require plot)
(require math)
(require texpict/mrpict)
;;; This function reads line-oriented JSON (as output by massmine),
;;; and packages it into an array. For very large data sets, loading
;;; everything into memory like this is heavy handed. For data this small,
;;; working in memory is simpler
(define (json-lines->json-array #:head [head #f])
  (let loop ([num 0]
             [json-array '()]
             [record (read-json (current-input-port))])
    (if (or (eof-object? record)
            (and head (>= num head)))
        (jsexpr->string json-array)
        (loop (add1 num) (cons record json-array)
              (read-json (current-input-port))))))
;;; Read in the entire tweet database (3200 tweets from uganda's timeline)
(define tweets (string->jsexpr
                (with-input-from-file "UGTweetsRealTime.json" (λ () (json-lines->json-array)))))
;;; Remove just the tweet text and source from each tweet
;;; hash. Finally, remove retweets.
;;; Remove just the tweet text, source,place, and timestamp from each tweet
;;; hash. Finally, remove retweets.
(define t
  (let ([tmp (map (λ (x) (list (hash-ref x 'text) (hash-ref x 'source)
                               (hash-ref x 'created_at)(hash-ref x 'place))) tweets)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)))
;; Check the country origin of the tweets 
(define tweet-by-country
  (map (λ (x) (list (first x)
                    (if (string=? (fourth x) "Uganda")
                        "Uganda"
                        (display "Origin of tweets must be Uganda"))))
       t))
;;; Label tweets coming from phones computers and others
(define tweet-by-type
  (map (λ (x) (list
                        (cond [(string-contains? (second x) "mobile-devices") "mobile-devices"]
                              [(string-contains? (second x) "computer") "computer"]
                              [else "other"])))
         tweet-by-country))
(define mobile-devices (filter (λ (x) (string=? (second x) "mobile-devices")) tweet-by-type))
(define computer (filter (λ (x) (string=? (second x) "computer")) tweet-by-type))
;;; Normalize case, remove URLs, remove punctuation, and remove spaces
;;; from each tweet.
(define (preprocess-text str)
  (string-normalize-spaces
   (remove-punctuation
    (remove-urls
     (string-downcase str)) #:websafe? #t)))
;;; From here foward, we remove quoted tweets to focus exclusively on
;;; content unique to the Uganda twitter feed
(define mob (map (λ (x)
                 (remove-stopwords x))
               (map (λ (y)
                      (string-split (preprocess-text y)))
                    ($ mobile-devices 0))))
(define comp (map (λ (x)
                 (remove-stopwords x))
               (map (λ (y)
                      (string-split (preprocess-text y)))
                    ($ computer 0))))
;;; Remove empty strings and flatten tweets into a single list of
;;; words
(define a (filter (λ (x) (not (equal? x ""))) (flatten mob)))
(define i (filter (λ (x) (not (equal? x ""))) (flatten comp)))
;;; All words from both sources of tweets
(define b (append a i))
;;; Only words that are used by both devices
(define c (set-intersect a i))
;; ;;; Word list from mobile-devices and computer tweets
(define awords (sort (sorted-counts a)
                     (λ (x y) (> (second x) (second y)))))
(define iwords (sort (sorted-counts i)
                     (λ (x y) (> (second x) (second y)))))
(define bwords (sort (sorted-counts b)
                     (λ (x y) (> (second x) (second y)))))
;;; Now we calculate the log odds ratio of words showing up in tweets
;;; across both platforms
(define (get-word-freq w lst)
  (let ([word-freq (filter (λ (x) (equal? (first x) w)) lst)])
    (if (null? word-freq) 0 (second (first word-freq)))))

;;; Sentiment from words coming from either device
(define bsentiment (filter (λ (x) (second x)) (list->sentiment bwords #:lexicon
'nrc)))
;;; We calculate the log odds for each affective label from the
;;; sentiment analysis
(define (find-sentiment-log-odds str)
(map (λ (x)
'(,x
,(log-base (/
(/ (add1 (get-word-freq x awords)) (add1 (length a)))
(/ (add1 (get-word-freq x iwords)) (add1 (length i))))
#:base 2)))
     ($ (subset bsentiment 1 (λ (x) (string=? x str))) 0)))
;;; Apply the above helper to each affective label
(define sadness-lo (find-sentiment-log-odds "sadness"))
(define fear-lo (find-sentiment-log-odds "fear"))
(define anger-lo (find-sentiment-log-odds "anger"))
(define disgust-lo (find-sentiment-log-odds "disgust"))
(define surprise-lo (find-sentiment-log-odds "surprise"))
(define anticipation-lo (find-sentiment-log-odds "anticipation"))
(define trust-lo (find-sentiment-log-odds "trust"))
(define joy-lo (find-sentiment-log-odds "joy"))
;;; Helper plotting function
(define (plot-sentiment lst)
  (let* ([n (min 10 (length lst))]
         [top-words (take (sort lst (λ (x y) (> (abs (second x)) (abs (second
                                                                       y))))) n)]
         [mobile-devices-words (filter (λ (x) (positive? (second x))) top-words)]
         [computer-words (filter (λ (x) (negative? (second x))) top-words)])
    (parameterize ([plot-width 300]
                   [plot-height 400]
                   [plot-x-tick-label-anchor 'right]
                   [plot-x-tick-label-angle 90])
      (plot-pict (list
                  (discrete-histogram mobile-devices-words
                                      #:y-min -4
                                      #:y-max 4
                                      #:color "OrangeRed"
                                      #:line-color "OrangeRed")
                  (discrete-histogram (reverse computer-words)
                                      #:x-min (length mobile-devices-words)
                                      #:y-min -4
                                      #:y-max 4
                                      #:color "LightSeaGreen"
                                      #:line-color "LightSeaGreen"))
                 #:x-label ""
                 #:y-label ""))))
;;; Plot everything together
(vl-append
 (hc-append (ct-superimpose (plot-sentiment sadness-lo) (text "sadness" null 20))
            (ct-superimpose (plot-sentiment fear-lo) (text "fear" null 20))
            (ct-superimpose (plot-sentiment anger-lo) (text "anger" null 20))
            (ct-superimpose (plot-sentiment disgust-lo) (text "digust" null 20)))
 (hc-append (ct-superimpose (plot-sentiment surprise-lo) (text "surprise" null 20))
            (ct-superimpose (plot-sentiment anticipation-lo) (text "anticipation"
                                                                   null 20))
            (ct-superimpose (plot-sentiment trust-lo) (text "trust" null 20))
            (ct-superimpose (plot-sentiment joy-lo) (text "joy" null 20))))
