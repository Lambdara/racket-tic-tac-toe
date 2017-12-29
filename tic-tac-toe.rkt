#lang racket

(provide tic-tac-toe%)

(define tic-tac-toe%
  (class object%

    ;; The current player is either cross or circle
    (define current-player 'cross)
    (define other-player 'cricle)

    ;; Sets current player to other player and vice-versa
    (define (switch-players)
      (define (other player)
        (if (equal? player 'cross)
            'circle
            'cross))
      (set! current-player (other current-player))
      (set! other-player (other other-player)))

    ;; Indices denote squares on the board in the following way:
    ;; 0 1 2
    ;; 3 4 5
    ;; 6 7 8
    (define current-board (make-vector 9 'nil))

    ;; Can be nil when game is not over, or it is cross, circle or tie.
    (define current-winner 'nil)

    ;; Returns whether the move is legal
    (define/public (legal-move? index)
      (equal? 'nil (vector-ref current-board index)))

    ;; Makes move if it is legal; if it is not legal the current player loses
    (define/public (move index)
      (define value current-player)
      (if (legal-move? index)
          (begin (vector-set! current-board index value)
                 (switch-players)
                 (set! current-winner
                       (letrec ((lines
                                 '((0 1 2)
                                   (3 4 5)
                                   (6 7 8)
                                   (0 4 8)
                                   (2 4 6)
                                   (0 3 6)
                                   (1 4 7)
                                   (2 5 8)))
                                (line-make-ups
                                 (map (curry map (curry vector-ref current-board))
                                      lines))
                                (line-winners
                                 (map (lambda (line)
                                        (if (and (equal? (car line) (cadr line))
                                                 (equal? (cadr line) (caddr line)))
                                            (car line)
                                            'nil))
                                      line-make-ups))
                                (lines-won
                                 (filter (compose not (curry equal? 'nil))
                                         line-winners)))
                         (cond ((not (null? lines-won))
                                (car lines-won))
                               ((null? (filter (curry equal? 'nil)
                                               (vector->list current-board)))
                                'tie)
                               (else 'nil)))))
          (set! current-winner other-player)))

    (define/public (get-winner)
      current-winner)

    (define/public (get-board)
      (vector-copy current-board))

    (define/public (get-current-player)
      current-player)

    (define/public (get-other-player)
      other-player)

    (super-new)))
