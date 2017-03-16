(ql:quickload "vecto")
(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")
(ql:quickload "lispbuilder-sdl-vecto")
(ql:quickload "lispbuilder-sdl-ttf")
(ql:quickload "trivial-shell")
(ql:quickload "split-sequence")

(in-package #:lispbuilder-sdl)

(defmethod _render-string-blended_ ((string string) (font ttf-font) (color color) free cache)
  (let ((surf nil))
    (with-foreign-color-copy (col-struct color)
      (setf surf (make-instance 'surface :fp (sdl-ttf-cffi::render-utf8-blended (fp font) string
                                                                                (if (cffi:foreign-symbol-pointer "TTF_glue_RenderUTF8_Blended")
                                                                                  col-struct
                                                                                  (+ (ash (b color) 16)
                                                                                     (ash (g color) 8)
                                                                                     (r color)))))))
    (when cache
      (setf (cached-surface font) surf))
    surf))

(in-package #:cl-user)
(defpackage #:dock (:use #:cl) (:export #:main))

(in-package #:dock)

(defparameter wwidth 200)
(defparameter wheight 300)

(defstruct view
  (kind :button)
  (x 0 :type integer)
  (y 0 :type integer)
  (w 0 :type integer)
  (h 0 :type integer)
  (label "view")
  (icon "")
  (func nil)
  )

(defvar color-white (sdl:color :r 0 :g 0 :b 0 :a 255))
(defvar color-black (sdl:color :r 255 :g 255 :b 255 :a 255))
(defvar font-times nil)

(defvar active-view nil)
(defvar active-layout :main)

(defun launch (cmd)
  (print cmd)
  (sb-ext:run-program (caddr cmd) (cdddr cmd) :search t :wait nil))

(defvar main-views nil)
(defvar launchers nil)

(defvar default-font nil)
(defvar default-font-def (make-instance 'sdl:ttf-font-definition
                                                    :size 24
                                                    :filename "InputMono-Regular.ttf"))

(defvar icon-font nil)
(defvar icon-font-def (make-instance 'sdl:ttf-font-definition
                                                    :size 32
                                                    :filename "unifont.ttf"))


(defun make-main-views ()
  (let ((i 0)) 
    (mapcar #'(lambda (launcher)
                (setf i (+ i 1))
                (make-view :kind :button
                           :x 10 :y (+ 10 (* 36 i))
                           :w (- wwidth 20) :h 36
                           :label (cadr launcher)
                           :icon (car launcher)
                           :func `(launch (list ,@launcher))
                           ))
            launchers))
  )

(defun render-views (views)
  (mapcar #'(lambda (v) (let ((active (eq active-view v)))
                          
                          (sdl:with-color (col (if active color-black color-white))
                            (sdl:draw-box (sdl:rectangle :x (view-x v) :y (view-y v) :w (view-w v) :h (view-h v))))

                          (case (view-kind v)
                            (:button 
                             (sdl:with-font (icon-font icon-font-def) (sdl:draw-string-blended-* (view-icon v) (view-x v) (view-y v) :color (if active color-white color-black)))
                             (sdl:with-font (default-font default-font-def) (sdl:draw-string-blended-* (view-label v) (+ 24 (view-x v)) (+ 2 (view-y v)) :color (if active color-white color-black))))
                            (:text
                             (let ((yo (view-y v)))
                               (mapcar #'(lambda (line)
                                           (sdl:draw-string-blended line
                                                                  (sdl:point :x (view-x v) :y yo)
                                                                  :color (if active color-white color-black))
                                           (setf yo (+ yo 24)))
                                       (view-label v))) )
                            )))
          views)
  )

(defun view-at-point (views px py)
  (first (remove nil (mapcar #'(lambda (v) (let* ((x (view-x v)) (y (view-y v)) (x2 (+ x (view-w v))) (y2 (+ y (view-h v))))
                          (if (and (>= px x) (>= py y) (<= px x2) (<= py y2)) v nil)))
          views)))
  )

(defun active-layout-views ()
  main-views
  )

(defun render-active-layout ()
  (sdl:with-color (col color-white)
    (sdl:draw-box (sdl:rectangle :x 0 :y 0 :w wwidth :h wheight)))
  
  (render-views (active-layout-views))
  )

(defun load-views ()
  (print "load-views")
  (load "launchers.lisp")
  (setq main-views (make-main-views))
  (render-active-layout)
)

(defun main () 
(sdl:with-init ()
  (let ()
    (sdl:window wwidth wheight)
    (sdl:clear-display sdl:*white*)
    (sdl:initialise-default-font default-font-def)
    (setf default-font (sdl:initialise-font default-font-def))
    (setf icon-font (sdl:initialise-font icon-font-def))

    (load-views)
    
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display))

      (:key-down-event ()
                       (when (sdl:key-pressed-p :sdl-key-r) (load-views)
                             ))

      (:mouse-button-down-event (:x x :y y)

                                (print (list "mouse-down" x y))
                                (let ((clicked-view (view-at-point (active-layout-views) x y)))
                                  (if (and clicked-view active-view (eq active-view clicked-view))
                                      (eval (view-func clicked-view))
                                      (setq active-view clicked-view)))
                                (print active-view)
                                (render-active-layout) )

      (:idle () (sdl:update-display))
      
      ))))

; (main)
