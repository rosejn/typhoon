(ns typhoon.core
  (:use overtone.live
        [overtone.util.lib :only (defunk)]
        [overtone.sc.machinery.ugen.fn-gen :only (with-overloaded-ugens)]
        overtone.inst.drum
        [clojure repl pprint]))

; Synth Design
; * two wave oscillators
;  -- saw, tri, saw-tri, pulse (adjustable)
; * two sample based oscillators
; * 4 pole lowpass filter
; * 2 pole highpass filter
; * VCA with feedback
; * 5 envelopes
; * two LFOs
; * 8 modulation paths

; Questions:
; * how to do oscillator synchronization, so we can have the
; option of resetting osc1 when osc2 resets (Sync 2->1 setting)

; TODO:
; * glide mode
; * key follow
; -- osc 1/2 have a fixed base-freq, but they can optionally switch to using an input freq instead of their base-freq
; * wave reset
; * 2-pole and 4-pole switching for lowpass filter
; * audio mod control of lowpass-filter
; * per-oscillator (1 & 2) oscillator slop

(defunk dapdsr
  "Create a delay, attack, peak-hold, decay, sustain, release envelope suitable for use with
  the env-gen ugen"
  [delay-t 0.0
   attack 0.01 peak-hold 0 decay 0.3 sustain 0.5 release 1
   level 1 curve -4 bias 0]
  (with-overloaded-ugens
    (envelope
      (map #(+ %1 bias) [0 0 level level (* level sustain) 0])
      [delay-t attack peak-hold decay release] curve)))

(def WAVETABLE-RATE 44100)
(def DEFAULT-NOTE (note :c4)) ; middle C

(def silence (buffer 1))
(def bass-drum (load-sample "samples/909 01 - Bass Drum/909ft_t0_a100_d75.wav"))
(def snare-drum (load-sample "samples/909 02 - Snare Drum/909sn_t50_t50_s100.wav"))
(def one-clap (load-sample "samples/909 06 - Percussion/909hc.wav"))
(def hi-hat (load-sample "samples/909 07 - Hi Hat/909chh_d75.wav"))


(defsynth typhoon [note DEFAULT-NOTE velocity 127

                   ;; "analog" oscillator settings
                   ;osc-slop 0.0

                   ; fine controls use cents with typical range
                   ; [-50 to 50] (50 cents = 1/2 semitone)
                   osc1-base-pitch DEFAULT-NOTE osc1-fine 0

                   ; 0 => use base-pitch, 1 => use note
                   osc1-key-follow 1
                   osc1-wave 1 osc1-duty 0.5 osc1-glide 0.0

                   osc2-key-follow 1
                   osc2-base-pitch DEFAULT-NOTE osc2-fine 0.0
                   osc2-wave 2 osc2-duty 0.5 osc2-glide 0.0

                   sub-osc-level 0.0
                   mix-1-2 0.5 ; 0 = osc1, 1 = osc2, 0.5 = even mix

                   ;; wavetable oscillator settings
                   osc3-pitch-offset 0.0 osc3-fine-offset 0.0
                   osc3-buf (:id silence) osc3-glide 0.0 osc3-level 0.5
                   osc3-trig 0 osc3-start 0.0 osc3-loop 0.0

                   osc4-pitch-offset 0.0 osc4-fine-offset 0.0
                   osc4-buf (:id silence) osc4-glide 0.0 osc4-level 0.5
                   osc4-trig 0 osc4-start 0.0 osc4-loop 0.0

                   ; pre and post filter levels for osc's 3 and 4
                   osc34-pre-filter-level 1.0 osc34-post-filter-level 0.0

                   ;; feedback settings
                   feedback-level 0.0 feedback-delay 0.0

                   ;; filter settings
                   ; key-track [0.0 - 1.0]
                   ; * 0.0 => no tracking
                   ; * 0.5 => filter cutoff increases a semitone per midi note
                   ; * 1.0 => filter cutoff increases one tone per midi note
                   lpf-cutoff 20000.0 lpf-resonance 1.0 lpf-key-track 0.5
                   lpf-env-amount 0.5 lpf-env-vel-amount 0.3 lpf-env-velocity-amount 1.0
                   lpf-env-curve -4
                   lpf-delay 0.0 lpf-attack 0.001 lpf-peak-hold 0.0
                   lpf-decay 0.001 lpf-sustain 0.4 lpf-release 0.05

                   hpf-cutoff 0.0 hpf-key-track 0.0

                   ;; vca settings
                   amp-env-amount 1.0
                   amp-delay 0.0 amp-attack 0.001 amp-peak-hold 0.0
                   amp-decay 0.05 amp-sustain 0.5 amp-release 0.01
                   amp-env-curve -4

                   vol 0.99 pan 0.5]

  (let [
        ; convert velocity from midi range to [0-1]
        velocity   (/ velocity 127.0)

        ; first draft implementation of subtle oscillator slop
        ;freq (mul-add:kr (dust2 (* osc-slop 10)) (* freq 0.01) freq)

        ;;;; "analog" oscillators

        ;; osc 1
        ; key follow
        osc1-pitch (select:kr osc1-key-follow [osc1-base-pitch note])

        ; shift by osc1-fine cents
        osc1-freq  (* (midicps osc1-pitch) (pow 2 (/ osc1-fine 1200.0)))

        ; portamento
        osc1-freq  (lag:kr osc1-freq osc1-glide)

        ; oscillator shape options (all band-limited)
        osc1-sin   (sin-osc osc1-freq)
        osc1-saw   (saw osc1-freq)

        ; need to amp up since the one-pole severely lowers the amplitude
        osc1-tri   (* 7 (one-pole (pulse osc1-freq) 0.99))
        osc1-pulse (pulse osc1-freq osc1-duty)
        osc1       (select osc1-wave [osc1-sin osc1-saw osc1-tri osc1-pulse])

        ;; sub-oscillator
        ; square wave 1 octave below osc1
        sub-osc    (* sub-osc-level (pulse osc1-freq))

        ;; osc 2
        osc2-pitch (select:kr osc2-key-follow [osc2-base-pitch note])
        osc2-freq  (* (midicps osc2-pitch) (pow 2 (/ osc2-fine 1200.0)))
        osc2-freq  (lag:kr osc2-freq osc2-glide)
        osc2-sin   (sin-osc osc2-freq)
        osc2-saw   (saw osc2-freq)
        osc2-tri   (* 7 (one-pole (pulse osc2-freq) 0.99))
        osc2-pulse (pulse osc2-freq osc2-duty)
        osc2       (select osc2-wave [osc2-sin osc2-saw osc2-tri osc2-pulse])

        osc12-mix  (x-fade2 osc1 osc2 (mul-add mix-1-2 2 -1))

        ;;;; wavetable oscillators

        ;; osc 3
        ; for now just adjusting the rate to modify the pitch, but we might
        ; want to use pitch-shift or something else to do this in a better way
        osc3-rate         (/ (* (pow 2 (/ osc3-pitch-offset 12))
                                (pow 2 (/ osc3-fine-offset 1200))
                                WAVETABLE-RATE)
                             WAVETABLE-RATE)
        osc3-rate         (lag:kr osc3-rate osc3-glide)
        osc3              (play-buf 1 osc3-buf osc3-rate osc3-trig osc3-start osc3-loop)

        ;; osc 4
        osc4-rate         (/ (* (pow 2 (/ osc4-pitch-offset 12))
                                (pow 2 (/ osc4-fine-offset 1200))
                                WAVETABLE-RATE)
                             WAVETABLE-RATE)
        osc4-rate         (lag:kr osc4-rate osc4-glide)
        osc4              (play-buf 1 osc4-buf osc4-rate osc4-trig osc4-start osc4-loop)

        ;; feedback, with added delay feature
        feedback-in       (local-in)
        delayed-feedback  (leak-dc (delay-n feedback-in 1.0 feedback-delay))
        feedback          (* feedback-level delayed-feedback)

        osc34-mix         (+ osc3 osc4)
        osc34-pre-filter  (* osc34-mix osc34-pre-filter-level)
        osc34-post-filter (* osc34-mix osc34-post-filter-level)

        all-osc           (+ osc12-mix osc34-pre-filter sub-osc feedback)

        ;; filter section

        ; low-pass filter
        ; TODO: incorporate velocity and lpf-env-velocity-amount so that the velocity
        ; can effect the amount of envelope depth
        lpf-cent-full-offset (* note (* lpf-key-track 1200))
        lpf-pitch-offset     (round (/ lpf-cent-full-offset 1200) 1)
        lpf-cent-offset      (mod lpf-cent-full-offset 1200)
        lpf-key-offset       (* (pow 2 (/ lpf-pitch-offset 12)) lpf-cutoff)
        lpf-key-offset       (* (pow 2 (/ lpf-cent-offset 1200)) lpf-cutoff)
        offset-lpf-cutoff           (+ lpf-cutoff lpf-key-offset)
        lpf-envelope         (env-gen (dapdsr lpf-delay lpf-attack lpf-peak-hold
                                                         lpf-decay lpf-sustain lpf-release
                                                         :curve lpf-env-curve))
        enveloped-lpf-cutoff           (- lpf-cutoff
                                (* lpf-cutoff lpf-env-amount (- 1 lpf-envelope)))
        low-filtered         (rlpf all-osc enveloped-lpf-cutoff lpf-resonance)

        ; need to experiment with different lowpass filters...
        ;lpf (moog-ff mixed lpf-cutoff (* 4 lpf-resonance))

        ; high-pass filter
        hpf-cent-full-offset (* note (* hpf-key-track 1200))
        hpf-pitch-offset     (round (/ hpf-cent-full-offset 1200) 1)
        hpf-cent-offset      (mod hpf-cent-full-offset 1200)
        hpf-key-offset       (* (pow 2 (/ hpf-pitch-offset 12)) hpf-cutoff)
        hpf-cutoff           (- hpf-cutoff hpf-key-offset)
        high-filtered        (hpf low-filtered hpf-cutoff)

        ; mix in osc's 3 and 4 post-filter
        full-snd        (+ high-filtered osc34-post-filter)

        fout            (local-out all-osc)
        ]
    (out 0 (pan2 high-filtered 0.5 0.5))))
        (comment
        ; vca with amplitude envelope
        amp-env (env-gen (dapdsr amp-delay amp-attack amp-peak-hold
                                 amp-decay amp-sustain amp-release
                                 :curve amp-env-curve))

        vca (- vol (* vol amp-env-amount (- 1 amp-env)))
        amped-snd  (* vca full-snd)

        ; panning
        [left-chan right-chan] (pan2 amped-snd pan)

        ; left channel goes through feedback loop
        feedback-out (local-out left-chan)]
  (out 0 [left-chan right-chan])))




; Equation to have envelopes with adjustable amount setting:
; output = input - (input * amount * (1 - envelope))

; A Sound:
; * collection of sample, oscillator, filter, VCA, envelope,
; LFO & modulation settings for a single audio element

; A Beat:
; * all settings to create a drumbeat
; * a note sequence up to 4 measures of 4/4 time
; * up to 32 sounds (16 in each of bank A and B)
; * mixer settings
; * effect settings

; A Project:
; * collection of sounds, beats, play-lists, and settings (tempo)

; Interface Design
; * 2 x 8 sequencer drum pads
; * adjustable swing timing
; * 2 axis (pressure and vertical position) control surfaces
; * modifying synth settings applies to last pad hit

; Pad modes:
; * 16-sounds
; -- load a set of sounds onto the pads (to create beats)
; * 16-beats
; -- load a set of beats onto the pads (to arrange beats)
; * 16 time steps
; -- for step sequence programming
; * 16 mutes
; -- while a beat is playing the pads start all lit
; -- touching a pad toggles whether a given sound will be
;  active or muted
; * 16 tunings
; -- to create bass & melody lines
; -- settings to select scale, octave, transpose
; * 16 levels
; -- each pad plays a single sound at different velocity levels

; Pad option buttons:
; * roll: if roll is on and a pad is held, depending on pressure it will loop at 1/32, 1/16th, etc.
; * fixed level: toggle pad velocity sensitive or not

; Sequencer mod buttons:
; * quantize: delay incoming trigger to next 16th, 1/4, 1/2, or bar
; * swing: modify 16/th note swing 50% => straight 16ths & 66% => triplet swing
; * beat roll: adjustable length stutter effect (loop a portion of sound by adjustable length)

;(def kick  (sample "kick.wav"))
;(def snare (sample "snare.wav"))
;(def hit   (sample "hit.wav"))

(def insts {:kick  kick
            :snare snare
            :hit   c-hat})

(def pat {:kick  [1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0]
          :snare [0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0]
          :hit   [0 0 1 0 0 0 0 0 0 1 0 0 0 1 0 0]})

(def metro (metronome 135))

(defn play-pat [beat i]
  (let [t (mod beat 4)
        p (vec (take 4 (drop (* 4 t) (pat i))))]
    (if (= 1 (p 0)) (at (metro (+ 0.00 beat)) ((insts i))))
    (if (= 1 (p 1)) (at (metro (+ 0.25 beat)) ((insts i))))
    (if (= 1 (p 2)) (at (metro (+ 0.50 beat)) ((insts i))))
    (if (= 1 (p 3)) (at (metro (+ 0.75 beat)) ((insts i))))))

(defn player [beat]
  (doseq [i (keys insts)] (play-pat beat i))
  (apply-at (metro (inc beat)) #'player (inc beat) []))

;(player (metro))

;(metro :bpm 160)
;(metro :bpm 90)

;(defn player [beat] "stop")


