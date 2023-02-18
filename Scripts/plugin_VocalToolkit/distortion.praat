# Hard Clipping formula adapted from a script by David Weenink, "Speech Signal Processing with Praat. 3.6.2.1 Oversteering and clipping",
#   https://www.fon.hum.uva.nl/david/LOT/sspbook.pdf
#
# Soft Clipping formula adapted from "Soft Clipping", https://www.hackaudio.com/digital-signal-processing/distortion-effects/soft-clipping/,
#   and from "Analogue Tape Simulation in MATLAB. 2.3 Tape Saturation Using Arctangent Function", https://hdl.handle.net/2123/22615

form Distortion
	choice Type_of_distortion 1
	button Hard clipping
	button Soft clipping
	comment Hard clipping: the amplitude is limited to a maximum value
	positive Amplitude_threshold_(0-1_Pa) 0.2
	comment Soft clipping: amplitude is saturated along a smooth curve (arctangent function)
	positive Soft_clipping_amount 10
	boolean Retrieve_average_intensity 1
	boolean Preview_(click_Apply._Uncheck_to_publish) 1
endform

include batch.praat

procedure action
	s = selected("Sound")
	s$ = selected$("Sound")
	int = Get intensity (dB)

	if int <> undefined

include preview1.inc

		result = Copy: "tmp"
		runScript: "fixdc.praat"

		if type_of_distortion = 1
			a = amplitude_threshold
			Formula: "if self > " + string$(a) + " then " + string$(a) + " else if self < -" + string$(a) + " then -" + string$(a) + " else self fi fi"
		elsif type_of_distortion = 2
			a = soft_clipping_amount
			Formula: "(2/pi) * arctan(self * " + string$(a) + ")"
		endif

		if retrieve_average_intensity
			Scale intensity: int
		endif

		runScript: "declip.praat"

include preview2.inc

		if not preview
			Rename: s$ + "-distortion_" + type_of_distortion$ + "_" + string$(a)
		endif
	else
		if not preview
			Copy: s$ + "-distortion_" + type_of_distortion$ + "_" + string$(a)
		endif
	endif
endproc
