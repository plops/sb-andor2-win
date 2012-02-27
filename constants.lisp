;;; -*- Lisp -*- - well, that's stretching a point.  code=data != data=code
("C:/Users/martin/Desktop/tmp/sb-andor2-win/ATMCD32D.H")

((:structure andorcaps
 	     ("struct ANDORCAPS"
 	      (unsigned size "ULONG" "ulSize")
	      (unsigned Acq-Modes "ULONG" "ulAcqModes")
	      (unsigned Read-Modes "ULONG" "ulReadModes")
	      (unsigned Trigger-Modes "ULONG" "ulTriggerModes")
	      (unsigned Camera-Type "ULONG" "ulCameraType")
	      (unsigned Pixel-Mode "ULONG" "ulPixelMode")
	      (unsigned Set-Functions "ULONG" "ulSetFunctions")
	      (unsigned Get-Functions "ULONG" "ul3GetFunctions")
	      (unsigned Features "ULONG" "ulFeatures")
	      (unsigned PCI-Card "ULONG" "ulPCICard")
	      (unsigned EMGain-Capability "ULONG" "ulEMGainCapability")
	      (unsigned FT-Read-Modes "ULONG" "ulFTReadModes")
	      ))

 (:function abort-acquisition* ("AbortAcquisition" unsigned-int))
)

(:function ,lname (,name unsigned-int))

